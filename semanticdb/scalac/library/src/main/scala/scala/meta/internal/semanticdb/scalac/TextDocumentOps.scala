package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.SemanticCompat._
import scala.meta.internal.inputs._
import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordering
import scala.reflect.internal._
import scala.reflect.internal.{Flags => gf, util => gu}
import scala.{meta => m}

trait TextDocumentOps {
  self: SemanticdbOps =>

  def validateCompilerState(): Unit = {
    if (!g.settings.Yrangepos.value) sys.error("the compiler instance must have -Yrangepos enabled")
    if (g.useOffsetPositions) sys.error("the compiler instance must use range positions")
    if (!g.settings.plugin.value.exists(_.contains("semanticdb"))) sys
      .error("the compiler instance must use the semanticdb plugin")
    val analyzerClassName = g.analyzer.getClass.getName
    if (!analyzerClassName.contains("HijackAnalyzer")) sys
      .error(s"the compiler instance must use a hijacked analyzer, instead of $analyzerClassName")
    if (g.currentRun.phaseNamed("typer") != NoPhase) {
      if (g.phase.id < g.currentRun.phaseNamed("typer").id) sys
        .error("the compiler phase must be not earlier than typer")
    } else sys.error("the compiler instance does not have a typer phase")
    if (g.currentRun.phaseNamed("patmat") != NoPhase) {
      if (g.phase.id > g.currentRun.phaseNamed("patmat").id) sys
        .error("the compiler phase must be not later than patmat")
    } else {
      // do nothing
    }
  }

  import s.SymbolOccurrence.Role
  type Occurrence = (String, Role)
  def emptyOccurrenceMap() = mutable.Map.empty[m.Position, Occurrence]

  val isOldBehaviourOnPats = BuildInfo.scalaVersion.split("\\.") match {
    case Array("2", "13", patch) if patch == "16" || patch == "15" || patch == "14" => true
    case Array("2", "13", _) => false
    case _ => true
  }

  implicit class XtensionCompilationUnitDocument(unit: g.CompilationUnit) {
    def toTextDocument: s.TextDocument = toTextDocument(None)

    def toTextDocument(explicitDialect: Option[m.Dialect]): s.TextDocument = {
      clearDocumentCaches()
      val occurrences = emptyOccurrenceMap()
      val samoccurrences = emptyOccurrenceMap()
      // SAM methods share positions with their classes.
      val sammethodoccurrences = emptyOccurrenceMap()
      val symbols = mutable.Map[String, s.SymbolInformation]()
      val synthetics = mutable.ListBuffer[s.Synthetic]()
      val syntheticTreeCache = new mutable.HashMap[g.Tree, Option[s.Synthetic]]
      // macro expandees can have cycles, keep tracks of visited nodes.
      val isVisited = mutable.Set.empty[g.Tree]
      val todo = mutable.Set[m.Name]() // names to map to global trees
      val mstarts = mutable.Map[Int, m.Name]() // start offset -> tree
      val mends = mutable.Map[Int, m.Name]() // end offset -> tree
      // start offset of enclosing apply -> its arg names
      val margnames = mutable.Map[Int, List[m.Name]]()
      // name of enclosing member -> name of private/protected within
      val mwithins = mutable.Map[m.Tree, m.Name]()
      val mwithinctors = mutable.Map[m.Tree, m.Name]() // name of enclosing class -> name of private/protected within for primary ctor
      val mctordefs = mutable.Map[Int, m.Name]() // start offset of ctor -> ctor's anonymous name
      val mctorrefs = mutable.Map[Int, m.Name]() // start of new, init or init's type name -> name
      val mparents = mutable.Map[Int, m.Type]() // start offset of init's type -> the type
      val mselftypes = mutable.Map[Int, m.Type]() // start offset -> declared self type
      val mclassofs = mutable.Map[Int, m.Type]() // start offset of classOf -> its type argument
      val msupers = mutable.Map.empty[Int, m.Term.Super] // end offset
      val mfuncs = mutable.Map.empty[Int, m.Position] // start offset of a function
      val mannottees = mutable.Map.empty[Int, m.Member] // start offset of an annotation's init
      val mexpansions = mutable.Map.empty[Int, m.Member] // the same, for a macro annotation

      // Occurrences for names in val patterns, like `val (a, b) =`. Unlike the `occurrences` map, val pattern
      // occurrences uses "last occurrence wins" instead of "first occurrences wins" when disambiguating between
      // multiple symbols that resolve to the same position.
      val mpatoccurrences = emptyOccurrenceMap()
      val mvalpatstart = mutable.Set.empty[Int] // start pos for Pat.Var names inside val patterns
      // start pos for vals with patterns -> last Pat.Var name
      val msinglevalpats = mutable.Map.empty[Int, m.Position]

      def addOccurrenceFromSemantic(mpos: m.Position, ssym: String, role: Role): Unit =
        if (ssym != Symbols.None) occurrences.update(mpos, (ssym, role))
      def addOccurrence(mpos: m.Position, gsym: g.Symbol, role: Role): Unit =
        addOccurrenceFromSemantic(mpos, gsym.toSemantic, role)

      def addSamOccurrence(gt: g.Function) = gt.attachments.get[global.SAMFunction]
        .foreach { samf =>
          val sam = samf.synthCls
          def atPos(mpos: m.Position): Unit = {
            def set(gsym: g.Symbol, map: mutable.Map[m.Position, Occurrence]): Boolean = {
              // SAM symbols aren't reusable and share position with lambda
              val ssym = freshSymbolRaw(gsym)
              val ok = ssym ne Symbols.None
              if (ok) {
                map.update(mpos, (ssym, Role.DEFINITION))
                saveSymbolFromSemantic(gsym, ssym)
              }
              ok
            }
            set(sam, samoccurrences) && set(sam.info.decls.elems.sym, sammethodoccurrences)
          }
          val gsym = gt.symbol
          val gpos = gt.pos
          if (gpos.isDefined && (gsym ne null) && !config.symbols.isNone) {
            val gstart = gpos.start
            mfuncs.get(gstart).fold(mstarts.get(gstart).foreach { name =>
              val mpos = name.pos
              if (mpos.end == gpos.end) atPos(mpos)
            })(atPos)
          }
        }

      def addPatOccurrenceFromSemantic(mpos: m.Position, ssym: String): Unit =
        if (ssym != Symbols.None) mpatoccurrences.update(mpos, (ssym, Role.DEFINITION))
      def addPatOccurrence(mpos: m.Position, gsym: g.Symbol): Unit =
        addPatOccurrenceFromSemantic(mpos, gsym.toSemantic)

      def saveSymbol(gs: g.Symbol): Unit = if (gs.isUsefulSymbolInformation) saveSymbolDo(gs)
      def saveSymbolDo(gs: g.Symbol): Unit = saveSymbolFromSemantic(gs, gs.toSemantic)
      def saveSymbolFromSemantic(gs: g.Symbol, ssym: String): s.SymbolInformation = {
        val info = gs.toSymbolInfo(SymlinkChildren, ssym)
        symbols(ssym) = info
        info
      }
      def shouldNotSaveSymbol(gsym: g.Symbol): Boolean = config.symbols.isNone || gsym == null ||
        gsym.hasPackageFlag || gsym.isUselessSymbolInformation
      def shouldNotSaveSemanticSymbol(ssym: String): Boolean = ssym == Symbols.None ||
        config.symbols.isLocalOnly && !ssym.isLocal

      locally {
        object traverser extends m.Traverser {
          private def indexName(mname: m.Name): Unit = {
            val mpos = mname.pos
            if (mpos.isEmpty) return // for instance, missing type in method declaration
            todo += mname
            val range = mname.tokens.findNot(_.is[m.Token.LeftParen]).getOrElse(mpos)
            mstarts.put(range.start, mname).foreach(errorAmbiguous("mStart", mname, _))
            mends.put(range.end, mname).foreach(errorAmbiguous("mEnd", mname, _))
          }
          private def setArgNames(pos: => Iterable[Int])(terms: m.Term.ArgClause*): Unit = {
            val names = List.newBuilder[m.Name]
            terms.foreach(_.values.foreach {
              case m.Term.Assign(n: m.Term.Name, _) => names += n
              case _ =>
            })
            names.result() match {
              case Nil =>
              case names => pos.foreach(margnames.update(_, names))
            }
          }
          private def indexWithin(mname: m.Name.Indeterminate): Unit = {
            todo += mname
            def setWithins(mencl: m.Tree): Unit = mwithins.put(mencl, mname)
              .foreach(errorAmbiguous("mWithins", mname, _))
            mname.parent.parent.get match {
              case mencl: m.Ctor.Primary => mencl.parent match {
                  case Some(p: m.Member) => mwithinctors.put(p.name, mname)
                      .foreach(errorAmbiguous("mWithinCtors", mname, _))
                  case _ =>
                }
              case mencl: m.Member => setWithins(mencl.name)
              case m.Tree.WithPats(p :: Nil) => p.traverse { case m.Pat.Var(n) => setWithins(n) }
              case _ =>
            }
          }
          // scalac puts the constructor reference of `n.B(1)` at `B`
          @tailrec
          private def typeNameStart(mtpe: m.Type): Int = mtpe match {
            case t: m.Type.Apply => typeNameStart(t.tpe)
            case t: m.Type.Select => t.name.pos.start
            case t: m.Type.Project => t.name.pos.start
            case t => t.pos.start
          }
          def indexPats(pats: List[m.Pat]): Unit = pats.foreach(_.traverse {
            case pat: m.Pat.Extract => indexPatsWithExtract(pat)
            case pat: m.Pat.Var => mvalpatstart += pat.name.pos.start
          })
          // In an Extract pattern `Foo(name) = ...`, let's map the start position of the `fun` field
          // to the `name` position. Compiler desugars it into a getter DefDef and specifically
          // in the case of a single binder sets the position of this getter as an OffsetPosition
          // pointing to "start of fun" rather than the field being extracted.
          def indexPatsWithExtract(extract: m.Pat.Extract): Unit = extract.args match {
            case pat :: Nil =>
              // Most likely the difference coming from https://github.com/scalameta/scalameta/commit/27cb4c43dbb1485fc6f43b71dc58435393556390
              val mpos = if (isOldBehaviourOnPats) extract.fun.pos.end else extract.fun.pos.start
              pat.traverse {
                case pat: m.Pat.Extract => indexPatsWithExtract(pat)
                case pat: m.Pat.Var =>
                  val npos = pat.name.pos
                  mvalpatstart += npos.start
                  msinglevalpats(mpos) = npos
              }
            case pats => indexPats(pats)
          }
          override def apply(mtree: m.Tree): Unit = {
            mtree match {
              case mtree: m.Term.Apply => setArgNames(mtree.fun.pos.end :: Nil)(mtree.argClause)
              case m.Mod.WithWithin(mname: m.Name.Indeterminate) => indexWithin(mname)
              // NOTE: ignore mrename for now, we may decide to make it a binder
              case m.Importee.Rename(mname, _) =>
                indexName(mname)
                return
              case mtree: m.Term.Super => msupers.update(mtree.pos.end, mtree)
              case m.Self(_, Some(mtpe)) => mselftypes(mtpe.pos.start) = mtpe
              case m.Term.ApplyType(m.Term.Name("classOf"), mtpe :: Nil) =>
                mclassofs(mtree.pos.start) = mtpe
              case mtree: m.Mod.Annot => mtree.parent.foreach {
                  case p: m.Member => mannottees(mtree.init.pos.start) = p
                  case _ =>
                }
              case mtree: m.Ctor => mctordefs(mtree.pos.start) = mtree.name
              case mtree: m.Term.New => mctorrefs(mtree.pos.start) = mtree.init.name
              case mtree: m.Init =>
                mctorrefs(mtree.pos.start) = mtree.name
                mctorrefs(typeNameStart(mtree.tpe)) = mtree.name
                mparents(mtree.tpe.pos.start) = mtree.tpe
                def parentOpt = mtree.parent.collect { case p: m.Term.New => p }
                setArgNames((mtree +: parentOpt.toList).map(_.pos.start))(mtree.argClauses: _*)
              case _: m.Name.Anonymous | _: m.Name.Placeholder | _: m.Name.This =>
              case mtree: m.Name => indexName(mtree)
              case mtree @ (_: m.Term.FunctionLike | _: m.Term.AnonymousFunction) =>
                val mpos = mtree.pos
                mfuncs.update(mpos.start, mpos)
              case mtree: m.Tree.WithPats => mtree.pats match {
                  case (_: m.Pat.Var) :: Nil =>
                  case pats => indexPats(pats)
                }
              case _ =>
            }
            super.apply(mtree)
          }
        }

        traverser(unit.toSource(explicitDialect))
      }

      locally {
        object traverser extends g.Traverser {
          private var genclosing: List[g.Tree] = Nil // the current tree and its parents
          private lazy val grootImports = g.analyzer.rootContext(unit).imports.map(_.tree)

          private def trySymbolDefinition(gsym: g.Symbol): Unit = {
            if (shouldNotSaveSymbol(gsym)) return
            val symbol = gsym.toSemantic
            if (shouldNotSaveSemanticSymbol(symbol)) return
            val symInfo = saveSymbolFromSemantic(gsym, symbol)

            if (gsym.isClass && !gsym.isTrait) {
              if (gsym.isAnonymousClass && gsym.pos.isDefined)
                addOccurrenceFromSemantic(gsym.pos.focus.toMeta, symbol, Role.DEFINITION)
              val gprim = gsym.primaryConstructor
              saveSymbol(gprim)
              gprim.info.paramss.flatten.foreach(saveSymbol)
            }
            if (gsym.isGetter) {
              val gsetter = gsym.setterIn(gsym.owner)
              saveSymbol(gsetter)
              gsetter.info.paramss.flatten.foreach(saveSymbol)
            }
            if (gsym.isUsefulField && gsym.isMutable) {
              val setterInfos = Synthetics.setterInfos(symInfo, SymlinkChildren)
              setterInfos.foreach(info => symbols(info.symbol) = info)
            }
          }
          // A `val`/`var` parameter of a primary constructor (and any plain parameter that scalac
          // turns into a field) gives rise to more than one symbol: the accessor (the getter, or the
          // underlying field) and the constructor PARAMETER symbol. References resolve to the
          // accessor (see the #1538 FIXME below), but per
          // https://github.com/scalameta/scalameta/issues/1327 the definition site covers both, so
          // we tuck the constructor parameter into a multi-symbol that `finalOccurrences` expands
          // into a second occurrence sharing the parameter's range.
          // Scalac keeps no symbol-level link from a param accessor back to the constructor
          // parameter it stems from, so pairing them by name is the available relation — the same
          // one the compiler itself uses, and the same one scalacp's SymbolInformationOps uses to
          // go in the opposite direction.
          private def withCtorParam(gsym: g.Symbol, ssym: String): String =
            if (!gsym.isParamAccessor) ssym
            else gsym.owner.primaryConstructor.info.paramss.flatten
              .find(_.symbolName == gsym.symbolName)
              .fold(ssym)(gparam => Symbols.Multi(List(ssym, gparam.toSemantic)))
          private def success(mtree: Option[m.Name], gsym0: => g.Symbol): Unit = mtree
            .foreach(success(_, gsym0))
          private def success(mtree: m.Name, gsym0: g.Symbol): Unit = {
            // We cannot be guaranteed that all symbols have a position, see
            // https://github.com/scalameta/scalameta/issues/665
            // Instead of crashing with "unsupported file", we ignore these cases.
            if (gsym0 == null) return
            if (gsym0.isUselessOccurrence) return
            if (gsym0.isImplicitPrimitiveConversion(mtree)) return
            val pos = mtree.parent match {
              // without arguments, the name of an init is at the next token, past any comment
              case Some(p: m.Init) if (p.name eq mtree) && p.argClauses.isEmpty =>
                p.pos.input.pos(p.pos.end)
              case _ => mtree.pos
            }
            if (pos == m.Position.None) return
            if (occurrences.contains(pos)) return

            val gsym =
              if (gsym0.isConstructor && !mtree.isAny[m.Name.Anonymous, m.Name.This]) gsym0.owner
              else gsym0

            val register: String => Unit =
              if (mtree.isDefinition)
                if (!mvalpatstart.contains(pos.start))
                  addOccurrenceFromSemantic(pos, _, Role.DEFINITION)
                else if (gsym.name.endsWith(mtree.value)) addPatOccurrenceFromSemantic(pos, _)
                else null
              else if (gsym.pos.source == unit.source || !gsym.owner.isRefinementClass)
                // We don't emit occurrences for accesses on structural types
                // because is no canonical definition of a structural types,
                // the same structure can be re-declared in various locations.
                // However, in the special case where the structural type is defined
                // within the same file we use the local symbol that's generated by scalac.
                // We can't use the local symbol if `gsym.pos.source != unit.source` because
                // that would mean we reference local symbols across files.
                addOccurrenceFromSemantic(pos, _, Role.REFERENCE)
              else null
            if (register eq null) return
            val symbol = gsym.toSemantic
            if (symbol == Symbols.None) return

            todo -= mtree
            register(if (mtree.isDefinition) withCtorParam(gsym, symbol) else symbol)

            def tryWithin(map: mutable.Map[m.Tree, m.Name], gsym0: g.Symbol): Unit = map.get(mtree)
              .foreach { mname =>
                val gsym = gsym0.getterIn(gsym0.owner).orElse(gsym0)
                if (!gsym.hasAccessBoundary) return
                val within1 = gsym.privateWithin
                val within2 = within1.owner.info.member {
                  val wname = within1.name
                  if (wname.isTermName) wname.toTypeName else wname.toTermName
                }
                success(mname, wrapAlternatives("<within " + symbol + ">", within1, within2))
              }
            tryWithin(mwithins, gsym)
            tryWithin(mwithinctors, gsym.primaryConstructor)
          }
          private def tryNamedArg(gsym: g.Symbol, offsets: Int*): Unit =
            if (gsym != null && (gsym.isMethod || gsym.isConstructor)) for {
              offset <- offsets
              margnames <- margnames.get(offset)
              margname <- margnames
              gparams <- gsym.paramss
              gparam <- gparams.find(_.name.decoded == margname.value)
            } success(margname, gparam)
          private def tryFindMtree(gtree: g.Tree): Unit = {
            val gpos = gtree.pos
            if (gpos == null || !gpos.isDefined) return
            val gsym = gtree.symbol
            val gstart = gpos.start
            val gpoint = gpos.point
            val gend = gpos.end

            def tryMstart(pos: Int): Unit = success(mstarts.get(pos), gsym)
            def tryMend(pos: Int): Unit = success(mends.get(pos), gsym)

            gtree match {
              case _: g.DefTree => trySymbolDefinition(gsym)
              case _ =>
            }

            // in a macro expansion, find only the annottee and its companion
            if (!gpos.isRange) mexpansions.get(gpoint) match {
              case Some(mannottee) =>
                if (gtree.isInstanceOf[g.ImplDef]) mannottee.parent.foreach { p =>
                  val gname = gsym.decodedName
                  p.foreachChild {
                    case t: m.Member
                        if t.name.value == gname &&
                          t.name.isInstanceOf[m.Term.Name] == gsym.isTerm =>
                      success(t.name, gsym)
                      t match { // 2.12 puts the primary constructor past the class
                        case t: m.Defn.Class => success(t.ctor.name, gsym.primaryConstructor)
                        case _ =>
                      }
                    case _ =>
                  }
                }
                return
              case _ =>
            }

            tryNamedArg(gsym, gstart, gpoint)

            (gtree match {
              case gtree: g.Template => gtree.body.iterator.map(_.symbol)
                  .find(x => x != null && x.isPrimaryConstructor)
              case _: g.DefDef if gsym.isConstructor => Some(gsym)
              case _ => None
            }).foreach(success(mctordefs.get(gstart), _))

            gtree match {
              case g.Select(_, g.nme.CONSTRUCTOR) => success(mctorrefs.get(gpoint), gsym)
              case _ =>
            }

            // Ideally, we'd like a perfect match when gtree.pos == mtree.pos.
            // Unfortunately, this is often not the case as demonstrated by a bunch of cases above and below.
            val gstartMtree = mstarts.get(gstart)
            gstartMtree match {
              case Some(mtree) if mtree.pos.end == gend =>
                success(mtree, gsym)
                return
              case _ =>
            }

            def singleValPat(): Unit = msinglevalpats.get(gpoint) match {
              case Some(mpos) => addPatOccurrence(mpos, gsym)
              case _ => tryMstart(gpoint)
            }

            gtree match {
              case _: g.ValDef if gsym.isSelfParameter => tryMstart(gstart)
              case _: g.MemberDef if gsym.hasFlag(Flags.SYNTHETIC | Flags.ARTIFACT) =>
                if (!gsym.isSemanticdbLocal && !gsym.isUseless) saveSymbolDo(gsym)
              case _: g.PackageDef => // NOTE: capture PackageDef.pid instead
              case gtree: g.ModuleDef if gtree.name == g.nme.PACKAGE =>
                // NOTE: if a package object comes first in the compilation unit
                // then its positions are completely mental, so we just hack around
                tryMstart(gpoint + 7)
                tryMstart(gpoint)
              case _: g.DefDef if gsym.isGetter => singleValPat()
              case _: g.ValDef => if (gsym.isMethod || gsym.getterIn(gsym.owner) == g.NoSymbol) {
                  // FIXME: https://github.com/scalameta/scalameta/issues/1538
                  // Skip the field definition in favor of the associated getter.
                  // This will make sure that val/var class parameters are consistently
                  // resolved to getter symbols both as definition and references.
                  tryMstart(gstart)
                  singleValPat()
                }
              // mctordefs found it above; its point can be on a parent's name
              case _: g.DefDef if gsym.isConstructor =>
              case _: g.DefTree => tryMstart(gpoint)
              case _: g.This => mstarts.get(gpoint)
                  .foreach(name => if (gsym.nameString == name.value) success(name, gsym))
              case t: g.Super => msupers.get(gend).foreach { mtree =>
                  success(mtree.thisp, gsym)
                  // now find the super parent
                  val parent = t.mix
                  val parents = gsym.asClass.info.parents.map(_.typeSymbol)
                  if (parent.nonEmpty) parents.find(_.name == parent)
                    .foreach(success(mtree.superp, _))
                }
              // an unqualified super stands for the parent that has the selected member
              case g.Select(t: g.Super, _) if t.mix.isEmpty =>
                tryMstart(gpoint)
                if (gsym != null && gsym.exists) msupers.get(t.pos.end).foreach { mtree =>
                  val owner = gsym.owner
                  val parents = t.symbol.asClass.info.parents.map(_.typeSymbol)
                  parents.find(_ == owner).orElse(parents.find(_.isSubClass(owner)))
                    .foreach(success(mtree.superp, _))
                }
              case gtree: g.Select if gtree.symbol == g.definitions.NilModule =>
                // NOTE: List() gets desugared into mkAttributedRef(NilModule)
                tryMstart(gstart)
              case g.Select(g.Ident(qualTerm), _) if qualTerm.startsWith(g.nme.QUAL_PREFIX) =>
                tryMend(gend) // transformNamedApplication blockWithQualifier case
              case _: g.RefTree => if (!gsym.isDefaultGetter) tryMstart(gpoint)
              case gtree: g.Import if gtree.expr != null && gtree.expr.tpe != null =>
                val sels = gtree.selectors.flatMap(sel =>
                  if (sel.name == g.nme.WILDCARD) Nil
                  else mstarts.get(sel.namePos).map(mname => (sel.name, mname)),
                )
                sels.foreach { case (gname, mname) =>
                  val gexpr = gtree.expr
                  val gtpe = gexpr.tpe
                  val import1 = gtpe.member(gname.toTermName)
                  val import2 = gtpe.member(gname.toTypeName)
                  val altName = "<import " + gexpr + "." + gname + ">"
                  success(mname, wrapAlternatives(altName, import1, import2))
                }
              case gtree: g.AppliedTypeTree
                  if gtree.symbol.name == g.typeNames.REPEATED_PARAM_CLASS_NAME =>
                gtree.args.headOption.foreach(x => success(gstartMtree, x.symbol))
              case _ =>
            }
          }

          @tailrec
          private def tryFindQualifier(mqual: m.Term.Ref, gpre: g.Type): Unit =
            (mqual, gpre) match {
              case (m.Term.Select(mq: m.Term.Ref, mname), g.SingleType(gp, gsym)) =>
                success(mname, gsym)
                tryFindQualifier(mq, gp)
              case (m.Term.Select(mq: m.Term.Ref, mname), g.ThisType(gsym)) =>
                success(mname, gsym)
                tryFindQualifier(mq, gsym.owner.thisType)
              case (mname: m.Term.Name, g.SingleType(_, gsym)) => success(mname, gsym)
              case (mname: m.Term.Name, g.ThisType(gsym)) => success(mname, gsym)
              case _ =>
            }

          /* scalac dealiased the parent, so its prefix may not be the qualifier: resolve the
           * qualifier from the scope around the class instead, and keep it if its member of
           * that name leads to the same class */
          private def tryFindParentQualifier(mqual: m.Term.Ref, mname: m.Name, gtpe: g.Type): Unit = {
            val gscope = genclosing.dropWhile(!_.isInstanceOf[g.ImplDef]).drop(1)
            resolvePath(mqual, gscope, mqual.pos.start).foreach { case (gpre, gquals) =>
              val gmember = gpre.member(g.TypeName(mname.value).encode)
              @tailrec
              def dealiased(gtpe: g.Type): g.Symbol = gtpe match {
                case g.PolyType(_, gres) => dealiased(gres)
                case _ => gtpe.dealias.typeSymbol
              }
              val gcls = if (gmember.isClass) gmember else dealiased(gpre.memberInfo(gmember))
              if (gmember.exists && gcls == gtpe.typeSymbol) gquals.foreach { case (mq, gq) =>
                success(mq, gq)
              }
            }
          }
          // the type of a stable path and the symbols of its names, if scalac would accept it
          private def resolvePath(
              mqual: m.Term.Ref,
              gscope: List[g.Tree],
              start: Int,
          ): Option[(g.Type, List[(m.Name, g.Symbol)])] = mqual match {
            case m.Term.Select(mq: m.Term.Ref, mname) => resolvePath(mq, gscope, start).flatMap {
                case (gpre, gquals) => stablePath(gpre.member(g.TermName(mname.value).encode))
                    .map { case (gtpe, gsym) => (gtpe, (mname -> gsym) :: gquals) }
              }
            case mname: m.Term.Name if mname.value == "_root_" =>
              Some((g.rootMirror.RootClass.tpe, (mname -> g.rootMirror.RootPackage) :: Nil))
            case mname: m.Term.Name => lookupName(g.TermName(mname.value).encode, gscope, start)
                .flatMap(stablePath).map { case (gtpe, gsym) => (gtpe, (mname -> gsym) :: Nil) }
            case m.Term.This(mq) => enclosingClass(mq, gscope).map(gcls => (gcls.thisType, Nil))
            case m.Term.Super(mthis, msuper) => enclosingClass(mthis, gscope).flatMap { gcls =>
                val gparents = gcls.info.parents
                if (msuper.value.isEmpty) Some((g.intersectionType(gparents), Nil))
                else gparents.find(_.typeSymbol.decodedName == msuper.value).map((_, Nil))
              }
            case _ => None
          }
          // the class that `this` or `super` refers to, with an optional class name
          private def enclosingClass(mname: m.Name, gscope: List[g.Tree]): Option[g.Symbol] = gscope
            .collectFirst {
              case t: g.Template
                  if mname.value.isEmpty || t.symbol.owner.decodedName == mname.value =>
                t.symbol.owner
            }
          private def stablePath(gsym: g.Symbol): Option[(g.Type, g.Symbol)] =
            if (gsym.exists && gsym.isTerm && !gsym.isOverloaded && gsym.isStable)
              Some((gsym.info.finalResultType, gsym))
            else None
          /* the symbol of a name in scope, innermost binding first, or None if unsure;
           * for a compiled tree, a binding never conflicts with one further out */
          private def lookupName(gname: g.TermName, gscope: List[g.Tree], start: Int) = {
            val gbindings = gscope.iterator.map(scopeBinding(gname, _, start)) ++
              grootImports.iterator.map(importBinding(gname, _)) ++
              Iterator(Some(g.rootMirror.RootClass.info.member(gname)))
            gbindings.find(_.forall(_.exists)).flatten
          }
          // the symbol that a scope binds to a name: NoSymbol if none, None if unsure
          private def scopeBinding(gname: g.TermName, gtree: g.Tree, start: Int): Option[g.Symbol] = {
            def orElse(gsym: Option[g.Symbol])(other: => Option[g.Symbol]) =
              if (gsym.contains(g.NoSymbol)) other else gsym
            def imports(gstats: List[g.Tree]) = gstats.reverseIterator.collect {
              case t: g.Import if t.pos.isDefined && t.pos.start < start => importBinding(gname, t)
            }.find(_.forall(_.exists)).getOrElse(Some(g.NoSymbol))
            def defined(gtrees: List[g.Tree]) = Some(
              gtrees.collectFirst {
                case t: g.DefTree if t.name == gname && t.symbol != null => t.symbol
              }.getOrElse(g.NoSymbol),
            )
            gtree match {
              case t: g.Block => orElse(imports(t.stats))(defined(t.stats))
              case t: g.Template => orElse(imports(t.body))(defined(t.self :: Nil)) match {
                  case Some(g.NoSymbol) => Some(t.symbol.owner.thisType.member(gname))
                  case gsym => gsym
                }
              case t: g.PackageDef => orElse(imports(t.stats)) {
                  val gpkg = t.pid.symbol.moduleClass
                  Some(gpkg.info.member(gname).orElse(gpkg.packageObject.info.member(gname)))
                }
              case t: g.DefDef => defined(t.vparamss.flatten)
              case t: g.Function => defined(t.vparams)
              case t: g.CaseDef => defined(t.pat.collect { case b: g.Bind => b })
              case _ => Some(g.NoSymbol)
            }
          }
          // the symbol that an import binds to a name: NoSymbol if none, None if unsure
          private def importBinding(gname: g.TermName, gimport: g.Import): Option[g.Symbol] = {
            val gtpe = gimport.expr.tpe
            if (gtpe == null) None
            else gimport.selectors.find(_.rename == gname) match {
              case Some(sel) => Some(gtpe.member(sel.name.toTermName))
              case _ =>
                val isWildcard = gimport.selectors.exists(_.name == g.nme.WILDCARD)
                // a selector that renames the name hides it from the wildcard
                if (isWildcard && !gimport.selectors.exists(_.name == gname)) Some(gtpe.member(gname))
                else Some(g.NoSymbol)
            }
          }

          // match the names in a type that scalac left without symbols to its type
          private def tryFindType(mtpe: m.Type, gtpe: g.Type): Unit = (mtpe, gtpe) match {
            case (m.Type.Apply(mfun, margs), g.TypeRef(_, _, gargs)) =>
              tryFindType(mfun, gtpe)
              tryFindTypes(margs, gargs)
            case (mname: m.Type.Name, g.TypeRef(_, gsym, _)) => success(mname, gsym)
            case (m.Type.Select(mqual, mname), g.TypeRef(gpre, gsym, _)) =>
              success(mname, gsym)
              tryFindQualifier(mqual, gpre)
            case (m.Type.Project(mqual, mname), g.TypeRef(gpre, gsym, _)) =>
              success(mname, gsym)
              tryFindType(mqual, gpre)
            case (mtpe: m.Type.Tuple, g.TypeRef(_, _, gargs)) => tryFindTypes(mtpe.args, gargs)
            case (mtpe: m.Type.Function, g.TypeRef(_, _, gargs)) =>
              tryFindTypes(mtpe.paramClause.values :+ mtpe.res, gargs)
            case (_, g.ExistentialType(_, gtpe)) => tryFindType(mtpe, gtpe)
            case _ =>
          }
          private def tryFindTypes(mtpes: List[m.Type], gtpes: List[g.Type]): Unit = mtpes.zip(gtpes)
            .foreach { case (mtpe, gtpe) => tryFindType(mtpe, gtpe) }

          private def processMemberDef(gtree: g.MemberDef) = {
            val gsym = gtree.symbol
            // a macro annotation puts its expansion at the offset of the annotation
            if (gtree.isInstanceOf[g.ImplDef]) {
              val gpos = gtree.pos
              if (gpos.isDefined && !gpos.isRange) mannottees.get(gpos.point)
                .foreach(mexpansions(gpos.point) = _)
            }
            if (gsym != null) gsym.annotations.foreach(ann => traverse(ann.original))
            tryFindMtree(gtree)
          }

          override def traverse(gtree: g.Tree): Unit = if (isVisited.add(gtree)) {
            genclosing = gtree :: genclosing
            gtree.attachments.all.foreach {
              case att: g.analyzer.MacroExpansionAttachment => traverse(att.expandee)
              case _ =>
            }

            def isClassOf(pos: m.Position): Boolean = {
              val chars = pos.input.chars
              val classOfChars = "classOf".toCharArray

              chars.length >= classOfChars.length + pos.start && (0 until classOfChars.length)
                .forall(i => chars(i + pos.start) == classOfChars(i))
            }

            gtree match {
              case OriginalTreeOf(original) => traverse(original)
              case CompoundTypeTreeOf(original) => traverse(original)

              case gtree: g.TypeTree if gtree.original != null =>
                traverse(gtree.original)
                // annotations in the original are untyped, like `@field` in `@(deprecated @field)`
                gtree.tpe match {
                  case g.AnnotatedType(anns, _) => anns.foreach { ann =>
                      val gpos = ann.original.pos
                      if (gpos.isRange && gpos.source == unit.source) traverse(ann.original)
                    }
                  case _ =>
                }
              case gtree: g.TypeTreeWithDeferredRefCheck => traverse(gtree.check())

              case t: g.Literal if t.tpe != null && t.tpe.typeSymbol == g.definitions.ClassClass =>
                val mpos = t.toMeta
                if (!mpos.isEmpty && isClassOf(mpos)) {
                  val mposClassOf = mpos.trunc("classOf".length)
                  addOccurrenceFromSemantic(mposClassOf, "scala/Predef.classOf().", Role.REFERENCE)
                  // scalac replaces `classOf[T]` with a constant of type T
                  mclassofs.get(mpos.start).foreach(tryFindType(_, t.value.typeValue))
                } else tryFindMtree(t)

              case t: g.Function =>
                addSamOccurrence(t)
                t.vparams.foreach(p =>
                  if (!(p.symbol.isSynthetic || p.name.decoded.startsWith("x$"))) traverse(p),
                )
                traverse(t.body)
                tryFindMtree(t)
              case t: g.ValDef =>
                if (t.hasSyntheticSymbol) traverse(t.tpt)
                traverse(t.rhs)
                processMemberDef(t)
              case t: g.MemberDef => processMemberDef(t)
              case t: g.Ident => t.pos match {
                  case p: gu.RangePosition if p.start >= p.end => // do nothing, empty identifier (such as empty package)
                  case _ => tryFindMtree(t)
                }
              case _: g.GenericApply =>
                tryFindSynthetic(gtree)
                gtree.pos match {
                  case p: gu.RangePosition => tryNamedArg(gtree.symbol, p.start, p.point)
                  case _ =>
                }
                // fix #2040
                gtree match {
                  case v: g.ApplyImplicitView => v.args.headOption.foreach(traverse)
                  case _ =>
                }
              case gtree: g.Select if gtree.symbol != null && gtree.symbol.isDefaultGetter =>
                // the qualifier of a default argument may have its offset at another name
                if (!gtree.qualifier.pos.isRange) isVisited += gtree.qualifier
                tryFindMtree(gtree)
              case select: g.Select if isSyntheticName(select) => tryFindSynthetic(select)
              case gtree: g.AppliedTypeTree => tryFindMtree(gtree)
              case gblock @ NamedApplyBlock(_) =>
                // Given the result of NamesDefaults.transformNamedApplication, such as:
                //    BLOCK: {
                //      <artifact> val qual$1: helper = test.this.qualifier
                //      <artifact> val x$1: Int = ao.this.local;
                //      <artifact> val x$2: Int = 3;
                //      <artifact> val x$3: Int = ao.this.foo$default$2;
                //      qual1$.foo(x$1, x$3, x$2)
                //    }
                //
                // Three issues:
                // 1 - In the case of an explicitly passed argument (ex: x$1 above), the generated ValDef and its RHS
                //     both have a Position identical to the original argument. We want to avoid matching the original
                //     argument with this ValDef, which has its own Symbol, and ensure we match it to the RHS.
                // 2 - In the case of a default-valued argument (ex: x$3 above), the generated ValDef, and several of
                //     its RHS sub-trees, have Positions starting and ending at the start of the method name in the
                //     original source. We want to avoid matching any of these w/ our "tryMstart" implementations.
                // 3 - In the case of a non-stable qualifier reference, a qual$ ValDef is generated. In these cases
                //     the Select(qual$1, foo) has a position which corresponds to `.foo` in the originating source
                //     (the period is captured).
                //
                // Current Solutions:
                // For case #1, explicitly traverse the ValDef RHS, to claim the proper association before general
                // recursion continues.
                // For case #2, explicitly traverse the function invocation, to claim the proper association before
                // recursion continues.
                // For case #3, we do not change traversal order, but have added an additional end-only match case
                // for Select's against a qual$ Ident.
                gblock.stats.foreach {
                  case g.ValDef(_, _, _, rhs) =>
                    if (rhs.symbol == null || !rhs.symbol.isDefaultGetter) traverse(rhs)
                  case _ =>
                }
                traverse(gblock.expr)
              case t: g.Template =>
                // context bounds could be moved into body
                t.body.foreach {
                  case s: g.ValDef =>
                    if (s.symbol.hasAllFlags(Flags.IMPLICIT | Flags.SYNTHETIC)) traverse(s.tpt)
                  case _ =>
                }
                t.parents.foreach {
                  // a parent with inferred type arguments, like `n.P(1)`, has no original tree
                  case p: g.TypeTree if p.original == null && p.pos.isRange =>
                    mparents.get(p.pos.start).foreach {
                      case m.Type.Select(mqual, mname) =>
                        success(mname, p.tpe.typeSymbol)
                        tryFindParentQualifier(mqual, mname, p.tpe)
                      case mtpe => tryFindType(mtpe, p.tpe)
                    }
                  case _ =>
                }
                t.self.tpt match {
                  case tpt: g.TypeTree if tpt.original != null =>
                    // scalac types `this: T =>` in class C as `C with T`
                    val gtpe = tpt.tpe match {
                      case g.RefinedType(gcls :: gtpe :: Nil, _)
                          if gcls.typeSymbol == t.symbol.owner => gtpe
                      case gtpe => gtpe
                    }
                    mselftypes.get(tpt.original.pos.start).foreach(tryFindType(_, gtpe))
                  case _ =>
                }
                tryFindMtree(t)
              case _ => tryFindMtree(gtree)
            }
            super.traverse(gtree)
            genclosing = genclosing.tail
          }
        }
        traverser.traverse(unit.body)
      }

      object Synth {
        def cached(gt: g.Tree)(res: => Option[s.Synthetic]): Option[s.Synthetic] =
          syntheticTreeCache.getOrElseUpdate(
            gt,
            // if v is None, no synthetic; if v has no range, cache for parents but don't output
            {
              val v = res
              v.foreach(s => if (s.range.isDefined) synthetics += s)
              v
            },
          )

        def getCached(gt: g.Tree): Option[s.Synthetic] = cached(gt) {
          gt match {
            case t: g.ApplyImplicitView =>
              val pos = t.toRange
              syn(pos, getApplyImplicitView(t, pos))
            case t: g.ApplyToImplicitArgs =>
              val pos = t.toRange
              t.fun match {
                case f: g.ApplyImplicitView => cached(f)(syn(getApplyImplicitView(f, pos)))
                    .flatMap(x => syn(pos, getApplyToImplicitArgs(t, x.tree)))
                case _ => syn(pos, getApplyToImplicitArgs(t))
              }
            case t: g.TypeApply => getTypeApply(t, isWithinFor = false)
            case t: g.Apply =>
              val res = cachedIfSelect(t.fun)
              if (res.isEmpty) forApply(t) else None
            case t: g.Select => getSelect(t)
            case _ => None
          }
        }

        def syn(pos: Option[s.Range], st: s.Tree): Some[s.Synthetic] = Some(s.Synthetic(pos, st))
        def syn(st: s.Tree): Some[s.Synthetic] = syn(None, st)
        def syn(pos: s.Range, st: s.Tree): Some[s.Synthetic] = syn(Some(pos), st)
        def syn(pos: => s.Range, st: s.Tree, usePos: Boolean): Option[s.Synthetic] =
          if (usePos) syn(pos, st) else syn(st)

        private def getSelect(gt: g.Select, usePos: Boolean = true): Option[s.Synthetic] = {
          val qual = gt.qualifier
          if (gt.pos != qual.pos) None
          else {
            val pos = qual.toRange
            def res(qual: s.Tree) = syn(pos, s.SelectTree(qual, Some(gt.toSemanticId)), usePos)
            syntheticName(gt.name)(res(cachedForOrOrig(qual)))(res(pos.toSemanticOriginal))(None)
          }
        }

        private def cachedIfSelect(gt: g.Tree, usePos: Boolean = true) = gt match {
          case t: g.Select => cached(t)(getSelect(t, usePos = usePos))
          case _ => None
        }

        private def getTypeApply(gt: g.TypeApply, isWithinFor: Boolean): Option[s.Synthetic] = {
          def res(fun: s.Tree) = s.TypeApplyTree(fun, gt.args.map(_.tpe.toSemanticTpe))
          (if (isWithinFor) cachedForApplyFun(gt.fun) else None) match {
            case Some(fun) => syn(res(fun.tree))
            case _ if gt.args.headOption.exists(!_.pos.isRange) =>
              val pos = gt.fun.toRange
              def noApply = gt.fun match {
                case f: g.Apply =>
                  cached(f)(None)
                  f.fun
                case f => f
              }
              def fun = cachedIfSelect(noApply, false).fold[s.Tree](pos.toSemanticOriginal)(_.tree)
              syn(pos, res(fun))
            case _ => None
          }
        }

        private def getApplyImplicitView(gt: g.ApplyImplicitView, range: s.Range) = {
          val fun = gt.fun match {
            // a method of a value, like `f.apply`, does not identify the conversion by itself
            case f @ g.Select(qual, _)
                if qual.symbol != null && !qual.symbol.isModule && !qual.symbol.hasPackageFlag =>
              f.toSemanticQualifierTree
            case f => f.toSemanticTree
          }
          s.ApplyTree(fun, List(range.toSemanticOriginal))
        }

        private def getApplyToImplicitArgs(gt: g.ApplyToImplicitArgs, fun: s.Tree): s.ApplyTree = s
          .ApplyTree(fun, gt.args.map(_.toSemanticTree))

        private def getApplyToImplicitArgs(gt: g.ApplyToImplicitArgs): s.ApplyTree = {
          val fun = gt.fun match {
            case f: g.Apply => cachedOrOrig(f)(forApply(f, usePos = false))
            case f => f.toSemanticOriginal
          }
          getApplyToImplicitArgs(gt, fun)
        }

        private def cachedForOrOrig(gt: g.Tree) = cachedOrOrig(gt)(gt match {
          case t: g.ApplyToImplicitArgs => syn(getApplyToImplicitArgs(t))
          case t: g.Apply => forApply(t, usePos = false)
          case _ => None
        })

        private def cachedOrOrig(gt: g.Tree)(res: => Option[s.Synthetic]) = cached(gt)(res)
          .fold[s.Tree](gt.toSemanticOriginal)(_.tree)

        private def forApply(gt: g.Apply, usePos: Boolean = true): Option[s.Synthetic] =
          gt.args match {
            case (f: g.Function) :: Nil => cachedForApplyFun(gt.fun).filter(_.range.isEmpty)
                .flatMap { x =>
                  // a pattern binds through a match on the parameter; its range spans the generator
                  val body = f.body match {
                    case g.Match(g.Typed(sel, _), cd :: Nil)
                        if f.vparams.exists(_.symbol == sel.symbol) => cd.body
                    case body => body
                  }
                  val arg = s.FunctionTree(f.vparams.map(_.toSemanticId), cachedForOrOrig(body))
                  syn(gt.toRange, s.ApplyTree(x.tree, List(arg)), usePos)
                }
            case _ => None
          }

        private def cachedForApplyFun(gt: g.Tree): Option[s.Synthetic] = cached(gt)(gt match {
          case t: g.Select => getSelect(t, usePos = false)
          case t: g.TypeApply => getTypeApply(t, isWithinFor = true)
          case _ => None
        })
      }

      def tryFindSynthetic(gtree: g.Tree): Unit =
        if (config.synthetics.isOn && gtree.pos.isRange) Synth.getCached(gtree)

      val finalSymbols = symbols.values.toList

      val finalOccurrences = {
        val buf = List.newBuilder[s.SymbolOccurrence]
        for {
          map <- Iterator(occurrences, mpatoccurrences, samoccurrences, sammethodoccurrences)
          (pos, (sym, role)) <- map
          flatSym <- sym.asMulti
        } buf += s.SymbolOccurrence(Some(pos.toRange), flatSym, role)
        buf.result()
      }

      val diagnostics = unit.reportedDiagnostics(mstarts)

      s.TextDocument(
        schema = s.Schema.SEMANTICDB4,
        uri = unit.source.toUri,
        text = unit.source.toText,
        md5 = unit.source.toMD5,
        language = s.Language.SCALA,
        buildTarget = config.buildTarget,
        symbols = finalSymbols.sortBy(_.symbol),
        occurrences = finalOccurrences.sortBy(_.range),
        diagnostics = diagnostics.sortBy(_.range),
        synthetics = synthetics.toScalaSeq.sortBy(_.range),
      )
    }
  }

  private def syntheticName[A](gt: g.Name)(fFor: => A)(fOther: => A)(fElse: => A): A = gt match {
    case g.nme.foreach | g.nme.withFilter | g.nme.flatMap | g.nme.map => fFor
    case g.nme.apply | g.nme.update | g.nme.unapplySeq | g.nme.unapply => fOther
    case _ => fElse
  }

  private def isSyntheticName(gt: g.Select): Boolean = gt.pos == gt.qualifier.pos &&
    syntheticName(gt.name)(true)(true)(false)

  private def syntaxAndPos(mtree: m.Tree): String = s"`$mtree`[${mtree.pos.syntax}]"

  private def errorAmbiguous(kind: String, ntree: m.Tree, otree: m.Tree): Unit = sys
    .error(s"ambiguous $kind: ${syntaxAndPos(ntree)} ${syntaxAndPos(otree)}")

  private def wrapAlternatives(name: String, alts: g.Symbol*): g.Symbol = {
    val normalizedAlts = {
      val alts1 = alts.toList.filter(_.exists)
      val alts2 = alts1.map(alt => if (alt.isModuleClass) alt.asClass.module else alt)
      alts2.distinct
    }
    normalizedAlts match {
      case List(sym) => sym
      case normalizedAlts =>
        val wrapper = g.NoSymbol.newTermSymbol(g.TermName(name))
        wrapper.setFlag(gf.OVERLOADED)
        wrapper.setInfo(g.OverloadedType(g.NoType, normalizedAlts))
    }
  }

}
