package scala.meta.internal.semanticdb.scalac

import org.scalameta.internal.ScalaCompat._
import scala.meta.internal.inputs._
import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal._
import scala.reflect.internal.util._
import scala.reflect.internal.{Flags => gf}
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

  implicit class XtensionCompilationUnitDocument(unit: g.CompilationUnit) {
    def toTextDocument: s.TextDocument = toTextDocument(None)

    def toTextDocument(explicitDialect: Option[m.Dialect]): s.TextDocument = {
      clearSymbolPointsCache()
      val occurrences = emptyOccurrenceMap()
      val samoccurrences = emptyOccurrenceMap()
      val symbols = mutable.Map[String, s.SymbolInformation]()
      val synthetics = mutable.ListBuffer[s.Synthetic]()
      // macro expandees can have cycles, keep tracks of visited nodes.
      val isVisited = mutable.Set.empty[g.Tree]
      // synthetics we have already visited the parents of
      val isVisitedParent = mutable.Set.empty[g.Tree]
      val todo = mutable.Set[m.Name]() // names to map to global trees
      val mstarts = mutable.Map[Int, m.Name]() // start offset -> tree
      val mends = mutable.Map[Int, m.Name]() // end offset -> tree
      // start offset of enclosing apply -> its arg names
      val margnames = mutable.Map[Int, List[m.Name]]()
      // name of enclosing member -> name of private/protected within
      val mwithins = mutable.Map[m.Tree, m.Name]()
      val mwithinctors = mutable.Map[m.Tree, m.Name]() // name of enclosing class -> name of private/protected within for primary ctor
      val mctordefs = mutable.Map[Int, m.Name]() // start offset of ctor -> ctor's anonymous name
      val mctorrefs = mutable.Map[Int, m.Name]() // start offset of new/init -> new's anonymous name
      val msupers = mutable.Map.empty[Int, m.Term.Super] // end offset
      val mfuncs = mutable.Map.empty[Int, m.Position] // start offset of a function

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

      def addSamOccurrence(gt: g.Function) = getSyntheticSAMClass(gt).foreach { sam =>
        val gsym = gt.symbol
        def atPos(mpos: m.Position): Unit = gsym.toSemantic match {
          case Symbols.None =>
          case ssym =>
            samoccurrences.update(mpos, (ssym, Role.DEFINITION))
            if (!shouldNotSaveSymbol(sam) && !shouldNotSaveSemanticSymbol(ssym))
              saveSymbolFromSemantic(sam, ssym)
        }
        val gpos = gt.pos
        if (gpos.isDefined && (gsym ne null)) {
          val gstart = gpos.start
          mfuncs.get(gstart).fold {
            mstarts.get(gstart).foreach { name =>
              val mpos = name.pos
              if (mpos.end == gpos.end) atPos(mpos)
            }
          }(atPos)
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
          def indexPats(pats: List[m.Pat]): Unit = pats.foreach(_.traverse {
            case pat: m.Pat.Extract => indexPatsWithExtract(pat)
            case pat: m.Pat.Var => mvalpatstart += pat.name.pos.start
          })
          // In an Extract pattern `Foo(name) = ...`, let's map the end position of the `fun` field
          // to the `name` position. Compiler desugars it into a getter DefDef and specifically
          // in the case of a single binder sets the position of this getter as an OffsetPosition
          // pointing to "end of fun" rather than the field being extracted.
          def indexPatsWithExtract(extract: m.Pat.Extract): Unit = extract.args match {
            case pat :: Nil =>
              val mpos = extract.fun.pos.end
              pat.traverse {
                case pat: m.Pat.Extract => indexPatsWithExtract(pat)
                case pat: m.Pat.Var =>
                  val npos = pat.name.pos; mvalpatstart += npos.start; msinglevalpats(mpos) = npos
              }
            case pats => indexPats(pats)
          }
          override def apply(mtree: m.Tree): Unit = {
            mtree match {
              case mtree: m.Term.Apply => setArgNames(mtree.fun.pos.end :: Nil)(mtree.argClause)
              case m.Mod.WithWithin(mname: m.Name.Indeterminate) => indexWithin(mname)
              // NOTE: ignore mrename for now, we may decide to make it a binder
              case m.Importee.Rename(mname, _) => indexName(mname); return
              case mtree: m.Term.Super => msupers.update(mtree.pos.end, mtree)
              case mtree: m.Ctor => mctordefs(mtree.pos.start) = mtree.name
              case mtree: m.Term.New => mctorrefs(mtree.pos.start) = mtree.init.name
              case mtree: m.Init =>
                mctorrefs(mtree.pos.start) = mtree.name
                def parentOpt = mtree.parent.collect { case p: m.Term.New => p }
                setArgNames((mtree +: parentOpt.toList).map(_.pos.start))(mtree.argClauses: _*)
              case _: m.Name.Anonymous | _: m.Name.Placeholder | _: m.Name.This =>
              case mtree: m.Name => indexName(mtree)
              case mtree @ (_: m.Term.FunctionTerm | _: m.Term.AnonymousFunction) =>
                val mpos = mtree.pos; mfuncs.update(mpos.start, mpos)
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
          private def trySymbolDefinition(gsym: g.Symbol): Unit = {
            if (shouldNotSaveSymbol(gsym)) return
            val symbol = gsym.toSemantic
            if (shouldNotSaveSemanticSymbol(symbol)) return
            val symInfo = saveSymbolFromSemantic(gsym, symbol)

            if (gsym.isClass && !gsym.isTrait) {
              if (gsym.isAnonymousClass)
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
          private def success(mtree: Option[m.Name], gsym0: => g.Symbol): Unit = mtree
            .foreach(success(_, gsym0))
          private def success(mtree: m.Name, gsym0: g.Symbol): Unit = {
            // We cannot be guaranteed that all symbols have a position, see
            // https://github.com/scalameta/scalameta/issues/665
            // Instead of crashing with "unsupported file", we ignore these cases.
            if (gsym0 == null) return
            if (gsym0.isUselessOccurrence) return
            if (gsym0.isImplicitPrimitiveConversion(mtree)) return
            val pos = mtree.pos
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
            register(symbol)

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
            if (gpos == null || gpos == NoPosition) return
            val gsym = gtree.symbol

            def tryMstart(pos: Int): Unit = success(mstarts.get(pos), gsym)
            def tryMend(pos: Int): Unit = success(mends.get(pos), gsym)

            gtree match {
              case _: g.DefTree => trySymbolDefinition(gsym)
              case _ =>
            }

            val gstart = gpos.start
            val gpoint = gpos.point
            tryNamedArg(gsym, gstart, gpoint)

            (gtree match {
              case gtree: g.Template => gtree.body.iterator.map(_.symbol)
                  .find(x => x != null && x.isPrimaryConstructor).orElse(Some(g.NoSymbol))
              case gtree: g.DefDef => if (gsym.isConstructor) Some(gsym) else None
              case _ => None
            }).foreach(success(mctordefs.get(gstart), _))

            gtree match {
              case g.Select(_, g.nme.CONSTRUCTOR) => success(mctorrefs.get(gpoint), gsym)
              case _ =>
            }

            // Ideally, we'd like a perfect match when gtree.pos == mtree.pos.
            // Unfortunately, this is often not the case as demonstrated by a bunch of cases above and below.
            val gend = gpos.end
            val gstartMtree = mstarts.get(gstart)
            gstartMtree match {
              case Some(mtree) if mtree.pos.end == gend => success(mtree, gsym); return
              case _ =>
            }

            gtree match {
              case gtree: g.ValDef if gsym.isSelfParameter => tryMstart(gstart)
              case gtree: g.MemberDef if gsym.isSynthetic || gsym.isArtifact =>
                if (!gsym.isSemanticdbLocal && !gsym.isUseless) saveSymbolDo(gsym)
              case gtree: g.PackageDef =>
                // NOTE: capture PackageDef.pid instead
                ()
              case gtree: g.ModuleDef if gtree.name == g.nme.PACKAGE =>
                // NOTE: if a package object comes first in the compilation unit
                // then its positions are completely mental, so we just hack around
                tryMstart(gpoint + 7)
                tryMstart(gpoint)
              case _: g.DefDef if gsym.isGetter =>
                msinglevalpats.get(gpoint) match {
                  case Some(mpos) => addPatOccurrence(mpos, gsym)
                  case _ => tryMstart(gpoint)
                }
              case _: g.ValDef =>
                if (!gsym.isMethod && gsym.getterIn(gsym.owner) != g.NoSymbol) {
                  // FIXME: https://github.com/scalameta/scalameta/issues/1538
                  // Skip the field definition in favor of the associated getter.
                  // This will make sure that val/var class parameters are consistently
                  // resolved to getter symbols both as definition and references.
                } else {
                  tryMstart(gstart)
                  tryMstart(gpoint)
                }
              case gtree: g.MemberDef => tryMstart(gpoint)
              case gtree: g.DefTree => tryMstart(gpoint)
              case gtree: g.This
                  if mstarts.get(gpoint).exists(name => gsym.nameString == name.value) =>
                tryMstart(gpoint)
              case t: g.Super => msupers.get(gend).foreach { mtree =>
                  success(mtree.thisp, gsym)
                  // now find the super parent
                  val parent = t.mix
                  val parents = gsym.asClass.info.parents.map(_.typeSymbol)
                  (if (parent.isEmpty) parents.headOption else parents.find(_.name == parent))
                    .foreach(success(mtree.superp, _))
                }
              case gtree: g.Select if gtree.symbol == g.definitions.NilModule =>
                // NOTE: List() gets desugared into mkAttributedRef(NilModule)
                tryMstart(gstart)
              case g.Select(g.Ident(qualTerm), _) if qualTerm.startsWith(g.nme.QUAL_PREFIX) =>
                tryMend(gend) // transformNamedApplication blockWithQualifier case
              case gtree: g.RefTree =>
                def prohibited(name: String) = name.contains(g.nme.DEFAULT_GETTER_STRING)
                if (prohibited(gtree.name.decoded)) return
                tryMstart(gpoint)
              case gtree: g.Import if gtree.expr != null && gtree.expr.tpe != null =>
                val sels = gtree.selectors.flatMap { sel =>
                  if (sel.name == g.nme.WILDCARD) Nil
                  else mstarts.get(sel.namePos).map(mname => (sel.name, mname))
                }
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

          private def tryFindSynthetic(gtree: g.Tree): Unit = {
            if (!config.synthetics.isOn) return

            if (!gtree.pos.isRange) return

            object ApplySelect {
              def unapply(tree: g.Tree): Option[g.Select] = Option(tree).collect {
                case g.Apply(select: g.Select, _) => select
                case select: g.Select => select
              }
            }

            @tailrec
            def isForSynthetic(gtree: g.Tree): Boolean = {
              def isForComprehensionSyntheticName(select: g.Select): Boolean = select.pos ==
                select.qualifier.pos &&
                (select.name == g.nme.map || select.name == g.nme.withFilter ||
                  select.name == g.nme.flatMap || select.name == g.nme.foreach)
              gtree match {
                case g.Apply(fun, List(arg: g.Function)) => isForSynthetic(fun)
                case g.TypeApply(fun, _) => isForSynthetic(fun)
                case gtree: g.Select if isForComprehensionSyntheticName(gtree) => true
                case _ => false
              }
            }

            def forMethodSelect(gtree: g.Tree): s.Tree = {
              isVisitedParent += gtree
              gtree match {
                case gtree: g.TypeApply =>
                  val typeArguments = gtree.args.map(_.tpe.toSemanticTpe)
                  val innerTree = forMethodSelect(gtree.fun)
                  s.TypeApplyTree(function = innerTree, typeArguments = typeArguments)
                case gtree: g.Select if isForSynthetic(gtree) =>
                  val qualifier = forSyntheticOrOrig(gtree.qualifier)
                  s.SelectTree(qualifier = qualifier, id = Some(gtree.toSemanticId))
              }
            }

            def forMethodBody(gtree: g.Tree): s.Tree = gtree match {
              case gtree: g.Function =>
                val names = gtree.vparams.map(_.toSemanticId)
                val bodyTree = forSyntheticOrOrig(gtree.body)
                s.FunctionTree(names, bodyTree)
              case _ => gtree.toSemanticOriginal
            }

            def forSyntheticOrOrig(gtree: g.Tree): s.Tree = {
              isVisitedParent += gtree
              gtree match {
                case gtree: g.ApplyToImplicitArgs =>
                  val implicitArgs = gtree.args.map(_.toSemanticTree)
                  val innerTree = forSyntheticOrOrig(gtree.fun)
                  s.ApplyTree(function = innerTree, arguments = implicitArgs)
                case gtree: g.Apply if isForSynthetic(gtree) =>
                  val fun = forMethodSelect(gtree.fun)
                  val body = forMethodBody(gtree.args.head)
                  s.ApplyTree(function = fun, arguments = List(body))
                case gtree => gtree.toSemanticOriginal
              }
            }

            if (!isVisitedParent(gtree)) gtree match {
              case gtree: g.ApplyImplicitView =>
                val range = gtree.toRange
                synthetics += s.Synthetic(
                  range = Some(range),
                  tree = s.ApplyTree(
                    function = gtree.fun.toSemanticTree,
                    arguments = Seq(range.toSemanticOriginal)
                  )
                )
                isVisited += gtree.fun
              case gtree: g.ApplyToImplicitArgs => gtree.fun match {
                  case gfun: g.ApplyImplicitView =>
                    isVisitedParent += gfun
                    val range = gtree.toRange
                    synthetics += s.Synthetic(
                      range = Some(range),
                      tree = s.ApplyTree(
                        function = s.ApplyTree(
                          function = gfun.fun.toSemanticTree,
                          arguments = Seq(range.toSemanticOriginal)
                        ),
                        arguments = gtree.args.map(_.toSemanticTree)
                      )
                    )
                  case gfun if isForSynthetic(gfun) =>
                    val range = gtree.toRange
                    val synthTree = forSyntheticOrOrig(gtree)
                    synthetics += s.Synthetic(range = Some(range), tree = synthTree)
                  case gfun =>
                    val range = gfun.toRange
                    synthetics += s.Synthetic(
                      range = Some(range),
                      tree = s.ApplyTree(
                        function = range.toSemanticOriginal,
                        arguments = gtree.args.map(_.toSemanticTree)
                      )
                    )
                }
              case g.TypeApply(fun, targs @ List(targ, _*)) =>
                if (targ.pos.isRange) return
                // for loops
                val range = fun.toRange
                val fnTree = fun match {
                  case ApplySelect(select @ g.Select(qual, _)) if isSyntheticName(select) =>
                    isVisitedParent += select
                    isVisitedParent += fun
                    val symbol = select.symbol.toSemantic
                    s.SelectTree(
                      qualifier = qual.toSemanticOriginal,
                      id = Some(s.IdTree(symbol = symbol))
                    )
                  case _ => range.toSemanticOriginal
                }
                synthetics += s.Synthetic(
                  range = Some(range),
                  tree = s
                    .TypeApplyTree(function = fnTree, typeArguments = targs.map(_.tpe.toSemanticTpe))
                )
              case ApplySelect(select @ g.Select(qual, _)) if isSyntheticName(select) =>
                isVisitedParent += select
                val symbol = select.symbol.toSemantic
                val range = qual.toRange
                synthetics += s.Synthetic(
                  range = Some(range),
                  tree = s.SelectTree(
                    qualifier = range.toSemanticOriginal,
                    id = Some(s.IdTree(symbol = symbol))
                  )
                )
              case gtree if isForSynthetic(gtree) =>
                val range = gtree.toRange
                val synthTree = forSyntheticOrOrig(gtree)
                synthetics += s.Synthetic(range = Some(range), tree = synthTree)
              case _ =>
              // do nothing
            }
          }

          private def processMemberDef(gtree: g.MemberDef) = {
            val gsym = gtree.symbol
            if (gsym != null) gsym.annotations.foreach(ann => traverse(ann.original))
            tryFindMtree(gtree)
          }

          override def traverse(gtree: g.Tree): Unit = if (isVisited.add(gtree)) {
            gtree.attachments.all.foreach {
              case att: g.analyzer.MacroExpansionAttachment => traverse(att.expandee)
              case _ =>
            }

            def isClassOf(pos: m.Position): Boolean = {
              val chars = pos.input.chars
              val classOfChars = "classOf".toCharArray

              chars.length >= (classOfChars.length + pos.start) && (0 until classOfChars.length)
                .forall(i => chars(i + pos.start) == classOfChars(i))
            }

            gtree match {
              case OriginalTreeOf(original) => traverse(original)
              case ConstfoldOf(original) => traverse(original)
              case ClassOf(original) => traverse(original)
              case NewArrayOf(original) => traverse(original)
              case SingletonTypeTreeOf(original) => traverse(original)
              case CompoundTypeTreeOf(original) => traverse(original)
              case ExistentialTypeTreeOf(original) => traverse(original)
              case AnnotatedOf(original) => traverse(original)
              case SelfTypeOf(original) => traverse(original)
              case SelectOf(original) => traverse(original)

              case gtree: g.TypeTree if gtree.original != null => traverse(gtree.original)
              case gtree: g.TypeTreeWithDeferredRefCheck => traverse(gtree.check())

              case t: g.Literal if t.tpe != null && t.tpe.typeSymbol == g.definitions.ClassClass =>
                val mpos = t.toMeta
                if (!mpos.isEmpty && isClassOf(mpos)) {
                  val mposClassOf = mpos.trunc("classOf".length)
                  addOccurrenceFromSemantic(mposClassOf, "scala/Predef.classOf().", Role.REFERENCE)
                  t.value match {
                    // Limitation: can't extract occurrence from types inside classOf
                    // if it's not a TypeRef without any type applications.
                    // because `classOf[...]` is encoded as `Literal(Constant(...))` while typing and
                    // positinal information inside of `classOf` disappear after the typechecking.
                    case g.Constant(tpe: g.TypeRef) if tpe.args.isEmpty =>
                      val mposClazz = mpos.input.pos(mposClassOf.end + 1, mpos.end - 1)
                      addOccurrence(mposClazz, tpe.typeSymbol, Role.REFERENCE)
                    case _ =>
                  }
                } else tryFindMtree(t)

              case t: g.Function =>
                addSamOccurrence(t)
                t.vparams.foreach { p =>
                  if (!(p.symbol.isSynthetic || p.name.decoded.startsWith("x$"))) traverse(p)
                }
                traverse(t.body)
                tryFindMtree(t)
              case t: g.ValDef =>
                val gsym = t.symbol
                if (gsym != null && gsym.isSynthetic) {
                  traverse(t.tpt)
                  traverse(t.rhs)
                }
                processMemberDef(t)
              case t: g.MemberDef => processMemberDef(t)
              case _: g.Apply | _: g.TypeApply =>
                tryFindSynthetic(gtree)
                val gpos = gtree.pos
                if (gpos != null && gpos.isRange) tryNamedArg(gtree.symbol, gpos.start, gpos.point)
                // fix #2040
                gtree match {
                  case view: g.ApplyImplicitView => view.args.headOption.foreach(traverse)
                  case _ =>
                }
              case select: g.Select if isSyntheticName(select) =>
                select.qualifier match {
                  // This case handles multiple synthetics in a row, for example
                  //
                  // ```val foo: Option[(Int, Int)] = None
                  //    for { (_, _) <- foo } yield ()```
                  // where `for` expands to `foo.withFilter(...).map(...)`
                  case g.Apply(s: g.Select, _) if isSyntheticName(s) => traverse(s)
                  case qualifier => traverse(qualifier)
                }
                tryFindSynthetic(select)
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
                tryFindMtree(t)
              case _ => tryFindMtree(gtree)
            }
            super.traverse(gtree)
          }
        }
        traverser.traverse(unit.body)
      }

      val finalSymbols = symbols.values.toList

      val finalOccurrences = {
        val buf = List.newBuilder[s.SymbolOccurrence]
        for {
          map <- Iterator(occurrences, mpatoccurrences, samoccurrences)
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
        symbols = finalSymbols,
        occurrences = finalOccurrences,
        diagnostics = diagnostics,
        synthetics = synthetics.toScalaSeq
      )
    }
  }

  private def isSyntheticName(select: g.Select): Boolean = select.pos == select.qualifier.pos &&
    (select.name == g.nme.apply || select.name == g.nme.update || select.name == g.nme.foreach ||
      select.name == g.nme.withFilter || select.name == g.nme.flatMap || select.name == g.nme.map ||
      select.name == g.nme.unapplySeq || select.name == g.nme.unapply)

  private def syntaxAndPos(gtree: g.Tree): String =
    if (gtree == g.EmptyTree) "\u001b[1;31mEmptyTree\u001b[0m"
    else {
      val text = gtree.toString.substring(0, Math.min(45, gtree.toString.length)).replace("\n", " ")
      s"$text [${gtree.pos.start}..${gtree.pos.end})"
    }

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
