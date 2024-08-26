package scala.meta.internal.semanticdb.scalac

import org.scalameta.internal.ScalaCompat._
import scala.meta.Dialect
import scala.meta.internal.inputs._
import scala.meta.internal.io.PathIO
import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal._
import scala.reflect.internal.util._
import scala.reflect.internal.{Flags => gf}
import scala.reflect.io.{PlainFile => GPlainFile}
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

  implicit class XtensionCompilationUnitDocument(unit: g.CompilationUnit) {
    def toTextDocument: s.TextDocument = toTextDocument(None)

    def toTextDocument(explicitDialect: Option[m.Dialect]): s.TextDocument = {
      pointsCache.clear()
      val binders = mutable.Set[m.Position]()
      val occurrences = mutable.Map[m.Position, String]()
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

      // Occurrences for names in val patterns, like `val (a, b) =`. Unlike the `occurrences` map, val pattern
      // occurrences uses "last occurrence wins" instead of "first occurrences wins" when disambiguating between
      // multiple symbols that resolve to the same position.
      val mpatoccurrences = mutable.Map[m.Position, String]()
      val mvalpatstart = mutable.Set.empty[Int] // start pos for Pat.Var names inside val patterns
      // start pos for vals with patterns -> last Pat.Var name
      val msinglevalpats = mutable.Map.empty[Int, m.Position]

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
          private def getAssignLhsAndIndexRhs(terms: Seq[m.Term.ArgClause]): List[m.Name] = {
            val names = List.newBuilder[m.Name]
            terms.foreach(_.values.foreach {
              case t: m.Term.Assign =>
                indexArgNames(t.rhs)
                t.lhs match {
                  case lhs: m.Term.Name => names += lhs
                  case _ =>
                }
              case _ =>
            })
            names.result()
          }
          private def indexArgNames(mapp: m.Tree): Unit = mapp match {
            case t: m.Init => margnames(t.pos.start) = getAssignLhsAndIndexRhs(t.argClauses)

            case t: m.Term.Apply =>
              margnames(t.fun.pos.end) = getAssignLhsAndIndexRhs(t.argClause :: Nil)

            case t: m.Term.Select => indexArgNames(t.qual)
            case _ =>
          }
          private def indexWithin(mname: m.Name.Indeterminate): Unit = {
            todo += mname
            val mencl = mname.parent.parent.get
            mencl match {
              case mencl: m.Ctor.Primary =>
                val menclName = mencl.parent.get.asInstanceOf[m.Member].name
                mwithinctors.put(menclName, mname).foreach(errorAmbiguous("mWithinCtors", mname, _))
              case _ =>
                def findBinder(pat: m.Pat) = pat.collect { case m.Pat.Var(name) => name }.head
                val menclName = mencl match {
                  case mtree: m.Member => mtree.name
                  case m.Decl.Val(_, pat :: Nil, _) => findBinder(pat)
                  case m.Decl.Var(_, pat :: Nil, _) => findBinder(pat)
                  case m.Defn.Val(_, pat :: Nil, _, _) => findBinder(pat)
                  case m.Defn.Var.Initial(_, pat :: Nil, _, _) => findBinder(pat)
                }
                mwithins.put(menclName, mname).foreach(errorAmbiguous("mWithins", mname, _))
            }
          }
          def indexPats(pats: List[m.Pat], pos: m.Position): Unit = pats match {
            case m.Pat.Var(_) :: Nil =>
            case _ => pats.foreach { pat =>
                pat.traverse { case m.Pat.Var(name) =>
                  mvalpatstart += name.pos.start
                  // Map the start position of the entire Defn.Val `val Foo(name) = ..` to the `name` position.
                  // This is needed to handle val patterns with a single binder since the position of `x` in the
                  // desugared `val x = ... match { case Foo(_x) => _x }` matches the position of Defn.Val
                  // and not `_x`.
                  msinglevalpats(pos.start) = name.pos
                }
              }
          }
          override def apply(mtree: m.Tree): Unit = {
            mtree match {
              case mtree: m.Term.Apply => indexArgNames(mtree)
              case m.Mod.Private(mname: m.Name.Indeterminate) => indexWithin(mname)
              case m.Mod.Protected(mname: m.Name.Indeterminate) => indexWithin(mname)
              case m.Importee.Rename(mname, mrename) =>
                indexName(mname)
                return // NOTE: ignore mrename for now, we may decide to make it a binder
              case mtree: m.Ctor => mctordefs(mtree.pos.start) = mtree.name
              case mtree: m.Term.New => mctorrefs(mtree.pos.start) = mtree.init.name
              case mtree: m.Init =>
                indexArgNames(mtree)
                mctorrefs(mtree.pos.start) = mtree.name
              case _: m.Name.Anonymous | _: m.Name.Placeholder | _: m.Name.This =>
              case mtree: m.Name => indexName(mtree)
              case mtree: m.Defn.Val => indexPats(mtree.pats, mtree.pos)
              case mtree: m.Defn.Var => indexPats(mtree.pats, mtree.pos)
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
            if (config.symbols.isNone) return
            if (gsym == null) return
            if (gsym.hasPackageFlag) return
            if (gsym.isUselessSymbolInformation) return
            val symbol = gsym.toSemantic
            if (symbol == Symbols.None) return
            if (config.symbols.isLocalOnly && !symbol.isLocal) return

            def saveSymbol(gs: g.Symbol): Unit = if (gs.isUsefulSymbolInformation)
              symbols(gs.toSemantic) = gs.toSymbolInformation(SymlinkChildren)

            saveSymbol(gsym)
            if (gsym.isClass && !gsym.isTrait) {
              if (gsym.isAnonymousClass) {
                val pos = gsym.pos.focus.toMeta
                binders += pos
                occurrences(pos) = gsym.toSemantic
              }
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
              val getterInfo = symbols(symbol)
              val setterInfos = Synthetics.setterInfos(getterInfo, SymlinkChildren)
              setterInfos.foreach { info =>
                val msymbol = info.symbol
                symbols(msymbol) = info
              }
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
            val symbol = gsym.toSemantic
            if (symbol == Symbols.None) return

            todo -= mtree

            if (mtree.isDefinition) {
              binders += pos
              if (mvalpatstart.contains(pos.start)) {
                if (gsym.name.endsWith(mtree.value)) mpatoccurrences(pos) = symbol
              } else occurrences(pos) = symbol
            } else {
              val isDefinedWithinThisSourceFile = gsym.pos.source == unit.source
              // We don't emit occurrences for accesses on structural types
              // because is no canonical definition of a structural types,
              // the same structure can be re-declared in various locations.
              // However, in the special case where the structural type is defined
              // within the same file we use the local symbol that's generated by scalac.
              // We can't use the local symbol if `gsym.pos.source != unit.source` because
              // that would mean we reference local symbols across files.
              val selectionFromGlobalStructuralType = !isDefinedWithinThisSourceFile &&
                gsym.owner.isRefinementClass
              if (!selectionFromGlobalStructuralType) occurrences(pos) = symbol
            }

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
          private def tryNamedArg(gsym: g.Symbol, gstart: Int, gpoint: Int): Unit =
            if (gsym != null && (gsym.isMethod || gsym.isConstructor)) for {
              margnames <- margnames.get(gstart) ++ margnames.get(gpoint)
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
              case gtree: g.DefDef =>
                val gsym = gtree.symbol
                if (gsym.isConstructor) Some(gsym) else None
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
              case gtree: g.MemberDef if gtree.symbol.isSynthetic || gtree.symbol.isArtifact =>
                if (!gsym.isSemanticdbLocal && !gsym.isUseless)
                  symbols(gsym.toSemantic) = gsym.toSymbolInformation(SymlinkChildren)
              case gtree: g.PackageDef =>
                // NOTE: capture PackageDef.pid instead
                ()
              case gtree: g.ModuleDef if gtree.name == g.nme.PACKAGE =>
                // NOTE: if a package object comes first in the compilation unit
                // then its positions are completely mental, so we just hack around
                tryMstart(gpoint + 7)
                tryMstart(gpoint)
              case gtree: g.ValDef =>
                val gsym = gtree.symbol
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
              case gtree: g.Super => tryMend(gend - 1)
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
              case gview: g.ApplyImplicitView =>
                val pos = gtree.pos.toMeta
                synthetics += s.Synthetic(
                  range = Some(pos.toRange),
                  tree = s.ApplyTree(
                    function = gview.fun.toSemanticTree,
                    arguments = List(s.OriginalTree(range = Some(pos.toRange)))
                  )
                )
                isVisited += gview.fun
              case gimpl: g.ApplyToImplicitArgs => gimpl.fun match {
                  case gview: g.ApplyImplicitView =>
                    isVisitedParent += gview
                    val range = gtree.pos.toMeta.toRange
                    synthetics += s.Synthetic(
                      range = Some(range),
                      tree = s.ApplyTree(
                        function = s.ApplyTree(
                          function = gview.fun.toSemanticTree,
                          arguments = List(s.OriginalTree(range = Some(range)))
                        ),
                        arguments = gimpl.args.map(_.toSemanticTree)
                      )
                    )
                  case gfun if isForSynthetic(gfun) =>
                    val range = gimpl.pos.toMeta.toRange
                    val synthTree = forSyntheticOrOrig(gimpl)
                    synthetics += s.Synthetic(range = Some(range), tree = synthTree)
                  case gfun => synthetics += s.Synthetic(
                      range = Some(gfun.pos.toMeta.toRange),
                      tree = s.ApplyTree(
                        function = s.OriginalTree(range = Some(gfun.pos.toMeta.toRange)),
                        arguments = gimpl.args.map(_.toSemanticTree)
                      )
                    )
                }
              case g.TypeApply(fun, targs @ List(targ, _*)) =>
                if (targ.pos.isRange) return
                // for loops
                val fnTree = fun match {
                  case ApplySelect(select @ g.Select(qual, nme)) if isSyntheticName(select) =>
                    isVisitedParent += select
                    isVisitedParent += fun
                    val symbol = select.symbol.toSemantic
                    s.SelectTree(
                      qualifier = s.OriginalTree(range = Some(qual.pos.toMeta.toRange)),
                      id = Some(s.IdTree(symbol = symbol))
                    )
                  case _ => s.OriginalTree(range = Some(fun.pos.toMeta.toRange))
                }
                synthetics += s.Synthetic(
                  range = Some(fun.pos.toMeta.toRange),
                  tree = s
                    .TypeApplyTree(function = fnTree, typeArguments = targs.map(_.tpe.toSemanticTpe))
                )
              case ApplySelect(select @ g.Select(qual, nme)) if isSyntheticName(select) =>
                isVisitedParent += select
                val symbol = select.symbol.toSemantic
                synthetics += s.Synthetic(
                  range = Some(qual.pos.toMeta.toRange),
                  tree = s.SelectTree(
                    qualifier = s.OriginalTree(range = Some(qual.pos.toMeta.toRange)),
                    id = Some(s.IdTree(symbol = symbol))
                  )
                )
              case gtree if isForSynthetic(gtree) =>
                val range = gtree.pos.toMeta.toRange
                val synthTree = forSyntheticOrOrig(gtree)
                synthetics += s.Synthetic(range = Some(range), tree = synthTree)
              case _ =>
              // do nothing
            }
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
              case g.Function(params, body) if params.forall { param =>
                    param.symbol.isSynthetic || param.name.decoded.startsWith("x$")
                  } => traverse(body)
              case gtree: g.TypeTree if gtree.original != null => traverse(gtree.original)
              case gtree: g.TypeTreeWithDeferredRefCheck => traverse(gtree.check())

              case gtree: g.Literal
                  if gtree.tpe != null && gtree.tpe.typeSymbol == g.definitions.ClassClass &&
                    gtree.pos.isRange && isClassOf(gtree.pos.toMeta) =>
                val coLen = "classOf".length
                val mpos = gtree.pos.toMeta
                val mposFix = new m.Position.Range(mpos.input, mpos.start, mpos.start + coLen)

                occurrences(mposFix) = "scala/Predef.classOf()."

                gtree.value match {
                  // Limitation: can't extract occurrence from types inside classOf
                  // if it's not a TypeRef without any type applications.
                  // because `classOf[...]` is encoded as `Literal(Constant(...))` while typing and
                  // positinal information inside of `classOf` disappear after the typechecking.
                  case g.Constant(tpe @ g.TypeRef(_, _, Nil)) =>
                    val mposClazz =
                      new m.Position.Range(mpos.input, mpos.start + coLen + 1, mpos.end - 1)
                    occurrences(mposClazz) = tpe.typeSymbol.toSemantic
                  case _ =>
                }

              case gtree: g.MemberDef =>
                val gsym = gtree.symbol
                tryFindMtree(gtree)
                if (gsym != null) {
                  gsym.annotations.foreach(ann => traverse(ann.original))
                  // Map Defn.Val position of val pattern with single binder to the position
                  // of the single binder. For example, map `val Foo(x) = ..` to the position of `x`.
                  if (!gsym.isSynthetic) {
                    val gpos = gtree.pos
                    if (gpos != null && gpos.isRange) msinglevalpats.get(gpos.start)
                      .foreach { mpos =>
                        occurrences(mpos) = gsym.toSemantic
                        binders += mpos
                      }
                  }
                }
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
              case _ => tryFindMtree(gtree)
            }
            super.traverse(gtree)
          }
        }
        traverser.traverse(unit.body)
      }

      val finalSymbols = symbols.values.toList

      val finalOccurrences = {
        for {
          (pos, sym) <- Iterator(occurrences, mpatoccurrences).flatten
          flatSym <- sym.asMulti
        } yield {
          val role =
            if (binders.contains(pos)) s.SymbolOccurrence.Role.DEFINITION
            else s.SymbolOccurrence.Role.REFERENCE
          s.SymbolOccurrence(Some(pos.toRange), flatSym, role)
        }
      }.toList

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
