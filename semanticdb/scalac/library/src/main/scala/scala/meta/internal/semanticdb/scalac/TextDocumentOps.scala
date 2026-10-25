package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.SemanticCompat._
import scala.meta.internal.inputs._
import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal._
import scala.reflect.internal.{Flags => gf, util => gu}
import scala.runtime.IntRef
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

  // walks each tree once, as a document does: its expandee, its original, itself, its children
  private[scalac] abstract class DocumentTraverser(source: gu.SourceFile) extends g.Traverser {
    // macro expandees can have cycles, keep tracks of visited nodes.
    protected val isVisited = mutable.Set.empty[g.Tree]

    override final def traverse(gtree: g.Tree): Unit =
      if (isVisited.add(gtree)) traverseUnvisited(gtree)

    protected def visit(gtree: g.Tree, hasOriginal: Boolean): Unit
    protected def enter(gtree: g.Tree): Unit = {}
    protected def leave(gtree: g.Tree): Unit = {}

    private def traverseUnvisited(gtree: g.Tree): Unit = {
      enter(gtree)
      gtree.attachments.get[g.analyzer.MacroExpansionAttachment]
        .foreach(att => traverse(att.expandee))
      val hasOriginal = traverseOriginal(gtree)
      visit(gtree, hasOriginal)
      super.traverse(gtree)
      leave(gtree)
    }

    // false if `tree` has no original
    private def traverseOriginal(gtree: g.Tree): Boolean = {
      gtree match {
        case OriginalTreeOf(original) => traverse(original)
        case CompoundTypeTreeOf(original) => traverse(original)
        case t: g.TypeTree if t.original != null =>
          traverse(t.original)
          // annotations in the original are untyped, like `@field` in `@(deprecated @field)`
          t.tpe match {
            case tpe: g.AnnotatedType => tpe.annotations.foreach { ann =>
                val gpos = ann.original.pos
                if (gpos.isRange && gpos.source == source) traverse(ann.original)
              }
            case _ =>
          }
        case t: g.TypeTreeWithDeferredRefCheck => traverse(t.check())
        case _ => return false
      }
      true
    }

    protected def traverseAnnotations(gtree: g.Tree): Unit = gtree match {
      case t: g.MemberDef if t.symbol != null =>
        t.symbol.annotations.foreach(ann => traverse(ann.original))
      case _ =>
    }
  }

  implicit class XtensionCompilationUnitDocument(unit: g.CompilationUnit) {
    def toTextDocument: s.TextDocument = toTextDocument(None)

    def toTextDocument(explicitDialect: Option[m.Dialect]): s.TextDocument = {
      startDocument(unit)
      try textDocument(explicitDialect)
      finally endDocument()
    }

    private def textDocument(explicitDialect: Option[m.Dialect]): s.TextDocument = {
      val occurrences = emptyOccurrenceMap()
      val samoccurrences = emptyOccurrenceMap()
      // SAM methods share positions with their classes.
      val sammethodoccurrences = emptyOccurrenceMap()
      val symbols = mutable.Map[String, s.SymbolInformation]()
      val synthetics = mutable.ListBuffer[s.Synthetic]()
      val syntheticTreeCache = new mutable.HashMap[g.Tree, Option[s.Synthetic]]
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
      val mexistentials = mutable.Map[Int, m.Type.Existential]() // start offset -> existential type
      val msupers = mutable.Map.empty[Int, m.Term.Super] // end offset
      val mfuncs = mutable.Map.empty[Int, m.Position] // start offset of a function
      val mannottees = mutable.Map.empty[Int, m.Member] // start offset of an annotation's init
      val mexpansions = mutable.Map.empty[Int, m.Member] // the same, for a macro annotation
      val munderscores = mutable.Map.empty[Int, m.Position] // start offset -> an underscore

      // Occurrences for names in val patterns, like `val (a, b) =`. Unlike the `occurrences` map, val pattern
      // occurrences uses "last occurrence wins" instead of "first occurrences wins" when disambiguating between
      // multiple symbols that resolve to the same position.
      val mpatoccurrences = emptyOccurrenceMap()
      val mvalpatstart = mutable.Set.empty[Int] // start pos for Pat.Var names inside val patterns
      // start pos for vals with single-element patterns -> their Pat.Var name
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
              val ssym = gsym.toSemantic
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
        val gdecl = if (ssym.isLocal) localDeclaration(ssym, gs) else gs
        val info = gdecl.toSymbolInfo(SymlinkChildren, ssym)
        symbols(ssym) = info
        info
      }
      // a synthetic can include a parameter that has no occurrence
      def shouldNotSaveSymbol(gsym: g.Symbol, isInSynthetic: Boolean = false): Boolean =
        config.symbols.isNone || gsym == null || gsym.hasPackageFlag ||
          gsym.isUselessSymbolInformation || !isInSynthetic && gsym.isHiddenParameter
      def shouldNotSaveSemanticSymbol(ssym: String): Boolean = ssym == Symbols.None ||
        config.symbols.isLocalOnly && !ssym.isLocal

      locally {
        object traverser extends m.Traverser {
          private var mpatvar: m.Name = null // the last variable that indexPat found

          private def indexName(mname: m.Name): Unit = {
            val mpos = mname.pos
            if (mpos.isEmpty) return // for instance, missing type in method declaration
            todo += mname
            mstarts.put(mpos.start, mname).foreach(errorAmbiguous("mStart", mname, _))
            mends.put(mpos.end, mname).foreach(errorAmbiguous("mEnd", mname, _))
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
          private def setWithins(mencl: m.Tree, mname: m.Name): Unit = mwithins.put(mencl, mname)
            .foreach(errorAmbiguous("mWithins", mname, _))
          private def indexWithin(mname: m.Name.Indeterminate): Unit = {
            todo += mname
            mname.parent.parent.get match {
              case mencl: m.Ctor.Primary => mencl.parent match {
                  case Some(p: m.Member) => mwithinctors.put(p.name, mname)
                      .foreach(errorAmbiguous("mWithinCtors", mname, _))
                  case _ =>
                }
              case mencl: m.Member => setWithins(mencl.name, mname)
              case t: m.Tree.WithPats => t.pats match {
                  case p :: Nil => p.dfsIf({
                      case t: m.Pat.Var => setWithins(t.name, mname)
                      case _: m.Pat | _: m.Pat.ArgClause =>
                    }: PartialFunction[m.Tree, Unit])
                  case _ =>
                }
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
          private def indexUnderscore(mtree: m.Tree): m.Position = {
            val mpos = mtree.pos.trunc(1)
            munderscores(mpos.start) = mpos
            mpos
          }
          // the start of the colon after a pattern that has a type, or -1
          private def typedColon(mpat: m.Pat): Int =
            if (!mpat.parent.is[m.Pat.Typed]) -1
            else {
              val mtoks = mpat.tokens
              val pred = (t: m.Token) => t.isAny[m.Token.Trivia, m.Token.RightParen]
              mtoks.findWideNotOrNull(pred, mtoks.length) match {
                case t: m.Token.Colon => t.start
                case _ => -1
              }
            }
          @tailrec
          private def isNestedPat(mtree: m.Tree): Boolean = mtree.parent match {
            case Some(_: m.Tree.WithPats) => true
            case Some(p: m.Pat) => isNestedPat(p)
            case _ => false
          }
          @tailrec // only a whole pattern, like `_` in `val _ =` or `_: Int <- xs`, has a symbol
          private def isWholePat(mpat: m.Pat): Boolean = mpat.parent match {
            case Some(p: m.Pat.Typed) => isWholePat(p)
            case Some(_: m.Pat | _: m.Pat.ArgClause | _: m.Case) => false
            case _ => true
          }
          // index the variables of a pattern and accumulate their number
          private def indexPat(mtree: m.Tree, cnt: IntRef): Unit = mtree match {
            case pat: m.Pat.Var =>
              mpatvar = pat.name
              mvalpatstart += pat.name.pos.start
              cnt.elem += 1
            case pat: m.Pat.Extract => indexPatsWithExtract(pat, cnt)
            case _: m.Pat | _: m.Pat.ArgClause => mtree.foreachChild(indexPat(_, cnt))
            case _ =>
          }
          // In an Extract pattern `Foo(name) = ...`, let's map the start position of the `fun` field
          // to the `name` position. Compiler desugars it into a getter DefDef and specifically
          // in the case of a single binder sets the position of this getter as an OffsetPosition
          // pointing to "start of fun" rather than the field being extracted.
          def indexPatsWithExtract(extract: m.Pat.Extract, cnt: IntRef): Unit = extract.args match {
            case pat :: Nil =>
              // Most likely the difference coming from https://github.com/scalameta/scalameta/pull/4118
              val mpos = if (isOldBehaviourOnPats) extract.fun.pos.end else extract.fun.pos.start
              val curcnt = cnt.elem
              indexPat(pat, cnt)
              if (cnt.elem - curcnt == 1) msinglevalpats(mpos) = mpatvar.pos
            case pats => pats.foreach(indexPat(_, cnt))
          }
          override def apply(mtree: m.Tree): Unit = {
            mtree match {
              case mtree: m.Term.Apply => setArgNames(mtree.fun.pos.end :: Nil)(mtree.argClause)
              case m.Mod.WithWithin(mname: m.Name.Indeterminate) => indexWithin(mname)
              // NOTE: ignore mrename for now, we may decide to make it a binder
              case mtree: m.Importee.Rename =>
                indexName(mtree.name)
                return
              case mtree: m.Term.Super => msupers.update(mtree.pos.end, mtree)
              case mtree: m.Self => mtree.decltpe.foreach(t => mselftypes(t.pos.start) = t)
              case m.Term.ApplyType(m.Term.Name(Symbols.ClassOfLabel), mtpe :: Nil) =>
                mclassofs(mtree.pos.start) = mtpe
              case mtree: m.Type.Existential => mexistentials(mtree.pos.start) = mtree
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
              case _: m.Name.Anonymous | _: m.Name.This =>
              case _: m.Name.Placeholder | _: m.Term.Placeholder | _: m.Type.Wildcard =>
                indexUnderscore(mtree)
              case mtree: m.Pat.Wildcard => if (isWholePat(mtree)) {
                  val mpos = indexUnderscore(mtree)
                  // scalac 2.12 and 2.13.15 put the value of `val (_: Int) = 1` at the colon
                  val colon = typedColon(mtree)
                  if (colon >= 0) munderscores(colon) = mpos
                }
              case mtree: m.Name => indexName(mtree)
              case mtree @ (_: m.Term.FunctionLike | _: m.Term.AnonymousFunction) =>
                val mpos = mtree.pos
                mfuncs.update(mpos.start, mpos)
              // index only the outermost patterns: they include the nested ones
              case mtree: m.Tree.WithPats if !isNestedPat(mtree) =>
                mtree.pats match {
                  case Nil | (_: m.Pat.Var) :: Nil =>
                  case pats =>
                    val cnt = IntRef.zero()
                    pats.foreach(indexPat(_, cnt))
                }
              case _ =>
            }
            super.apply(mtree)
          }
        }

        traverser(unit.toSource(explicitDialect))
      }

      locally {
        object traverser extends DocumentTraverser(unit.source) {
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
            // scalac types a reference to `self` in `class C { self => }` as `C.this`
            if (gsym0.isClass && mtree.isInstanceOf[m.Term.Name]) {
              val gself = gsym0.thisSym
              if ((gself ne gsym0) && gself.nameString == mtree.value) return success(mtree, gself)
            }
            if (gsym0.isUselessOccurrence) return
            if (gsym0.isImplicitPrimitiveConversion(mtree)) return
            // scalac puts the constructor of `@a.B` at `a`
            if (gsym0.isConstructor && mtree.isInstanceOf[m.Term.Name]) return
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
                else if (gsym.symbolName == mtree.value) addPatOccurrenceFromSemantic(pos, _)
                else null
              else addOccurrenceFromSemantic(pos, _, Role.REFERENCE)
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
          // define the symbol that scalac created for an underscore, like in `_ + 1` or `List[_]`
          private def defineUnderscore(gtree: g.Tree): Unit =
            defineUnderscore(findUnderscore(gtree), gtree.symbol)
          private def defineUnderscore(mpos: m.Position, gsym: g.Symbol): Unit =
            if (mpos ne null) addOccurrenceFromSemantic(mpos, saveUnderscore(gsym), Role.DEFINITION)
          // the underscore at a tree that has no occurrence yet, or null
          private def findUnderscore(gtree: g.Tree): m.Position = {
            if (munderscores.isEmpty) return null
            val gpos = gtree.pos
            if (!gpos.isDefined) return null
            val mpos = findUnderscore(gpos.start)
            if (mpos ne null) mpos else findUnderscore(gpos.point)
          }
          private def findUnderscore(offset: Int): m.Position = {
            val mpos = munderscores.getOrElse(offset, null)
            if ((mpos eq null) || occurrences.contains(mpos)) null else mpos
          }
          private def saveUnderscore(gsym: g.Symbol): String = {
            // define the getter rather than its field, and never an anonymous self
            if (gsym.isUseless || gsym.isAnonymousSelfParameter) return Symbols.None
            val ssym = gsym.toSemantic
            if (!config.symbols.isNone && !shouldNotSaveSemanticSymbol(ssym))
              saveSymbolFromSemantic(gsym, ssym)
            ssym
          }
          private def tryNamedArg(gsym: g.Symbol, offsets: Int*): Unit =
            if (gsym != null && (gsym.isMethod || gsym.isConstructor)) for {
              offset <- offsets
              margnames <- margnames.get(offset)
              margname <- margnames
              gparams <- gsym.paramss
              gparam <- gparams.find(_.decodedName == margname.value)
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
              case _: g.TypeDef | _: g.Bind | _: g.ValOrDefDef =>
                trySymbolDefinition(gsym)
                // scalac 2.12 and 2.13.15 start a constructor at the `_` of `extends A(_ + 1)`
                if (!gsym.isConstructor) defineUnderscore(gtree)
              case _: g.DefTree => trySymbolDefinition(gsym)
              case _ =>
            }

            // in a macro expansion, find only the annottee and its companion
            if (!gpos.isRange) mexpansions.get(gpoint) match {
              case Some(mannottee) =>
                if (gtree.isInstanceOf[g.ImplDef]) {
                  mannottee.parent.foreach { p =>
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
                  val gscope = genclosing.drop(1)
                  mannottee.foreachChild {
                    case t: m.Mod.Annot if t.init.pos.start == gpoint =>
                      tryFindMacroAnnotation(t.init.tpe, gscope, t.pos.start)
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
              case _: g.ValDef =>
                if (gsym.isMethod || !gsym.owner.isClass || gsym.getterIn(gsym.owner) == g.NoSymbol) {
                  // FIXME: https://github.com/scalameta/scalameta/issues/1538
                  // Skip the field definition in favor of the associated getter.
                  // This will make sure that val/var class parameters are consistently
                  // resolved to getter symbols both as definition and references.
                  tryMstart(gstart)
                  singleValPat()
                }
              // mctordefs found it above; its point can be on a parent's name
              case _: g.DefDef if gsym.isConstructor =>
              /* skip the fresh name that scalac binds to the pattern of a for value definition.
               * Its point can be the operator of an infix pattern, such as `::`. */
              case gtree: g.Bind if gtree.pos == gtree.body.pos =>
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
                  if (parents.contains(owner)) success(mtree.superp, owner)
                  else parents.find(_.isSubClass(owner)).foreach(success(mtree.superp, _))
                }
              case gtree: g.Select if gtree.symbol == g.definitions.NilModule =>
                // NOTE: List() gets desugared into mkAttributedRef(NilModule)
                tryMstart(gstart)
                // the type arguments of `List[T]()` remain only in the type of `Nil`
                if (gtree.tpe ne null) mstarts.get(gstart).flatMap(_.parent).foreach {
                  case p: m.Term.ApplyType => tryFindTypes(p.targClause.values, gtree.tpe.typeArgs)
                  case _ =>
                }
              case g.Select(g.Ident(qualTerm), _) // transformNamedApplication blockWithQualifier case
                  if qualTerm.decoded.startsWith(g.nme.QUAL_PREFIX) => tryMend(gend)
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
              case (mname: m.Term.Name, gpre: g.TypeRef) if isExistentialValueType(gpre.sym) =>
                success(mname, asExistentialValue(gpre.sym))
              case _ =>
            }

          /* scalac dealiased the parent, so its type and prefix may not be what the source names:
           * resolve the names from the scope around the class instead, and keep them if they
           * lead to the same class; the class itself binds only its type parameters there */
          private def tryFindParent(mtpe: m.Type, gtpe: g.Type): Boolean = {
            val gscope = genclosing.dropWhile(!_.isInstanceOf[g.ImplDef])
            @tailrec
            def dealiased(gtpe: g.Type): g.Symbol = gtpe match {
              case gtpe: g.PolyType => dealiased(gtpe.resultType)
              case _ => gtpe.dealias.typeSymbol
            }
            resolveTypePath(mtpe, gscope, mtpe.pos.start) match {
              case Some((gpre, gnames @ (_, gsym) :: _)) =>
                val gcls = if (gsym.isClass) gsym else dealiased(gpre.memberInfo(gsym))
                val isParent = gcls == gtpe.typeSymbol
                if (isParent) gnames.foreach { case (mname, gname) => success(mname, gname) }
                isParent
              case _ => false
            }
          }
          // the prefix of a type name and the symbols of the names in its path, the type first
          private def resolveTypePath(
              mtpe: m.Type,
              gscope: List[g.Tree],
              start: Int,
          ): Option[(g.Type, List[(m.Name, g.Symbol)])] = mtpe match {
            case mname: m.Type.Name => lookupMember(g.TypeName(mname.value).encode, gscope, start)
                .map { case (gpre, gsym) => (gpre, (mname -> gsym) :: Nil) }
            case mtpe: m.Type.Select => resolvePath(mtpe.qual, gscope, start)
                .flatMap { case (gpre, gquals) =>
                  val gsym = gpre.member(g.TypeName(mtpe.name.value).encode)
                  if (gsym.exists) Some((gpre, (mtpe.name -> gsym) :: gquals)) else None
                }
            case mtpe: m.Type.Project => resolveTypePath(mtpe.qual, gscope, start).flatMap {
                case (gpreQual, gquals @ (_, gqual) :: _) =>
                  val gpre = g.typeRef(gpreQual, gqual, Nil).dealias
                  val gsym = gpre.member(g.TypeName(mtpe.name.value).encode)
                  if (gsym.exists) Some((gpre, (mtpe.name -> gsym) :: gquals)) else None
                case _ => None
              }
            case _ => None
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
            case mq: m.Term.This => enclosingClass(mq.qual, gscope).map(gcls => (gcls.thisType, Nil))
            case mq: m.Term.Super => enclosingClass(mq.thisp, gscope).flatMap { gcls =>
                val gparents = gcls.info.parents
                val msuper = mq.superp
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
          private def lookupName(gname: g.Name, gscope: List[g.Tree], start: Int) =
            lookupMember(gname, gscope, start).map(_._2)
          /* the prefix and the symbol of a name in scope, innermost binding first, or None if
           * unsure; for a compiled tree, a binding never conflicts with one further out */
          private def lookupMember(gname: g.Name, gscope: List[g.Tree], start: Int) = {
            val gbindings = gscope.iterator.map(scopeBinding(gname, _, start)) ++
              grootImports.iterator.map(importBinding(gname, _)) ++
              Iterator(Some(member(g.rootMirror.RootClass.thisType, gname)))
            gbindings.find(_.forall(_._2.exists)).flatten
          }
          private def member(gpre: g.Type, gname: g.Name) = (gpre, gpre.member(gname))
          private val noBinding = (g.NoPrefix, g.NoSymbol)
          // the binding of a name in a scope: NoSymbol if none, None if unsure
          private def scopeBinding(
              gname: g.Name,
              gtree: g.Tree,
              start: Int,
          ): Option[(g.Type, g.Symbol)] = {
            def orElse(gbinding: Option[(g.Type, g.Symbol)])(other: => Option[(g.Type, g.Symbol)]) =
              if (gbinding.exists(_._2 == g.NoSymbol)) other else gbinding
            def imports(gstats: List[g.Tree]) = gstats.reverseIterator.collect {
              case t: g.Import if t.pos.isDefined && t.pos.start < start => importBinding(gname, t)
            }.find(_.forall(_._2.exists)).getOrElse(Some(noBinding))
            def defined(gtrees: List[g.Tree]) = Some(
              gtrees.collectFirst {
                case t: g.DefTree if t.name == gname && t.symbol != null => (g.NoPrefix, t.symbol)
              }.getOrElse(noBinding),
            )
            gtree match {
              case t: g.Block => orElse(imports(t.stats))(defined(t.stats))
              case t: g.Template => orElse(imports(t.body))(defined(t.self :: Nil)) match {
                  case Some((_, g.NoSymbol)) => Some(member(t.symbol.owner.thisType, gname))
                  case gbinding => gbinding
                }
              case t: g.PackageDef => orElse(imports(t.stats)) {
                  val gpkg = t.pid.symbol.moduleClass
                  val gbinding = member(gpkg.thisType, gname)
                  Some(if (gbinding._2.exists) gbinding else member(gpkg.packageObject.info, gname))
                }
              case t: g.ClassDef => defined(t.tparams)
              case t: g.DefDef => defined(t.tparams ++ t.vparamss.flatten)
              case t: g.Function => defined(t.vparams)
              case t: g.CaseDef => defined(t.pat.collect { case b: g.Bind => b })
              case _ => Some(noBinding)
            }
          }
          // the binding of a name by an import: NoSymbol if none, None if unsure
          private def importBinding(
              gname: g.Name,
              gimport: g.Import,
          ): Option[(g.Type, g.Symbol)] = {
            val gtpe = gimport.expr.tpe
            if (gtpe == null) None
            else Some {
              // selectors have term names, for a type too
              val gterm = gname.toTermName
              gimport.selectors.find(_.rename == gterm) match {
                case Some(sel) =>
                  member(gtpe, if (gname.isTypeName) sel.name.toTypeName else sel.name.toTermName)
                case _ =>
                  val isWildcard = gimport.selectors.exists(_.name == g.nme.WILDCARD)
                  // a selector that renames the name hides it from the wildcard
                  if (isWildcard && !gimport.selectors.exists(_.name == gterm)) member(gtpe, gname)
                  else noBinding
              }
            }
          }

          // match the names in a type that scalac left without symbols to its type
          @tailrec
          private def tryFindType(mtpe: m.Type, gtpe: g.Type): Unit = (mtpe, gtpe) match {
            case (mtpe: m.Type.Apply, gtpe: g.TypeRef) =>
              tryFindTypes(mtpe.argClause.values, gtpe.args)
              tryFindType(mtpe.tpe, gtpe)
            case (mname: m.Type.Name, gtpe: g.TypeRef) => success(mname, gtpe.sym)
            case (mtpe: m.Type.Select, gtpe: g.TypeRef) =>
              success(mtpe.name, gtpe.sym)
              tryFindQualifier(mtpe.qual, gtpe.pre)
            case (mtpe: m.Type.Project, gtpe: g.TypeRef) =>
              success(mtpe.name, gtpe.sym)
              tryFindType(mtpe.qual, gtpe.pre)
            case (mtpe: m.Type.Tuple, gtpe: g.TypeRef) => tryFindTypes(mtpe.args, gtpe.args)
            case (mtpe: m.Type.Function, gtpe: g.TypeRef) =>
              tryFindTypes(mtpe.paramClause.values :+ mtpe.res, gtpe.args)
            case (mtpe: m.Type.ByName, gtpe: g.TypeRef) => tryFindTypes(mtpe.tpe :: Nil, gtpe.args)
            case (mtpe: m.Type.Repeated, gtpe: g.TypeRef) => tryFindTypes(mtpe.tpe :: Nil, gtpe.args)
            case (mtpe: m.Type.ApplyInfix, gtpe: g.TypeRef) =>
              tryFindTypes(mtpe.lhs :: mtpe.rhs :: Nil, gtpe.args)
              tryFindType(mtpe.op, gtpe)
            case (mtpe: m.Type.Singleton, _: g.SingleType | _: g.TypeRef) =>
              tryFindQualifier(mtpe.ref, gtpe)
            case (mtpe: m.Type.With, gtpe: g.RefinedType) => tryFindParents(mtpe, gtpe)
            case (mtpe: m.Type.Refine, gtpe: g.RefinedType) =>
              tryFindDecls(mtpe.stats, gtpe.decls.toList)
              mtpe.tpe.foreach(tryFindParents(_, gtpe))
            case (mtpe: m.Type.Annotate, gtpe: g.AnnotatedType) =>
              // scalac lists the last annotation first
              tryFindTypes(mtpe.annots.map(_.init.tpe), gtpe.annotations.reverse.map(_.atp))
              tryFindType(mtpe.tpe, gtpe.underlying)
            case (mtpe: m.Type.Existential, gtpe: g.ExistentialType) =>
              // scalac merges a nested `forSome` into this one and lists its quantifiers last
              val (gquantified, ginner) = gtpe.quantified.splitAt(mtpe.stats.foldLeft(0) {
                case (n, mstat: m.Decl.Val) => n + mstat.pats.length
                case (n, _: m.Decl.Type) => n + 1
                case (n, _) => n
              })
              val gsyms = gquantified.map(gq =>
                if (isExistentialValueType(gq)) asExistentialValue(gq)
                else {
                  val gqinfo = gq.info
                  val ginfo = existentialValueRefs(gqinfo)
                  if (ginfo eq gqinfo) gq else gq.cloneSymbol.setInfo(ginfo)
                },
              )
              tryFindDecls(mtpe.stats, gsyms)
              tryFindType(mtpe.tpe, g.newExistentialType(ginner, gtpe.underlying))
            case (_, gtpe: g.ExistentialType) => tryFindType(mtpe, gtpe.underlying)
            case (mtpe: m.Type.Wildcard, gtpe: g.TypeRef) if gtpe.sym.isExistentiallyBound =>
              defineUnderscore(findUnderscore(mtpe.pos.start), gtpe.sym)
              tryFindBounds(mtpe.bounds, gtpe.sym.info.bounds)
            case _ =>
          }
          private def tryFindTypes(mtpes: List[m.Type], gtpes: List[g.Type]): Unit = mtpes.zip(gtpes)
            .foreach { case (mtpe, gtpe) => tryFindType(mtpe, gtpe) }
          private def tryFindBounds(mbounds: m.Type.Bounds, gbounds: g.TypeBounds): Unit = {
            mbounds.lo.foreach(tryFindType(_, gbounds.lo))
            mbounds.hi.foreach(tryFindType(_, gbounds.hi))
          }
          // scalac flattens `A with B with C` into one list of parents but keeps `(A with B)` whole
          @tailrec
          private def tryFindParents(mtpe: m.Type, gparents: List[g.Type]): Unit =
            (mtpe, gparents) match {
              case (_, gparent :: Nil) => tryFindType(mtpe, gparent)
              case (mtpe: m.Type.With, gparent :: gparents) =>
                tryFindType(mtpe.rhs, gparent)
                tryFindParents(mtpe.lhs, gparents)
              case _ =>
            }
          private def tryFindParents(mtpe: m.Type, gtpe: g.RefinedType): Unit =
            tryFindParents(mtpe, gtpe.parents.reverse)

          private def isNamed(n: String, isTerm: Boolean): g.Symbol => Boolean =
            x => x.isTerm == isTerm && x.decodedName == n
          // a `def` and a `val` or `var` of one name are different declarations
          private def isNamedTerm(n: String, isDef: Boolean): g.Symbol => Boolean = {
            val named = isNamed(n, isTerm = true)
            x => named(x) && x.isDefMethod == isDef
          }
          private def findByName(xs: Seq[g.Symbol], n: m.Type.Name): Option[g.Symbol] = xs
            .find(isNamed(n.value, isTerm = false))
          private def findByName(xs: Seq[g.Symbol], n: m.Term.Name): Option[g.Symbol] = xs
            .find(isNamedTerm(n.value, isDef = false))
          // scalac keeps overloads in the order of their declarations
          private def findOverload(
              mstats: List[m.Stat],
              mdef: m.Decl.Def,
              gsyms: Seq[g.Symbol],
          ): g.Symbol = {
            val name = mdef.name.value
            val git = gsyms.iterator.filter(isNamedTerm(name, isDef = true))
            val mit = mstats.iterator
            while (mit.hasNext && git.hasNext) mit.next() match {
              case `mdef` => return git.next()
              case mstat: m.Decl.Def if mstat.name.value == name => git.next()
              case _ =>
            }
            null
          }

          // match the declarations in `forSome` or a refinement, which scalac left untyped, to symbols
          private def tryFindDecls(mstats: List[m.Stat], gsyms: Seq[g.Symbol]): Unit = mstats
            .foreach {
              case mstat: m.Decl.Type => findByName(gsyms, mstat.name)
                  .foreach(tryFindTypeDecl(mstat.name, mstat.tparamClause.values, mstat.bounds, _))
              case mstat: m.Defn.Type => findByName(gsyms, mstat.name).foreach { gsym =>
                  tryFindTypeDecl(mstat.name, mstat.tparamClause.values, mstat.bounds, gsym)
                  tryFindType(mstat.body, gsym.info.resultType)
                }
              case mstat: m.Decl.Val => tryFindPatDecls(mstat.pats, mstat.decltpe, gsyms)
              case mstat: m.Decl.Var => tryFindPatDecls(mstat.pats, mstat.decltpe, gsyms)
              case mstat: m.Decl.Def =>
                val gsym = findOverload(mstats, mstat, gsyms)
                if (gsym ne null) {
                  tryFindTermDecl(mstat.name, mstat.decltpe, gsym)
                  mstat.paramClauseGroup.foreach { mgroup =>
                    val mtparams = mgroup.tparamClause.values
                    tryFindTypeParams(mtparams, gsym.typeParams)
                    // scalac adds a synthetic parameter for each view and context bound
                    val gevidence = new mutable.ListBuffer[g.Type]
                    val gparams = gsym.paramss.iterator.flatten.filter { gp =>
                      val synthetic = gp.isSynthetic
                      if (synthetic) gevidence += gp.info
                      !synthetic
                    }
                    mgroup.paramClauses.iterator.flatMap(_.values).zip(gparams).foreach {
                      case (mp, gp) => mp.decltpe.foreach(tryFindTermDecl(mp.name, _, gp))
                    }
                    while (gparams.hasNext) gparams.next() // drain
                    tryFindEvidence(mtparams, gevidence.iterator)
                  }
                }
              case _ =>
            }
          private def tryFindPatDecls(
              mpats: List[m.Pat],
              mtpe: m.Type,
              gsyms: Seq[g.Symbol],
          ): Unit = mpats.foreach {
            case x: m.Pat.Var => findByName(gsyms, x.name).foreach(tryFindTermDecl(x.name, mtpe, _))
            case _ =>
          }
          // scalac types `A <% B` as `A => B` and `A: B` as `B[A]`, in the order of the bounds
          private def tryFindEvidence(mtparams: List[m.Type.Param], gtpes: Iterator[g.Type]): Unit =
            mtparams.foreach { mtparam =>
              mtparam.bounds.view.iterator.zip(gtpes).foreach { case (mtpe, gtpe) =>
                gtpe.typeArgs.lastOption.foreach(tryFindType(mtpe, _))
              }
              mtparam.bounds.context.iterator.zip(gtpes).foreach { case (mtpe, gtpe) =>
                tryFindType(mtpe, gtpe)
              }
            }
          private def tryFindTermDecl(mname: m.Name, mtpe: m.Type, gsym: g.Symbol): Unit = {
            success(mname, gsym)
            trySymbolDefinition(gsym)
            tryFindType(mtpe, gsym.info.finalResultType)
          }
          private def tryFindTypeDecl(
              mname: m.Name,
              mtparams: List[m.Type.Param],
              mbounds: m.Type.Bounds,
              gsym: g.Symbol,
          ): Unit = {
            mname match {
              case mname: m.Type.Name => success(mname, gsym)
              case _ =>
            }
            trySymbolDefinition(gsym)
            tryFindTypeParams(mtparams, gsym.typeParams)
            tryFindBounds(mbounds, gsym.info.resultType.bounds)
          }
          private def tryFindTypeParams(
              mtparams: List[m.Type.Param],
              gtparams: List[g.Symbol],
          ): Unit = mtparams.zip(gtparams).foreach { case (mtparam, gtparam) =>
            tryFindTypeDecl(mtparam.name, mtparam.tparamClause.values, mtparam.bounds, gtparam)
          }

          private def isExistentialValueType(gsym: g.Symbol): Boolean = gsym
            .hasFlag(gf.EXISTENTIAL) && g.nme.isSingletonName(gsym.name)
          // quantified type -> value
          private val existentialValues = mutable.HashMap.empty[g.Symbol, g.Symbol]
          /* scalac quantifies `val x: T` as `type x.type <: T with Singleton`, and in `classOf` it
           * keeps no symbol of the value: make one at its point, to pair with any copy by name */
          private def asExistentialValue(gtype: g.Symbol): g.Symbol = existentialValues
            .getOrElseUpdate(
              gtype, {
                val gtpe = gtype.info.bounds.hi match {
                  case gtpe: g.RefinedType => gtpe.parents.headOption.getOrElse(gtpe)
                  case gtpe => gtpe
                }
                val gname = g.tpnme.dropSingletonName(gtype.name).toTermName
                gtype.owner.newValue(gname, gtype.pos, gf.EXISTENTIAL)
                  .setInfo(existentialValueRefs(gtpe))
              },
            )
          // a quantified type refers to `x.type`: refer to the value instead
          private object existentialValueRefs extends g.TypeMap {
            def apply(gtpe: g.Type): g.Type = gtpe match {
              case gtpe: g.TypeRef if isExistentialValueType(gtpe.sym) =>
                g.singleType(g.NoPrefix, asExistentialValue(gtpe.sym))
              case _ => mapOver(gtpe)
            }
          }

          // scalac keeps no tree of a macro annotation: resolve its name in the scope around it
          @tailrec
          private def tryFindMacroAnnotation(mtpe: m.Type, gscope: List[g.Tree], start: Int): Unit =
            mtpe match {
              case mtpe: m.Type.Apply => tryFindMacroAnnotation(mtpe.tpe, gscope, start)
              case _: m.Type.Name | _: m.Type.Select => resolveTypePath(mtpe, gscope, start).foreach {
                  case (_, (mname, gann) :: gquals) => successMacroAnnotation(mname, gann, gquals)
                  case _ =>
                }
              case _ =>
            }
          private def successMacroAnnotation(
              mname: m.Name,
              gann: g.Symbol,
              gquals: List[(m.Name, g.Symbol)],
          ): Unit = if (gann.isClass && gann.hasFlag(gf.MACRO)) {
            gquals.foreach { case (mq, gq) => success(mq, gq) }
            success(mname, gann)
          }

          private def processMemberDef(gtree: g.MemberDef) = {
            // a macro annotation puts its expansion at the offset of the annotation
            if (gtree.isInstanceOf[g.ImplDef]) {
              val gpos = gtree.pos
              if (gpos.isDefined && !gpos.isRange) mannottees.get(gpos.point)
                .foreach(mexpansions(gpos.point) = _)
            }
            traverseAnnotations(gtree)
            tryFindMtree(gtree)
          }

          override protected def enter(gtree: g.Tree): Unit = genclosing = gtree :: genclosing
          override protected def leave(gtree: g.Tree): Unit = genclosing = genclosing.drop(1)

          override protected def visit(gtree: g.Tree, hasOriginal: Boolean): Unit =
            if (!hasOriginal) traverseNoOriginal(gtree)

          private def stripClassOf(pos: m.Position): m.Position = {
            val off = pos.start
            val labelEnd = off + Symbols.ClassOfLabel.length
            if (labelEnd > pos.end) return pos
            val chars = pos.input.chars
            if (labelEnd > chars.length) return pos
            var i = 0
            while (i < Symbols.ClassOfLabel.length) {
              if (chars(off + i) != Symbols.ClassOfLabel(i)) return pos
              i += 1
            }
            pos.trunc(i)
          }

          private def traverseNoOriginal(gtree: g.Tree): Unit = gtree match {
            // skip an annotation that scalac added, like `@unchecked` on a desugared match
            case t: g.Annotated if !t.annot.pos.isOpaqueRange => isVisited += t.annot

            case t: g.Literal if t.tpe != null && t.tpe.typeSymbol == g.definitions.ClassClass =>
              val mpos = t.toMeta
              val mposClassOf = stripClassOf(mpos)
              if (mpos ne mposClassOf) {
                addOccurrenceFromSemantic(mposClassOf, "scala/Predef.classOf().", Role.REFERENCE)
                // scalac replaces `classOf[T]` with a constant of type T
                mclassofs.get(mpos.start).foreach(tryFindType(_, t.value.typeValue))
              } else tryFindMtree(t)

            case t: g.Function =>
              addSamOccurrence(t)
              // the parameter of a guard repeats that of its generator, which defines the `_`
              val isGuard = genclosing match {
                case _ :: g.Apply(gfun: g.Select, _) :: _ => gfun.name == g.nme.withFilter &&
                  gfun.hasAttachment[g.ForAttachment.type]
                case _ => false
              }
              // a for matches the parameter of its closure on the pattern
              val gpat = t.body match {
                case g.Match(_, g.CaseDef(gpat, _, _) :: Nil) => gpat
                case _ => g.EmptyTree
              }
              /* a for unpacks `y` of `y = x; _ <- ys` with a tuple pattern that has no range;
               * this parameter defines no `_`, though scalac 2.12 puts it at the one of `ys` */
              val isUnpacking = gpat.isInstanceOf[g.Apply] && !gpat.pos.isRange
              // scalac 2.13.15 may put a closure of a for at its body, but its pattern at `_`
              def findParamUnderscore(p: g.ValDef): m.Position = {
                val mpos = findUnderscore(gpat)
                if (mpos ne null) mpos else findUnderscore(p)
              }
              t.vparams.foreach(p =>
                if (!(p.symbol.isSynthetic || p.symbol.hasFreshName)) traverse(p)
                else if (isUnpacking) {}
                else if (!isGuard) defineUnderscore(findParamUnderscore(p), p.symbol)
                // mark it visited, or traversing the function would define the `_`
                else if (isVisited.add(p) && (findParamUnderscore(p) ne null))
                  saveUnderscore(p.symbol),
              )
              t.body match {
                /* skip the match of a pattern on the parameter.
                 * Its position can be the name after `yield`. */
                case g.Match(g.Typed(sel, _), cases) if t.vparams.exists(_.symbol == sel.symbol) =>
                  cases.foreach(traverse)
                case body => traverse(body)
              }
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
            case gtree: g.ExistentialTypeTree =>
              if (gtree.pos.isDefined) mexistentials.get(gtree.pos.start)
                .foreach(mtpe => tryFindDecls(mtpe.stats, gtree.whereClauses.map(_.symbol)))
              tryFindMtree(gtree)
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
                case t: g.ValDef =>
                  val rhs = t.rhs
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
                  mparents.get(p.pos.start).foreach(mtpe =>
                    if (!tryFindParent(mtpe, p.tpe)) mtpe match {
                      case mtpe: m.Type.Select => success(mtpe.name, p.tpe.typeSymbol)
                      case _ => tryFindType(mtpe, p.tpe)
                    },
                  )
                case _ =>
              }
              t.self.tpt match {
                case tpt: g.TypeTree if tpt.original != null && tpt.original.pos.isDefined =>
                  // the tree of `_` in `this: List[_] =>` defines it, not the type below
                  traverse(t.self)
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
            case f: g.Select
                if f.qualifier.symbol != null && !f.qualifier.symbol.isModule &&
                  !f.qualifier.symbol.hasPackageFlag => f.toSemanticQualifierTree
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
                  val arg = s.FunctionTree(f.vparams.map(toParamId), cachedForOrOrig(body))
                  syn(gt.toRange, s.ApplyTree(x.tree, List(arg)), usePos)
                }
            case _ => None
          }

        private def toParamId(gparam: g.ValDef): s.IdTree = {
          val gsym = gparam.symbol
          val ssym = gsym.toSemantic
          if (!shouldNotSaveSymbol(gsym, isInSynthetic = true) && !shouldNotSaveSemanticSymbol(ssym))
            saveSymbolFromSemantic(gsym, ssym)
          s.IdTree(ssym)
        }

        private def cachedForApplyFun(gt: g.Tree): Option[s.Synthetic] = cached(gt)(gt match {
          case t: g.Select => getSelect(t, usePos = false)
          case t: g.TypeApply => getTypeApply(t, isWithinFor = true)
          case _ => None
        })
      }

      def tryFindSynthetic(gtree: g.Tree): Unit =
        if (config.synthetics.isOn && gtree.pos.isRange) Synth.getCached(gtree)

      // the information of every local that the document refers to, but an existential hardlink
      if (!config.symbols.isNone) {
        // repeat while a pass frees an existential, which the pass may have skipped already
        var free = -1
        while (free != freeExistentials.size) {
          free = freeExistentials.size
          foreachDocumentLocal((ssym, gdecl) =>
            if (!symbols.contains(ssym) && !gdecl.isUseless && !gdecl.isError &&
              (!gdecl.hasFlag(gf.EXISTENTIAL) || freeExistentials(ssym)))
              saveSymbolFromSemantic(gdecl, ssym),
          )
        }
      }
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
