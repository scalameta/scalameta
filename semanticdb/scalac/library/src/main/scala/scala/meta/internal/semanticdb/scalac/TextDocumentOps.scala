package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.inputs._
import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.{KnownSymbols => ks}
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal._
import scala.reflect.internal.{Flags => gf, util => gu}
import scala.tools.nsc.settings.ScalaVersion
import scala.util.control.NonFatal
import scala.{meta => m}

trait TextDocumentOps {
  self: SemanticdbOps =>

  import TextDocumentOps._

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
    // the tree index of the source, or an empty one if the source does not parse
    def toMtrees: TreeIndex =
      try {
        val msource = unit.toSource
        val mtrees = TreeIndex(msource)
        new MTreeIndexer(mtrees).apply(msource)
        mtrees
      } catch { case NonFatal(_) => TreeIndex.empty }

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
      val synthetics = mutable.ArrayBuffer[s.Synthetic]()
      val syntheticTreeCache = new mutable.HashMap[g.Tree, Option[s.Synthetic]]
      val msource = unit.toSource(explicitDialect)
      val mtrees = TreeIndex(msource)
      // the offset of a macro annotation that scalac expanded -> the annotated member
      val mexpansions = mutable.LongMap.empty[m.Member]
      val mcolons = mutable.LongMap.empty[m.Pat] // the colon after a whole pattern -> the pattern
      val mfors = mutable.ArrayBuffer.empty[m.Term.ForClause] // outer first

      // Occurrences for names in val patterns, like `val (a, b) =`. Unlike the `occurrences` map, val pattern
      // occurrences uses "last occurrence wins" instead of "first occurrences wins" when disambiguating between
      // multiple symbols that resolve to the same position.
      val mpatoccurrences = emptyOccurrenceMap()

      def mstart(offset: Int): m.Tree = mtrees.mstart(offset)
      def mend(offset: Int): m.Tree = mtrees.mend(offset)
      @tailrec
      def mtreeOf[A >: Null <: AnyRef](mtree: m.Tree, offset: Int, atEnd: Boolean)(
          f: m.Tree => A,
      ): A = {
        if (null eq mtree) return null
        val res = f(mtree)
        if (null ne res) return res
        mtree.parent match {
          case Some(p) if (if (atEnd) p.pos.end else p.pos.start) == offset =>
            mtreeOf[A](p, offset, atEnd)(f)
          case _ => null
        }
      }
      // the first non-null value of `f` on the trees that start at `offset`, innermost first
      def mstartOf[A >: Null <: AnyRef](offset: Int)(f: m.Tree => A): A =
        mtreeOf[A](mstart(offset), offset, atEnd = false)(f)
      // the first non-null value of `f` on the trees that end at `offset`, innermost first
      def mendOf[A >: Null <: AnyRef](offset: Int)(f: m.Tree => A): A =
        mtreeOf[A](mend(offset), offset, atEnd = true)(f)
      // an anonymous name, `this` and `_` have lookups of their own
      def asName(mtree: m.Tree): m.Name = mtree match {
        case _: m.Name.Anonymous | _: m.Name.This | _: m.Name.Placeholder => null
        case t: m.Name if !t.pos.isEmpty => t
        case _ => null
      }
      // a name is a leaf, so it is the last tree that starts or ends at its offsets
      def mnameAt(offset: Int): m.Name = asName(mstart(offset))
      def mnameEndingAt(offset: Int): m.Name = asName(mend(offset))

      // the method's name in a method value, like `g` in `K.g`
      @tailrec
      def methodName(gtree: g.Tree): m.Name = gtree match {
        case t: g.Function => methodName(t.body)
        case t: g.Apply => methodName(t.fun)
        case t: g.TypeApply => methodName(t.fun)
        case t: g.RefTree if t.pos.isDefined => mnameAt(t.pos.point)
        case _ => null
      }

      /* Find the enumerator of a closure that scalac makes for a `for`. The point of the closure is
       * in its generator. Its end is in its guard or in the body of the `for`.
       */
      def forEnumerator(gpos: gu.Position): Option[m.Position] = {
        def findWithin[A >: Null <: m.Tree](iter: Iterator[A], beg: Int, end: Int): A = {
          while (iter.hasNext) {
            val res = iter.next()
            if (res.containsIncl(beg, end)) return res
          }
          null
        }

        val gpoint = gpos.point
        val gendIncl = gpos.end - 1
        val mfor = findWithin(mfors.reverseIterator, gpoint, gendIncl)
        if (null eq mfor) return None

        val menums = mfor.enums
        val menum = findWithin(menums.iterator, gendIncl, gendIncl) match {
          case t: m.Enumerator.Guard => t
          case _: m.Enumerator.Val => null // a value is not a function in the source
          case _ => findWithin(menums.iterator, gpoint, gpoint)
        }
        if (null eq menum) None else Some(menum.pos)
      }

      /*
       * Methods to save symbols, occurrences, etc
       */

      def addOccurrenceFromSemantic(mpos: m.Position, ssym: String, role: Role): Unit =
        if (ssym != Symbols.None) occurrences(mpos) = (ssym, role)
      def addOccurrence(mpos: m.Position, gsym: g.Symbol, role: Role): Unit =
        addOccurrenceFromSemantic(mpos, gsym.toSemantic, role)

      def setSamOccurrence(
          gsym: g.Symbol,
          mdef: m.Position,
          map: mutable.Map[m.Position, Occurrence],
      ) = {
        val ssym = gsym.toSemantic
        if (ssym == Symbols.None) null
        else {
          // keep the definition of each nested lambda that starts at this position
          val sdef = map.get(mdef).fold(ssym)(o => Symbols.Multi(o._1.asMulti :+ ssym))
          map(mdef) = (sdef, Role.DEFINITION)
          saveSymbolFromSemantic(gsym, ssym)
        }
      }

      def addSamOccurrence(gt: g.Function, isFor: Boolean): Unit = {
        val sam = gt.attachments.get[global.SAMFunction].orNull
        if (sam eq null) return
        // the source does not have the check that scalac adds for a pattern that can fail
        if (isRefutabilityCheck(gt)) return
        val samSym = sam.synthCls
        def atPos(mpos: m.Position): Unit = {
          val entry = samSym.info.decls.elems
          if (entry eq null) return
          if (setSamOccurrence(samSym, mpos, samoccurrences) eq null) return

          val gmethod = entry.sym
          val info = setSamOccurrence(gmethod, mpos, sammethodoccurrences)
          if (info eq null) return

          // the method takes the lambda's parameters, also in its result type
          val gparams = gt.vparams.map(_.symbol)
          val gtpe = gmethod.info
          val gsig = g.MethodType(gparams, gtpe.resultType.substSym(gtpe.params, gparams))
            .toSemanticSig(SymlinkChildren)
          // for an existential type, like `G[_]`, scalac does not find the overridden method
          val overrides =
            if (config.overrides.isOn) {
              val samOverrides = (sam.sam :: sam.sam.allOverriddenSymbols).map(_.toSemantic)
              (info.overriddenSymbols ++ samOverrides).distinct
            } else info.overriddenSymbols
          symbols(info.symbol) = info.copy(signature = gsig, overriddenSymbols = overrides)
        }
        val gsym = gt.symbol
        val gpos = gt.pos
        if (gpos.isDefined && (gsym ne null) && !config.symbols.isNone) {
          val gbeg = gpos.start
          val gend = gpos.end
          // the largest tree from `mtree` within the function, like `K.g` or `g[Int]`
          @tailrec
          def outermost(mtree: m.Tree): m.Tree = mtree.parent match {
            case Some(p) if p.pos.start >= gbeg && p.pos.end <= gend => outermost(p)
            case _ => mtree
          }
          if (isFor) forEnumerator(gpos).foreach(atPos)
          else mstartOf[m.Position](gbeg) {
            case t: m.Term.FunctionLike => t.pos
            case t: m.Term.AnonymousFunction => t.pos
            case t: m.Term.PartialFunction => t.pos
            case _ => null
          } match {
            // scalac sets a transparent position on a method value, even inside a placeholder
            case mpos: m.Position if !gpos.isTransparent => atPos(mpos)
            case _ => methodName(gt.body) match {
                case name: m.Name =>
                  val mpos = outermost(name).pos
                  if (mpos.start == gbeg && mpos.end == gend) atPos(mpos)
                case _ =>
              }
          }
        }
      }

      def addPatOccurrenceFromSemantic(mpos: m.Position, ssym: String): Unit =
        if (ssym != Symbols.None) mpatoccurrences(mpos) = (ssym, Role.DEFINITION)

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
          gsym.isUselessSymbolInformation || gsym.isTemporary ||
          !isInSynthetic && gsym.isHiddenParameter
      def shouldNotSaveSemanticSymbol(ssym: String): Boolean = ssym == Symbols.None ||
        config.symbols.isLocalOnly && !ssym.isLocal

      object MyTreeIndexerTraverser extends MTreeIndexer(mtrees) {
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
        override protected def visit(mtree: m.Tree): Unit = mtree match {
          case mtree: m.Pat.Wildcard => if (isWholePat(mtree)) {
              // scalac before 2.13.17 puts the value of `val (_: Int) = 1` at the colon
              val colon = typedColon(mtree)
              if (colon >= 0) mcolons(colon) = mtree
            }
          // scalac before 2.13.17 puts the value of `val (a: Int) = 1` at the colon
          case mtree: m.Pat.Var => if (isWholePat(mtree)) {
              val colon = typedColon(mtree)
              if (colon >= 0) mcolons(colon) = mtree
            }
          case mtree: m.Term.ForClause => mfors += mtree
          case _ =>
        }
      }

      object MyDocumentTraverser extends DocumentTraverser(unit.source) {
        private var genclosing: List[g.Tree] = Nil // the current tree and its parents
        // for each template, the getters with a tree in its body
        private val ggetterTrees = mutable.HashMap.empty[g.Template, Set[g.Symbol]]
        private lazy val grootImports = g.analyzer.rootContext(unit).imports.map(_.tree)

        private def gettersFor(t: g.Template) = ggetterTrees.getOrElseUpdate(
          t,
          t.body.iterator.collect { case d: g.DefDef if d.symbol.isGetter => d.symbol }.toSet,
        )

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
        private def success(mtree: m.Name, gsym0: g.Symbol): Unit = {
          // We cannot be guaranteed that all symbols have a position, see
          // https://github.com/scalameta/scalameta/issues/665
          // Instead of crashing with "unsupported file", we ignore these cases.
          if ((null eq mtree) || (null eq gsym0)) return
          // scalac types a reference to `self` in `class C { self => }` as `C.this`
          if (gsym0.isClass && mtree.isInstanceOf[m.Term.Name]) {
            val gself = gsym0.thisSym
            if ((gself ne gsym0) && gself.nameString == mtree.value) return success(mtree, gself)
          }
          if (gsym0.isUselessOccurrence) return
          // a parameter that scalac adds has the position of the one it copies
          if (mtree.isDefinition && gsym0.isAddedParameter) return
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
              if (!isValPatName(mtree)) addOccurrenceFromSemantic(pos, _, Role.DEFINITION)
              else if (gsym.symbolName == mtree.value) addPatOccurrenceFromSemantic(pos, _)
              else null
            else addOccurrenceFromSemantic(pos, _, Role.REFERENCE)
          if (register eq null) return
          val symbol = gsym.toSemantic
          if (symbol == Symbols.None) return

          register(if (mtree.isDefinition) withCtorParam(gsym, symbol) else symbol)

          def tryWithin(mname: m.Name, gsym0: g.Symbol): Unit = if (null ne mname) {
            val gsym = gsym0.getterIn(gsym0.owner).orElse(gsym0)
            if (!gsym.hasAccessBoundary) return
            val within1 = gsym.privateWithin
            val within2 = within1.owner.info.member {
              val wname = within1.name
              if (wname.isTermName) wname.toTypeName else wname.toTermName
            }
            success(mname, wrapAlternatives("<within " + symbol + ">", within1, within2))
          }
          tryWithin(withinOf(mtree), gsym)
          tryWithin(
            mtree.parent match {
              case Some(p: m.Member with m.Stat.WithCtor) if p.name eq mtree => withinOf(p.ctor.mods)
              case _ => null
            },
            gsym.primaryConstructor,
          )
        }
        @tailrec
        private def successes(res: List[(m.Name, g.Symbol)]): Unit = res match {
          case Nil =>
          case (mtree, gsym) :: tail =>
            success(mtree, gsym)
            successes(tail)
        }
        // define the symbol that scalac created for an underscore, like in `_ + 1` or `List[_]`
        private def defineUnderscore(gtree: g.Tree): Unit =
          defineUnderscore(findUnderscore(gtree), gtree.symbol)
        private def defineUnderscore(mpos: m.Position, gsym: g.Symbol): Unit =
          if (mpos ne null) addOccurrenceFromSemantic(mpos, saveUnderscore(gsym), Role.DEFINITION)
        private val munderscore: m.Tree => m.Tree = {
          case t @ (_: m.Name.Placeholder | _: m.Term.Placeholder | _: m.Type.Wildcard) => t
          case t: m.Pat.Wildcard if isWholePat(t) => t
          case _ => null
        }
        // the underscore at a tree that has no occurrence yet, or null
        private def findUnderscore(gtree: g.Tree): m.Position = {
          val gpos = gtree.pos
          if (!gpos.isDefined) return null
          val mpos = findUnderscore(gpos.start)
          if (mpos ne null) mpos else findUnderscore(gpos.point)
        }
        private def findUnderscore(offset: Int): m.Position = {
          val mtree = mstartOf[m.Tree](offset)(munderscore) match {
            case null => mcolons.getOrNull(offset) match {
                case t: m.Pat.Wildcard => t
                case _ => null
              }
            case t => t
          }
          if (null eq mtree) null
          else {
            val mpos = mtree.pos.trunc(1)
            if (occurrences.contains(mpos)) null else mpos
          }
        }
        private def saveUnderscore(gsym: g.Symbol): String = {
          // define the getter rather than its field, and never an anonymous self
          if (gsym.isUseless || gsym.isAnonymousSelfParameter) return Symbols.None
          val ssym = gsym.toSemantic
          if (!config.symbols.isNone && !shouldNotSaveSemanticSymbol(ssym))
            saveSymbolFromSemantic(gsym, ssym)
          ssym
        }
        /* the name of the constructor at `offset`. An empty primary constructor is at its template,
         * or at the end of its class if the template is empty. */
        private def mctorAt(offset: Int): m.Name = mstartOf[m.Name](offset) {
          case t: m.Ctor => t.name
          case t: m.Template => t.parent match {
              case Some(p: m.Stat.WithCtor) if p.ctor.pos.start == t.pos.start => p.ctor.name
              case _ => null
            }
          case _ => null
        } match {
          case t: m.Name => t
          case _ => mendOf[m.Name](offset) {
              case t: m.Stat.WithCtor if t.ctor.pos.start == t.pos.end => t.ctor.name
              case _ => null
            }
        }
        @tailrec
        private def initOf(mtpe: m.Type): m.Init = mtpe.parent match {
          case Some(p: m.Type.Apply) if p.tpe eq mtpe => initOf(p)
          case Some(p: m.Init) if p.tpe eq mtpe => p
          case _ => null
        }
        // scalac puts the constructor reference of `n.B(1)` at `B`
        private def initNamedBy(mtpe: m.Type): m.Init = mtpe.parent match {
          case Some(p: m.Type.Select) if p.name eq mtpe => initOf(p)
          case Some(p: m.Type.Project) if p.name eq mtpe => initOf(p)
          case _ => mtpe match {
              case _: m.Type.Select | _: m.Type.Project | _: m.Type.Apply => null
              case _ => initOf(mtpe)
            }
        }
        private val mctorRef: m.Tree => m.Name = {
          case t: m.Term.New => t.init.name
          case t: m.Init => t.name
          case t: m.Type =>
            val init = initNamedBy(t)
            if (null eq init) null else init.name
          case _ => null
        }
        // the name of the constructor that `new`, an init or its type name refers to at `offset`
        private def mctorRefAt(offset: Int): m.Name = mstartOf[m.Name](offset)(mctorRef)
        // the type of the init at `offset`
        private def mparentAt(offset: Int): m.Type = mstartOf[m.Type](offset) {
          case t: m.Type => t.parent match {
              case Some(p: m.Init) if p.tpe eq t => t
              case _ => null
            }
          case _ => null
        }
        private def mselfTypeAt(offset: Int): m.Type = mstartOf[m.Type](offset) {
          case t: m.Type => t.parent match {
              case Some(_: m.Self) => t
              case _ => null
            }
          case _ => null
        }
        // the type argument of the `classOf` at `offset`
        private def mclassOfAt(offset: Int): m.Type = mstartOf[m.Type](offset) {
          case t: m.Term.ApplyType => t.fun match {
              case fun: m.Term.Name if fun.value == Symbols.ClassOfLabel =>
                t.targClause.values match {
                  case mtpe :: Nil => mtpe
                  case _ => null
                }
              case _ => null
            }
          case _ => null
        }
        private def msuperAt(end: Int): m.Term.Super = mendOf[m.Term.Super](end) {
          case t: m.Term.Super => t
          case _ => null
        }
        // the member that the annotation at `offset` annotates
        private def mannotteeAt(offset: Int): m.Member = mstartOf[m.Member](offset) {
          case t: m.Init => t.parent match {
              case Some(p: m.Mod.Annot) => p.parent match {
                  case Some(pp: m.Member) => pp
                  case _ => null
                }
              case _ => null
            }
          case _ => null
        }
        private val mvalPatVarAtStart: m.Tree => m.Pat.Var = {
          case t: m.Pat.Extract => if (IsOldBehaviourOnPats) null else extractVar(t)
          case _: m.Pat.ExtractInfix if IsOldBehaviourOnPats => null
          case t: m.Term.Name if IsOldBehaviourOnPats =>
            t.parent match {
              case Some(p: m.Pat.ExtractInfix) if (p.op eq t) && isValPat(p) => onlyVar(p)
              case _ => null
            }
          case t: m.Pat if isValPat(t) => onlyVar(t)
          case _ => null
        }
        private val mvalPatVarAtEnd: m.Tree => m.Pat.Var = {
          case t: m.Term => t.parent match {
              case Some(p: m.Pat.Extract) if p.fun eq t => extractVar(p)
              case _ => null
            }
          case _ => null
        }
        /* scalac before 2.13.17 (#4118) puts the value of `val a :: Nil = xs` at the operator,
         * of `val Tuple2(a, _) = x` at the paren and of `val (a: Int) = 1` at the colon */
        private def mvalPatVarAt(offset: Int): m.Name = {
          val mvar = mcolons.getOrNull(offset) match {
            case t: m.Pat.Var => t
            case _ =>
              val atStart =
                mtreeOf[m.Pat.Var](mstart(offset), offset, atEnd = false)(mvalPatVarAtStart)
              if ((null ne atStart) || !IsOldBehaviourOnPats) atStart
              else mtreeOf[m.Pat.Var](mend(offset), offset, atEnd = true)(mvalPatVarAtEnd)
          }
          if (null eq mvar) null else mvar.name
        }
        // the call whose arguments are at `offset`: past its function, or at its init or `new`
        private def mcallAt(offset: Int): m.Tree = mendOf[m.Tree](offset) {
          case t: m.Term => t.parent match {
              case Some(p: m.Term.Apply) if p.fun eq t => p
              case _ => null
            }
          case _ => null
        } match {
          case t: m.Tree => t
          case _ => mstartOf[m.Tree](offset) {
              case t: m.Init => t
              case t: m.Term.New => t.init
              case _ => null
            }
        }
        private def tryNamedArg(gsym: g.Symbol, offsets: Int*): Unit = {
          def tryNamedArgs(margs: m.Term.ArgClause): Unit = margs.values.foreach {
            case t: m.Term.Assign => t.lhs match {
                case mname: m.Term.Name => gsym.paramss
                    .foreach(_.find(_.decodedName == mname.value).foreach(success(mname, _)))
                case _ =>
              }
            case _ =>
          }
          if ((null ne gsym) && (gsym.isMethod || gsym.isConstructor)) offsets.foreach(offset =>
            mcallAt(offset) match {
              case t: m.Term.Apply => tryNamedArgs(t.argClause)
              case t: m.Init => t.argClauses.foreach(tryNamedArgs)
              case _ =>
            },
          )
        }

        private def tryFindMtree(gtree: g.Tree): Unit = {
          val gpos = gtree.pos
          if (gpos == null || !gpos.isDefined) return
          val gsym = gtree.symbol
          val gstart = gpos.start
          val gpoint = gpos.point
          val gend = gpos.end

          def tryMstart(pos: Int): Unit = success(mnameAt(pos), gsym)
          def tryMend(pos: Int): Unit = success(mnameEndingAt(pos), gsym)

          def singleValPat(sym: g.Symbol): Unit = {
            val mvar = mvalPatVarAt(gpoint)
            success(if (null ne mvar) mvar else mnameAt(gpoint), sym)
          }

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

          gtree match {
            case _: g.ClassDef => defineEvidence(gsym.primaryConstructor, gpoint)
            case _: g.DefDef if !gsym.isSynthetic && !gsym.isConstructor =>
              defineEvidence(gsym, gpoint)
            case _ =>
          }

          tryNamedArg(gsym, gstart, gpoint)

          (gtree match {
            case gtree: g.Template => gtree.body.iterator.map(_.symbol)
                .find(x => x != null && x.isPrimaryConstructor)
            case _: g.DefDef if gsym.isConstructor => Some(gsym)
            case _ => None
          }).foreach(success(mctorAt(gstart), _))

          gtree match {
            case g.Select(_, g.nme.CONSTRUCTOR) => success(mctorRefAt(gpoint), gsym)
            case _ =>
          }

          // Ideally, we'd like a perfect match when gtree.pos == mtree.pos.
          // Unfortunately, this is often not the case as demonstrated by a bunch of cases above and below.
          val gstartMtree = mnameAt(gstart)
          gstartMtree match {
            case mtree: m.Name if mtree.pos.end == gend =>
              success(mtree, gsym)
              return
            case _ =>
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
            case _: g.DefDef if gsym.isGetter => singleValPat(gsym)
            case _: g.ValDef =>
              val ggetter = gsym.getterForValDef
              if (ggetter == g.NoSymbol) {
                // FIXME: https://github.com/scalameta/scalameta/issues/1538
                // Skip the field definition in favor of the associated getter.
                // This will make sure that val/var class parameters are consistently
                // resolved to getter symbols both as definition and references.
                tryMstart(gstart)
                singleValPat(gsym)
              } else if (genclosing match {
                  // skip if the template of the field contains `ggetter`
                  case _ :: (t: g.Template) :: _ => t.symbol.owner.isSemanticdbLocal &&
                    !gettersFor(t)(ggetter)
                  case _ => false
                }) {
                // scalac did not keep the getter's tree
                trySymbolDefinition(ggetter)
                singleValPat(ggetter)
              }
            // mctorAt found it above; its point can be on a parent's name
            case _: g.DefDef if gsym.isConstructor =>
            /* skip the fresh name that scalac binds to the pattern of a for value definition.
             * Its point can be the operator of an infix pattern, such as `::`. */
            case gtree: g.Bind if gtree.pos == gtree.body.pos =>
            case _: g.DefTree => tryMstart(gpoint)
            case _: g.This => mnameAt(gpoint) match {
                case name: m.Name if gsym.nameString == name.value => success(name, gsym)
                case _ =>
              }
            case t: g.Super => msuperAt(gend) match {
                case mtree: m.Term.Super =>
                  success(mtree.thisp, gsym)
                  // now find the super parent
                  val parent = t.mix
                  val parents = gsym.asClass.info.parents.map(_.typeSymbol)
                  if (parent.nonEmpty) parents.find(_.name == parent)
                    .foreach(success(mtree.superp, _))
                case _ =>
              }
            // an unqualified super stands for the parent that has the selected member
            case g.Select(t: g.Super, _) if t.mix.isEmpty =>
              tryMstart(gpoint)
              if ((null ne gsym) && gsym.exists) msuperAt(t.pos.end) match {
                case mtree: m.Term.Super =>
                  val owner = gsym.owner
                  val parents = t.symbol.asClass.info.parents.map(_.typeSymbol)
                  if (parents.contains(owner)) success(mtree.superp, owner)
                  else parents.find(_.isSubClass(owner)).foreach(success(mtree.superp, _))
                case _ =>
              }
            case gtree: g.Select if gtree.symbol == g.definitions.NilModule =>
              // NOTE: List() gets desugared into mkAttributedRef(NilModule)
              tryMstart(gstart)
              // the type arguments of `List[T]()` remain only in the type of `Nil`
              if (gtree.tpe ne null) mnameAt(gstart).parentOrNull match {
                case p: m.Term.ApplyType => tryFindTypes(p.targClause.values, gtree.tpe.typeArgs)
                case _ =>
              }
            case g.Select(q: g.Ident, _) // transformNamedApplication blockWithQualifier case
                if q.name.decoded.startsWith(g.nme.QUAL_PREFIX) => tryMend(gend)
            case _: g.RefTree => if (!gsym.isDefaultGetter) tryMstart(gpoint)
            case gtree: g.Import if gtree.expr != null && gtree.expr.tpe != null =>
              gtree.selectors.foreach { sel =>
                val gname = sel.name
                val mname = if (gname == g.nme.WILDCARD) null else mnameAt(sel.namePos)
                if (null ne mname) {
                  val gexpr = gtree.expr
                  val gtpe = gexpr.tpe
                  val import1 = gtpe.member(gname.toTermName)
                  val import2 = gtpe.member(gname.toTypeName)
                  val altName = "<import " + gexpr + "." + gname + ">"
                  success(mname, wrapAlternatives(altName, import1, import2))
                }
              }
            case gtree: g.AppliedTypeTree
                if gtree.symbol.name == g.typeNames.REPEATED_PARAM_CLASS_NAME =>
              gtree.args.headOption.foreach(x => success(gstartMtree, x.symbol))
            case _ =>
          }
        }

        @tailrec
        private def tryFindQualifier(mqual: m.Term.Ref, gpre: g.Type): Unit = mqual match {
          case mqual: m.Term.Select => mqual.qual match {
              case mq: m.Term.Ref => gpre match {
                  case gpre: g.SingleType =>
                    success(mqual.name, gpre.sym)
                    tryFindQualifier(mq, gpre.pre)
                  case gpre: g.ThisType =>
                    success(mqual.name, gpre.sym)
                    tryFindQualifier(mq, gpre.sym.owner.thisType)
                  case _ =>
                }
              case _ =>
            }
          case mname: m.Term.Name => gpre match {
              case gpre: g.SingleType => success(mname, gpre.sym)
              case gpre: g.ThisType => success(mname, gpre.sym)
              case gpre: g.TypeRef if isExistentialValueType(gpre.sym) =>
                success(mname, asExistentialValue(gpre.sym))
              case _ =>
            }
          case _ =>
        }

        /* scalac dealiased the parent, so its type and prefix may not be what the source names:
         * resolve the names from the scope around the class instead, and keep them if they
         * lead to the same class; the class itself binds only its type parameters there */
        private def tryFindParent(mtpe: m.Type, gtpe: g.Type): Boolean = {
          val gscope = genclosing.dropWhile(!_.isInstanceOf[g.ImplDef])
          resolveTypePath(mtpe, gscope, mtpe.pos.start) match {
            case (gpre, gnames @ (_, gsym) :: _) =>
              val gcls =
                if (gsym.isClass) gsym else gpre.memberInfo(gsym).flatten.dealias.typeSymbol
              val isParent = gcls == gtpe.typeSymbol
              if (isParent) successes(gnames)
              isParent
            case _ => false
          }
        }
        // the prefix of a type name and the symbols of the names in its path, the type first
        private def resolveTypePath(
            mtpe: m.Type,
            gscope: List[g.Tree],
            start: Int,
        ): (g.Type, List[(m.Name, g.Symbol)]) = mtpe match {
          case mname: m.Type.Name =>
            lookupMember(g.TypeName(mname.value).encode, gscope, start) match {
              case null => null
              case (gpre, gsym) => (gpre, (mname -> gsym) :: Nil)
            }
          case mtpe: m.Type.Select => resolvePath(mtpe.qual, gscope, start) match {
              case null => null
              case (gpre, gquals) =>
                val gsym = gpre.member(g.TypeName(mtpe.name.value).encode)
                if (gsym.exists) (gpre, (mtpe.name -> gsym) :: gquals) else null
            }
          case mtpe: m.Type.Project => resolveTypePath(mtpe.qual, gscope, start) match {
              case null => null
              case (gpreQual, gquals @ (_, gqual) :: _) =>
                val gpre = g.typeRef(gpreQual, gqual, Nil).dealias
                val gsym = gpre.member(g.TypeName(mtpe.name.value).encode)
                if (gsym.exists) (gpre, (mtpe.name -> gsym) :: gquals) else null
              case _ => null
            }
          case _ => null
        }
        private def encoded(name: m.Term.Name) = g.TermName(name.value).encode
        // the type of a stable path and the symbols of its names, if scalac would accept it
        private def resolvePath(
            mqual: m.Term,
            gscope: List[g.Tree],
            start: Int,
        ): (g.Type, List[(m.Name, g.Symbol)]) = mqual match {
          case mqual: m.Term.Select => resolvePath(mqual.qual, gscope, start) match {
              case null => null
              case (gpre, gquals) => stablePath(gpre.member(encoded(mqual.name))) match {
                  case null => null
                  case (gtpe, gsym) => (gtpe, (mqual.name -> gsym) :: gquals)
                }
            }
          case mname: m.Term.Name if mname.value == "_root_" =>
            (g.rootMirror.RootClass.tpe, (mname -> g.rootMirror.RootPackage) :: Nil)
          case mname: m.Term.Name => stablePath(lookupName(encoded(mname), gscope, start)) match {
              case null => null
              case (gtpe, gsym) => (gtpe, (mname -> gsym) :: Nil)
            }
          case mq: m.Term.This =>
            val gcls = enclosingClass(mq.qual, gscope)
            if (null eq gcls) null else (gcls.thisType, Nil)
          case mq: m.Term.Super => enclosingClass(mq.thisp, gscope) match {
              case null => null
              case gcls =>
                val gparents = gcls.info.parents
                val msuper = mq.superp
                if (msuper.value.isEmpty) (g.intersectionType(gparents), Nil)
                else gparents.find(_.typeSymbol.decodedName == msuper.value) match {
                  case Some(gparent) => (gparent, Nil)
                  case _ => null
                }
            }
          case _ => null
        }
        // the class that `this` or `super` refers to, with an optional class name
        @tailrec
        private def enclosingClass(mname: m.Name, gscope: List[g.Tree]): g.Symbol = gscope match {
          case (t: g.Template) :: _
              if mname.value.isEmpty || t.symbol.owner.decodedName == mname.value => t.symbol.owner
          case _ :: tail => enclosingClass(mname, tail)
          case _ => null
        }
        private def stablePath(gsym: g.Symbol): (g.Type, g.Symbol) =
          if ((null eq gsym) || !gsym.exists || !gsym.isTerm || gsym.isOverloaded || !gsym.isStable)
            null
          else (gsym.info.finalResultType, gsym)
        private def lookupName(gname: g.Name, gscope: List[g.Tree], start: Int): g.Symbol =
          lookupMember(gname, gscope, start) match {
            case null => null
            case (_, gsym) => gsym
          }
        /* the prefix and the symbol of a name in scope, innermost binding first, or null if
         * unsure; for a compiled tree, a binding never conflicts with one further out */
        private def lookupMember(
            gname: g.Name,
            gscope: List[g.Tree],
            start: Int,
        ): (g.Type, g.Symbol) = {
          val gbindings = gscope.iterator.map(scopeBinding(gname, _, start)) ++
            grootImports.iterator.map(importBinding(gname, _)) ++
            Iterator(member(g.rootMirror.RootClass.thisType, gname))
          gbindings.find(x => (x eq null) || x._2.exists).orNull
        }
        private def member(gpre: g.Type, gname: g.Name) = (gpre, gpre.member(gname))
        private val noBinding = (g.NoPrefix, g.NoSymbol)
        // the binding of a name in a scope: NoSymbol if none, null if unsure
        private def scopeBinding(gname: g.Name, gtree: g.Tree, start: Int): (g.Type, g.Symbol) = {
          def orElse(gbinding: (g.Type, g.Symbol))(other: => (g.Type, g.Symbol)) =
            if ((gbinding ne null) && gbinding._2 == g.NoSymbol) other else gbinding
          def imports(gstats: List[g.Tree]) = gstats.reverseIterator.collect {
            case t: g.Import if t.pos.isDefined && t.pos.start < start => importBinding(gname, t)
          }.find(x => (x eq null) || x._2.exists).getOrElse(noBinding)
          def defined(gtrees: List[g.Tree]) = gtrees.collectFirst {
            case t: g.DefTree if t.name == gname && t.symbol != null => (g.NoPrefix, t.symbol)
          }.getOrElse(noBinding)
          gtree match {
            case t: g.Block => orElse(imports(t.stats))(defined(t.stats))
            case t: g.Template => orElse(imports(t.body))(defined(t.self :: Nil)) match {
                case (_, g.NoSymbol) => member(t.symbol.owner.thisType, gname)
                case gbinding => gbinding
              }
            case t: g.PackageDef => orElse(imports(t.stats)) {
                val gpkg = t.pid.symbol.moduleClass
                val gbinding = member(gpkg.thisType, gname)
                if (gbinding._2.exists) gbinding else member(gpkg.packageObject.info, gname)
              }
            case t: g.ClassDef => defined(t.tparams)
            case t: g.DefDef => defined(t.tparams ++ t.vparamss.flatten)
            case t: g.Function => defined(t.vparams)
            case t: g.CaseDef => defined(t.pat.collect { case b: g.Bind => b })
            case _ => noBinding
          }
        }
        // the binding of a name by an import: NoSymbol if none, null if unsure
        private def importBinding(gname: g.Name, gimport: g.Import): (g.Type, g.Symbol) = {
          val gtpe = gimport.expr.tpe
          if (gtpe == null) return null
          // selectors have term names, for a type too
          val gterm = gname.toTermName
          gimport.selectors.find(_.rename == gterm) match {
            case Some(sel) =>
              member(gtpe, if (gname.isTypeName) sel.name.toTypeName else sel.name.toTermName)
            // a selector that renames the name hides it from the wildcard
            case _ if !gimport.selectors.exists(_.name == g.nme.WILDCARD) => noBinding
            case _ if gimport.selectors.exists(_.name == gterm) => noBinding
            case _ => member(gtpe, gname)
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
        @tailrec
        private def tryFindParents(
            mtpe: m.Type,
            gparents: List[g.Type], // scalac flattens `A with B with C` into one list
        ): Unit = (mtpe, gparents) match {
          case (_, gparent :: Nil) => tryFindType(mtpe, gparent) // `(A with B)` is kept whole
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

        private def tryFindDecls( // find symbols for untyped declarations (e.g. `forSome` or a refinement)
            mstats: List[m.Stat],
            gsyms: Seq[g.Symbol],
        ): Unit = mstats.foreach {
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
                val gevidence = new mutable.ListBuffer[g.Symbol]
                val gparams = gsym.paramss.iterator.flatten.filter { gp =>
                  val synthetic = gp.isSynthetic
                  if (synthetic) gevidence += gp
                  !synthetic
                }
                mgroup.paramClauses.iterator.flatMap(_.values).zip(gparams).foreach {
                  case (mp, gp) => mp.decltpe.foreach(tryFindTermDecl(mp.name, _, gp))
                }
                while (gparams.hasNext) gparams.next() // drain
                defineEvidence(mtparams, gevidence, findTypes = true)
              }
            }
          case _ =>
        }
        private def tryFindPatDecls(mpats: List[m.Pat], mtpe: m.Type, gsyms: Seq[g.Symbol]): Unit =
          mpats.foreach {
            case x: m.Pat.Var => findByName(gsyms, x.name).foreach(tryFindTermDecl(x.name, mtpe, _))
            case _ =>
          }
        // the evidence comes first in the last parameter list of `gmethod`, defined at `mpoint`
        private def defineEvidence(gmethod: g.Symbol, mpoint: Int): Unit = {
          val mtparams = mnameAt(mpoint).parentOrNull match {
            case t: m.Tree.WithParamClauseGroups => t.paramClauseGroups
                .flatMap(_.tparamClause.values)
            case t: m.Tree.WithParamClauseGroup => t.paramClauseGroup
                .fold(List.empty[m.Type.Param])(_.tparamClause.values)
            case t: m.Tree.WithTParamClause => t.tparamClause.values
            case _ => Nil
          }
          if (mtparams.nonEmpty) gmethod.paramss.lastOption
            .foreach(defineEvidence(mtparams, _, findTypes = false))
        }
        // scalac types `A <% B` as `A => B` and `A: B` as `B[A]`, in the order of the bounds
        private def defineEvidence(
            mtparams: Iterable[m.Type.Param],
            gparams: Iterable[g.Symbol],
            findTypes: Boolean,
        ): Unit = {
          /* stop at a parameter that scalac did not add for a bound
           * (NB: the parameters of a constructor lose their `SYNTHETIC`) */
          val gps = gparams.iterator
            .takeWhile(x => x.isImplicit && (x.isSynthetic || x.owner.isConstructor))
          val mtps = mtparams.iterator
          while (mtps.hasNext && gps.hasNext) {
            val mbounds = mtps.next().bounds
            mbounds.view.iterator.zip(gps).foreach { case (mtpe, gp) =>
              if (findTypes) gp.info.typeArgs.lastOption.foreach(tryFindType(mtpe, _))
              defineEvidence(mtpe, gp)
            }
            mbounds.context.iterator.zip(gps).foreach { case (mtpe, gp) =>
              if (findTypes) tryFindType(mtpe, gp.info)
              defineEvidence(mtpe, gp)
            }
          }
        }
        // the source writes no name for the evidence, so define it at the start of its bound
        private def defineEvidence(mtpe: m.Type, gparam: g.Symbol): Unit = {
          val ssym = gparam.toSemantic
          if (ssym == Symbols.None) return
          // the field of a class parameter shares its definition, as in `withCtorParam`
          val fields = gparam.owner match {
            case gpo if gpo.isPrimaryConstructor =>
              val res = List.newBuilder[String]
              gpo.owner.info.decl(gparam.name).alternatives.foreach(x =>
                if (x.isParamAccessor && !x.isMethod && !x.isUselessOccurrence) {
                  val field = x.toSemantic
                  if (field != Symbols.None && field != ssym) res += field
                },
              )
              res.result()
            case _ => Nil
          }
          val sdef = if (fields.isEmpty) ssym else Symbols.Multi(ssym :: fields)
          addOccurrenceFromSemantic(mtpe.pos.trunc(0), sdef, Role.DEFINITION)
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

        @tailrec
        private def tryFindMacroAnnotation( // scalac keeps no tree of a macro annotation: resolve its name in the scope around it
            mtpe: m.Type,
            gscope: List[g.Tree],
            start: Int,
        ): Unit = mtpe match {
          case mtpe: m.Type.Apply => tryFindMacroAnnotation(mtpe.tpe, gscope, start)
          case _: m.Type.Name | _: m.Type.Select => resolveTypePath(mtpe, gscope, start) match {
              case (_, (mname, gann) :: gquals) if gann.isClass && gann.hasFlag(gf.MACRO) =>
                successes(gquals)
                success(mname, gann)
              case _ =>
            }
          case _ =>
        }

        private def processMemberDef(gtree: g.MemberDef) = {
          // a macro annotation puts its expansion at the offset of the annotation
          if (gtree.isInstanceOf[g.ImplDef]) {
            val gpos = gtree.pos
            if (gpos.isDefined && !gpos.isRange) mannotteeAt(gpos.point) match {
              case t: m.Member => mexpansions(gpos.point) = t
              case _ =>
            }
          }
          traverseAnnotations(gtree)
          tryFindMtree(gtree)
        }

        override protected def enter(gtree: g.Tree): Unit = genclosing = gtree :: genclosing
        override protected def leave(gtree: g.Tree): Unit = {
          genclosing = genclosing.drop(1)
          gtree match {
            case t: g.Template => ggetterTrees -= t
            case _ =>
          }
        }

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

        // the `map`, `flatMap`, `foreach` or `withFilter` that scalac calls for a `for`, or null
        @tailrec
        private def forMethod(gfun: g.Tree): g.Select = gfun match {
          case t: g.Select => if (hasFor(t)) t else null
          case t: g.TypeApply => forMethod(t.fun)
          case _ => null
        }

        private def traverseNoOriginal(gtree: g.Tree): Unit = gtree match {
          // skip an annotation that scalac added, like `@unchecked` on a desugared match
          case t: g.Annotated if !t.annot.pos.isOpaqueRange => isVisited += t.annot

          /* skip a setter but for its annotations: only the setter has `@(a @setter)`.
           * We find the name through the getter, which has the same position. */
          case t: g.DefDef if t.symbol.isSetter =>
            traverseAnnotations(t)
            isVisited ++= t.vparamss.flatten
            isVisited += t.rhs

          case t: g.Literal if t.tpe != null && t.tpe.typeSymbol == g.definitions.ClassClass =>
            val mpos = t.toMeta
            val mposClassOf = stripClassOf(mpos)
            if (mpos ne mposClassOf) {
              addOccurrenceFromSemantic(mposClassOf, ks.PredefClassOf, Role.REFERENCE)
              // scalac replaces `classOf[T]` with a constant of type T
              mclassOfAt(mpos.start) match {
                case mtpe: m.Type => tryFindType(mtpe, t.value.typeValue)
                case _ =>
              }
            } else tryFindMtree(t)

          case t: g.Function =>
            val gfor = genclosing match {
              case _ :: (gapply: g.Apply) :: _ => forMethod(gapply.fun)
              case _ => null
            }
            addSamOccurrence(t, gfor ne null)
            // the parameter of a guard repeats that of its generator, which defines the `_`
            val isGuard = (gfor ne null) && gfor.name == g.nme.withFilter
            // a for matches the parameter of its closure on the pattern
            val gpat = t.body match {
              case g.Match(_, cd :: Nil) => cd.pat
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
              else if (isVisited.add(p) && (findParamUnderscore(p) ne null)) saveUnderscore(p.symbol),
            )
            t.body match {
              /* skip the match of a pattern on the parameter.
               * Its position can be the name after `yield`. */
              case g.Match(sel: g.Typed, cases) if t.vparams.exists(_.symbol == sel.expr.symbol) =>
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
            if (gtree.pos.isDefined) mstartOf[m.Type.Existential](gtree.pos.start) {
              case t: m.Type.Existential => t
              case _ => null
            } match {
              case mtpe: m.Type.Existential =>
                tryFindDecls(mtpe.stats, gtree.whereClauses.map(_.symbol))
              case _ =>
            }
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
                mparentAt(p.pos.start) match {
                  case mtpe: m.Type => if (!tryFindParent(mtpe, p.tpe)) mtpe match {
                      case mtpe: m.Type.Select => success(mtpe.name, p.tpe.typeSymbol)
                      case _ => tryFindType(mtpe, p.tpe)
                    }
                  case _ =>
                }
              case _ =>
            }
            t.self.tpt match {
              case tpt: g.TypeTree if tpt.original != null && tpt.original.pos.isDefined =>
                // the tree of `_` in `this: List[_] =>` defines it, not the type below
                traverse(t.self)
                // scalac types `this: T =>` in class C as `C with T`
                val gtpe = tpt.tpe match {
                  case g.RefinedType(gcls :: gtpe :: Nil, _) if gcls.typeSymbol == t.symbol.owner =>
                    gtpe
                  case gtpe => gtpe
                }
                mselfTypeAt(tpt.original.pos.start) match {
                  case mtpe: m.Type => tryFindType(mtpe, gtpe)
                  case _ =>
                }
              case _ =>
            }
            tryFindMtree(t)
          case _ => tryFindMtree(gtree)
        }
      }

      MyTreeIndexerTraverser(msource)
      MyDocumentTraverser.traverse(unit.body)

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

        private def getApplyImplicitView(gt: g.ApplyImplicitView, range: s.Range): s.ApplyTree =
          getApplyImplicitView(gt, range.toSemanticOriginal)

        private def getApplyImplicitView(gt: g.ApplyImplicitView, arg: s.Tree): s.ApplyTree = {
          val fun = gt.fun match {
            // a method of a value, like `f.apply`, does not identify the conversion by itself
            case f: g.Select
                if f.qualifier.symbol != null && !f.qualifier.symbol.isModule &&
                  !f.qualifier.symbol.hasPackageFlag => f.toSemanticQualifierTree
            case f => f.toSemanticTree
          }
          s.ApplyTree(fun, List(arg))
        }

        private def getApplyToImplicitArgs(gt: g.ApplyToImplicitArgs, fun: s.Tree): s.ApplyTree = s
          .ApplyTree(fun, gt.args.map(_.toSemanticTree))

        private def getApplyToImplicitArgs(gt: g.ApplyToImplicitArgs): s.ApplyTree = {
          val fun = gt.fun match {
            case f: g.Apply => cachedForApplyOrOrig(f)
            case f => uncachedOrig(f)
          }
          getApplyToImplicitArgs(gt, fun)
        }

        /* A call that a `for` adds shows in the synthetic, with its view and implicit arguments.
         * Quote a source expression as `orig`, so its synthetics stay ranged as in written code. */
        private def cachedForOrOrig(gt: g.Tree): s.Tree = gt match {
          case t: g.ApplyToImplicitArgs => cachedForImplicitArgsOrOrig(t)
          case t: g.ApplyImplicitView => cachedForImplicitViewOrOrig(t)
          case t: g.Apply if isForFun(t.fun) && (!isSourceFor(t.pos) || isRefutabilityCheck(t)) =>
            cachedForApplyOrOrig(t)
          case _ => uncachedOrig(gt) // uncached: the traverser builds its own synthetic
        }
        private def cachedForApplyOrOrig(gt: g.Apply): s.Tree =
          cachedOrOrig(gt)(forApply(gt, usePos = false))
        // build a view of a desugared call inside, and leave any other view ranged
        private def cachedForImplicitViewOrOrig(gt: g.ApplyImplicitView): s.Tree = gt.args match {
          case arg :: Nil =>
            val res = cachedForOrOrig(arg)
            if (isForCall(arg)) cachedOrOrig(gt)(syn(getApplyImplicitView(gt, res))) else res
          case _ => uncachedOrig(gt) // uncached, as above
        }
        private def cachedForImplicitArgsOrOrig(gt: g.ApplyToImplicitArgs): s.Tree = gt.fun match {
          case f: g.ApplyImplicitView =>
            val res = cachedForImplicitViewOrOrig(f)
            if (isForCall(f)) cachedOrOrig(gt)(syn(getApplyToImplicitArgs(gt, res))) else res
          // hide the implicit arguments of the check too
          case f: g.Apply if isRefutabilityCheck(f) =>
            cachedOrOrig(gt)(cached(f)(forRefutabilityCheckFun(f.fun)))
          case f: g.Apply if isForFun(f.fun) && !isSourceFor(f.pos) =>
            cachedOrOrig(gt)(syn(getApplyToImplicitArgs(gt)))
          case _ => uncachedOrig(gt) // uncached, as above
        }

        // a call that a `for` adds, except the check and a source `for`, or a view of such a call
        @tailrec
        private def isForCall(gt: g.Tree): Boolean = gt match {
          case t: g.ApplyImplicitView => t.args match {
              case arg :: Nil => isForCall(arg)
              case _ => false
            }
          case t: g.ApplyToImplicitArgs => isForCall(t.fun)
          case t: g.Apply => isForFun(t.fun) && !isSourceFor(t.pos) && !isRefutabilityCheck(t)
          case _ => false
        }

        // the outermost call of a `for` has the range of the `for`
        private def isSourceFor(gpos: gu.Position): Boolean = gpos.isRange &&
          (mstartOf[m.Term.ForClause](gpos.start) {
            case t: m.Term.ForClause => t
            case _ => null
          } match {
            case t: m.Term.ForClause => t.pos.end == gpos.end
            case _ => false
          })

        // use it when we don't want to output the original in `synthetics`
        private def uncachedOrig(gt: g.Tree): s.Tree = gt.toSemanticOriginal
        private def cachedOrOrig(gt: g.Tree)(res: => Option[s.Synthetic]) = cached(gt)(res)
          .fold(uncachedOrig(gt))(_.tree)

        private def forSingleFuncArgApply(
            gt: g.Apply,
            farg: g.Function,
            usePos: Boolean,
        ): Option[s.Synthetic] = cachedForApplyFun(gt.fun).filter(_.range.isEmpty).flatMap { x =>
          // a pattern binds through a match on the parameter; its range spans the generator
          val body = farg.body match {
            case g.Match(sel: g.Typed, cd :: Nil)
                if farg.vparams.exists(_.symbol == sel.expr.symbol) => cd.body
            case body => body
          }
          val arg = s.FunctionTree(farg.vparams.map(toParamId), cachedForOrOrig(body))
          syn(gt.toRange, s.ApplyTree(x.tree, List(arg)), usePos)
        }

        @tailrec
        private def forRefutabilityCheckFun(gt: g.Tree): Option[s.Synthetic] = gt match {
          // step through the type arguments that scalac infers for a generic `withFilter`
          case gt: g.TypeApply =>
            cached(gt)(None)
            forRefutabilityCheckFun(gt.fun)
          // show the generator: the source does not write the check
          case gt: g.Select =>
            cached(gt)(None)
            syn(cachedForOrOrig(gt.qualifier))
          case _ => None
        }

        private def forApply(gt: g.Apply, usePos: Boolean = true): Option[s.Synthetic] =
          gt.args match {
            case (f: g.Function) :: Nil if isForFun(gt.fun) =>
              if (isRefutabilityCheck(f)) forRefutabilityCheckFun(gt.fun)
              else forSingleFuncArgApply(gt, f, usePos)
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
              (!gdecl.hasFlag(gf.EXISTENTIAL) || freeExistentials.contains(ssym)))
              saveSymbolFromSemantic(gdecl, ssym),
          )
        }
      }

      val finalOccurrences = {
        val buf = IndexedSeq.newBuilder[s.SymbolOccurrence]
        for {
          map <- Iterator(occurrences, mpatoccurrences, samoccurrences, sammethodoccurrences)
          (pos, (sym, role)) <- map
          flatSym <- sym.asMulti
        } buf += s.SymbolOccurrence(Some(pos.toRange), flatSym, role)
        buf.result()
      }

      s.TextDocument(
        schema = s.Schema.SEMANTICDB4,
        uri = unit.source.toUri,
        text = unit.source.toText,
        md5 = unit.source.toMD5,
        language = s.Language.SCALA,
        buildTarget = config.buildTarget,
        symbols = symbols.values.toIndexedSeq.sorted,
        occurrences = finalOccurrences.sorted,
        diagnostics = unit.hijackedDiagnostics.toDiagnostics(mstart).toIndexedSeq.sorted,
        synthetics = synthetics.toIndexedSeq.sorted,
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

  @tailrec
  private def isForFun(gt: g.Tree): Boolean = gt match {
    case t: g.TypeApply => isForFun(t.fun)
    case t: g.Select => t.pos == t.qualifier.pos && syntheticName(t.name)(true)(false)(false)
    case _ => false
  }

  // the check that scalac adds for a generator whose pattern can fail
  private def isRefutabilityCheck(gt: g.Function): Boolean = gt.vparams
    .exists(_.name.startsWith(g.nme.CHECK_IF_REFUTABLE_STRING))

  private def isRefutabilityCheck(gt: g.Apply): Boolean = gt.args match {
    case (f: g.Function) :: Nil => isRefutabilityCheck(f)
    case _ => false
  }

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

object TextDocumentOps {

  import s.SymbolOccurrence.Role
  type Occurrence = (String, Role)
  def emptyOccurrenceMap() = mutable.Map.empty[m.Position, Occurrence]

  val IsOldBehaviourOnPats = ScalaVersion(BuildInfo.scalaVersion) < ScalaVersion("2.13.17")

  // index the trees of a source in `mtrees`
  private[scalac] class MTreeIndexer(mtrees: TreeIndex) extends m.Traverser {
    // an empty tree may not nest with the trees at its offsets
    private def index(mtree: m.Tree): Unit = {
      val mpos = mtree.pos
      if (!mpos.isEmpty) {
        mtrees(mpos.start) = mtree
        mtrees(mpos.end - 1) = mtree
      }
    }
    protected def visit(mtree: m.Tree): Unit = {}
    override def apply(mtree: m.Tree): Unit = {
      index(mtree)
      mtree match {
        // NOTE: ignore mrename for now, we may decide to make it a binder
        case mtree: m.Importee.Rename => index(mtree.name)
        case _ =>
          visit(mtree)
          super.apply(mtree)
      }
    }
  }

  object TreeIndex {
    val empty = new TreeIndex(Array.empty[m.Tree])
    def apply(msource: m.Source): TreeIndex =
      new TreeIndex(new Array[m.Tree](msource.pos.input.chars.length))
  }

  /* offset -> the last non-empty tree that starts there or ends right past it. The others that
   * start or end there are its ancestors. */
  class TreeIndex(private val self: Array[m.Tree]) extends AnyVal {
    private def contains(offset: Int): Boolean = offset >= 0 && offset < self.length

    private[scalac] def update(offset: Int, value: m.Tree): Unit =
      if (contains(offset)) self(offset) = value

    def apply(offset: Int): m.Tree = if (contains(offset)) self(offset) else null

    private[scalac] def mstart(offset: Int): m.Tree = apply(offset) match {
      case t: m.Tree if t.pos.start == offset => t
      case _ => null
    }

    private[scalac] def mend(offset: Int): m.Tree = apply(offset - 1) match {
      case t: m.Tree if t.pos.end == offset => t
      case _ => null
    }
  }

  /* the outermost `Tree.WithPats` that contains `mtree` with only patterns between them, or
   * `owner` if there is none */
  @tailrec
  private[meta] def patOwner(mtree: m.Tree, owner: m.Tree.WithPats = null): m.Tree.WithPats =
    mtree.parent match {
      case Some(p: m.Tree.WithPats) => p match {
          case _: m.Pat | _: m.Pat.ArgClause => patOwner(p, p)
          case p: m.Tree.WithBody if p.body eq mtree => owner
          case _ => p
        }
      case Some(p: m.Pat) => patOwner(p, owner)
      case _ => owner
    }

  @tailrec // only a whole pattern, like `_` in `val _ =` or `_: Int <- xs`, has a symbol
  private def isWholePat(mpat: m.Pat): Boolean = mpat.parent match {
    case Some(p: m.Pat.Typed) => isWholePat(p)
    case Some(_: m.Pat | _: m.Pat.ArgClause | _: m.Case) => false
    case _ => true
  }

  // a pattern of a val or a var, like `a :: Nil` in `val a :: Nil = xs`
  private def isValPat(mpat: m.Pat): Boolean = mpat.parent match {
    case Some(p: m.Stat.WithMods with m.Tree.WithPats) => p.pats.contains(mpat)
    case _ => false
  }

  // the only variable of a pattern, or null
  private def onlyVar(mpat: m.Pat): m.Pat.Var = {
    var mvar: m.Pat.Var = null
    var count = 0
    def visit(mtree: m.Tree): Unit = if (count < 2) mtree match {
      case t: m.Pat.Var =>
        mvar = t
        count += 1
      case _: m.Pat | _: m.Pat.ArgClause => mtree.foreachChild(visit)
      case _ =>
    }
    visit(mpat)
    if (count == 1) mvar else null
  }

  /* the only variable of `Foo(a)` if scalac puts its getter at `Foo`. scalac does that for a
   * pattern in a val or in the arguments of another pattern. */
  private def extractVar(mpat: m.Pat.Extract): m.Pat.Var =
    if (isValPat(mpat) || mpat.argClause.values.lengthCompare(1) == 0 && (null ne patOwner(mpat)))
      onlyVar(mpat)
    else null

  // a variable in a pattern that is more than a lone variable
  private def isValPatName(mname: m.Name): Boolean = mname.parent match {
    case Some(p: m.Pat.Var) => patOwner(p) match {
        case null => false
        case t => t.pats match {
            case (_: m.Pat.Var) :: Nil => false
            case _ => true
          }
      }
    case _ => false
  }

  @tailrec // the qualifier of the first `private[q]` or `protected[q]`, or null
  private def withinOf(mods: List[m.Mod]): m.Name = mods match {
    case (t: m.Mod.WithWithin) :: rest => t.within match {
        case mname: m.Name.Indeterminate => mname
        case _ => withinOf(rest)
      }
    case _ :: rest => withinOf(rest)
    case _ => null
  }

  // the qualifier of the access of the definition `mname`, or null
  private def withinOf(mname: m.Name): m.Name = mname.parent match {
    case Some(p: m.Pat.Var) => patOwner(p) match {
        case t: m.Stat.WithMods => withinOf(t.mods)
        case _ => null
      }
    case Some(p: m.Member) if p.name eq mname =>
      p match {
        case t: m.Stat.WithMods => withinOf(t.mods)
        case t: m.Member.Param => withinOf(t.mods)
        case t: m.Type.FunctionParamOrArg => withinOf(t.mods)
        case _ => null
      }
    case _ => null
  }

  private implicit class ImplicitMetaTreeOps(val self: m.Tree) extends AnyVal {

    def containsIncl(beg: Int, endIncl: Int): Boolean = {
      val mpos = self.pos
      mpos.start <= beg && endIncl < mpos.end
    }
  }

}
