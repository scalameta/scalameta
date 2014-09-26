package scala
package tools.nsc
package typechecker
import scala.reflect.internal.util.{ BatchSourceFile, Statistics, shortClassOfInstance }
import scala.reflect.internal.Flags._
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.tools.nsc.`package`.ListOfNil
trait Typers extends Adaptations with Tags with TypersTracking with PatternTypers { self: Analyzer =>
  import global._
  import definitions._
  import TypersStats._
  final def forArgMode(fun: Tree, mode: Mode) = if (treeInfo.isSelfOrSuperConstrCall(fun)) mode | SCCmode else mode
  val transformed = new scala.collection.mutable.AnyRefMap[Tree, Tree]
  final val shortenImports = false
  def resetDocComments() = clearDocComments()
  def resetTyper(): scala.Unit = {
    resetContexts()
    resetImplicits()
    resetDocComments()
  }
  sealed abstract class SilentResult[+T] {
    abstract def isEmpty: Boolean
    def nonEmpty = !isEmpty
    @inline final def fold[U](none: => U)(f: T => U): U = this match {
      case SilentResultValue(value) =>
        f(value)
      case _ =>
        none
    }
    @inline final def map[U](f: T => U): SilentResult[U] = this match {
      case SilentResultValue(value) =>
        SilentResultValue(f(value))
      case x: SilentTypeError =>
        x
    }
    @inline final def filter(p: T => Boolean): SilentResult[T] = this match {
      case SilentResultValue(value) if !p(value) =>
        SilentTypeError(TypeErrorWrapper(new TypeError(NoPosition, "!p")))
      case _ =>
        this
    }
    @inline final def orElse[T1 >: T](f: Seq[AbsTypeError] => T1): T1 = this match {
      case SilentResultValue(value) =>
        value
      case s: SilentTypeError =>
        f(s.reportableErrors)
    }
  }
  class SilentTypeError private (val errors: List[AbsTypeError]) extends SilentResult[Nothing] {
    override def isEmpty = true
    def err: AbsTypeError = errors.head
    def reportableErrors = errors match {
      case (e1: AmbiguousImplicitTypeError) +: _ =>
        List(e1)
      case all =>
        all
    }
  }
  object SilentTypeError {
    def apply(errors: AbsTypeError*): SilentTypeError = new SilentTypeError(errors.toList)
    def unapply(error: SilentTypeError): Option[AbsTypeError] = error.errors.headOption
  }
  case class SilentResultValue[+T](value: T) extends SilentResult[T] { override def isEmpty = false }
  def newTyper(context: Context): Typer = new NormalTyper(context)
  private class NormalTyper(context: Context) extends Typer(context)
  private final val SYNTHETIC_PRIVATE = 274877906944
  private final val InterpolatorCodeRegex = "\$\{.*?\}".r
  private final val InterpolatorIdentRegex = "\$[$\w]+".r
  abstract class Typer(context0: Context) extends TyperDiagnostics with Adaptation with Tag with PatternTyper with TyperContextErrors {
    import context0.unit
    import typeDebug.{ ptTree, ptBlock, ptLine, inGreen, inRed }
    import TyperErrorGen._
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._
    private val transformed: scala.collection.mutable.Map[Tree, Tree] = unit.transformed
    val infer = new Inferencer {
      def context = Typer.this.context
      override def isCoercible(tp: Type, pt: Type) = undoLog.undo(viewExists(tp, pt))
    }
    def canAdaptConstantTypeToLiteral = true
    def canTranslateEmptyListToNil = true
    def missingSelectErrorTree(tree: Tree, qual: Tree, name: Name): Tree = tree
    def typedDocDef(docDef: DocDef, mode: Mode, pt: Type): Tree = typed(docDef.definition, mode, pt)
    def applyImplicitArgs(fun: Tree): Tree = fun.tpe match {
      case MethodType(params, _) =>
        val argResultsBuff = new scala.collection.mutable.ListBuffer[SearchResult];
        val argBuff = new scala.collection.mutable.ListBuffer[Tree];
        var paramFailed = false;
        var mkArg: (Name, Tree) => Tree = tree;
        params.foreach(param => {
          var paramTp = param.tpe
          argResultsBuff.foreach(ar => paramTp = paramTp.subst(ar.subst.from, ar.subst.to))
          val res = if (paramFailed || paramTp.isError && {
            paramFailed = true
            true
          }) SearchFailure else inferImplicit(fun, paramTp, context.reportErrors, isView = false, context)
          argResultsBuff += res
          if (res.isSuccess) argBuff += mkArg(param.name, res.tree) else {
            mkArg = {
              (name, tree) => gen.mkNamedArg(name, tree)
            }
            if (!param.hasDefault && !paramFailed) {
              context.reportBuffer.errors.collectFirst({
                case dte: DivergentImplicitTypeError =>
                  dte
              }) match {
                case Some(divergent) =>
                  if (context.reportErrors) {
                    context.issue(divergent.withPt(paramTp))
                    context.reportBuffer.clearErrors({
                      case dte: DivergentImplicitTypeError =>
                        true
                    })
                  }
                case _ =>
                  NoImplicitFoundError(fun, param)
              }
              paramFailed = true
            }
          }
        });
        val args = argBuff.toList;
        argResultsBuff.foreach(ar => {
          ar.subst.traverse(fun)
          args.foreach(arg => ar.subst.traverse(arg))
        });
        new ApplyToImplicitArgs(fun, args).setPos(fun.pos)
      case ErrorType =>
        fun
    }
    def viewExists(from: Type, to: Type): Boolean = !from.isError && !to.isError && context.implicitsEnabled && inferView(context.tree, from, to, reportAmbiguous = false, saveErrors = true) != EmptyTree
    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean): Tree = inferView(tree, from, to, reportAmbiguous, saveErrors = true)
    def inferView(tree: Tree, from: Type, to: Type, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      debuglog("infer view from " + from + " to " + to)
      if (isPastTyper) EmptyTree else from match {
        case MethodType(_, _) =>
          EmptyTree
        case OverloadedType(_, _) =>
          EmptyTree
        case PolyType(_, _) =>
          EmptyTree
        case _ =>
          def wrapImplicit(from: Type): Tree = {
            val result = inferImplicit(tree, functionType(from.withoutAnnotations :: Nil, to), reportAmbiguous, isView = true, context, saveAmbiguousDivergent = saveErrors)
            if (result.subst != EmptyTreeTypeSubstituter) {
              result.subst.traverse(tree)
              notifyUndetparamsInferred(result.subst.from, result.subst.to)
            }
            result.tree
          };
          wrapImplicit(from).orElse(wrapImplicit(byNameType(from)))
      }
    }
    import infer._
    private var namerCache: Namer = null
    def namer = {
      if (namerCache.eq(null) || namerCache.context != context) namerCache = newNamer(context)
      namerCache
    }
    var context = context0
    def context1 = context
    def dropExistential(tp: Type): Type = tp match {
      case ExistentialType(tparams, tpe) =>
        new SubstWildcardMap(tparams)(tp)
      case TypeRef(_, sym, _) if sym.isAliasType =>
        val tp0 = tp.dealias;
        if (tp.eq(tp0)) {
          debugwarn(s"dropExistential did not progress dealiasing $tp, see SI-7126")
          tp
        } else {
          val tp1 = dropExistential(tp0)
          if (tp1.eq(tp0)) tp else tp1
        }
      case _ =>
        tp
    }
    private def errorNotClass(tpt: Tree, found: Type) = {
      ClassTypeRequiredError(tpt, found)
      false
    }
    private def errorNotStable(tpt: Tree, found: Type) = {
      TypeNotAStablePrefixError(tpt, found)
      false
    }
    def checkClassType(tpt: Tree): Boolean = {
      val tpe = unwrapToClass(tpt.tpe)
      isNonRefinementClassType(tpe) || errorNotClass(tpt, tpe)
    }
    def checkStablePrefixClassType(tpt: Tree): Boolean = {
      val tpe = unwrapToStableClass(tpt.tpe)
      def prefixIsStable = {
        def checkPre = tpe match {
          case TypeRef(pre, _, _) =>
            pre.isStable || errorNotStable(tpt, pre)
          case _ =>
            false
        }
        def checkTree = tpt match {
          case SelectFromTypeTree(qual, _) =>
            isSingleType(qual.tpe) || errorNotClass(tpt, tpe)
          case _ =>
            true
        }
        checkPre && checkTree
      }
      (isNonRefinementClassType(tpe) || errorNotClass(tpt, tpe)) && (isPastTyper || prefixIsStable)
    }
    def checkNonCyclic(pos: Position, tp: Type): Boolean = {
      def checkNotLocked(sym: Symbol) = sym.initialize.lockOK || {
        CyclicAliasingOrSubtypingError(pos, sym)
        false
      }
      tp match {
        case TypeRef(pre, sym, args) =>
          checkNotLocked(sym) && (!sym.isNonClassType || checkNonCyclic(pos, appliedType(pre.memberInfo(sym), args), sym))
        case SingleType(pre, sym) =>
          checkNotLocked(sym)
        case st: SubType =>
          checkNonCyclic(pos, st.supertype)
        case ct: CompoundType =>
          ct.parents.forall(x => checkNonCyclic(pos, x))
        case _ =>
          true
      }
    }
    def checkNonCyclic(pos: Position, tp: Type, lockedSym: Symbol): Boolean = try if (!lockedSym.lock(CyclicReferenceError(pos, tp, lockedSym))) false else checkNonCyclic(pos, tp) finally lockedSym.unlock()
    def checkNonCyclic(sym: Symbol): scala.Unit = if (!checkNonCyclic(sym.pos, sym.tpe_*)) {
      sym.setInfo(ErrorType)
      ()
    }
    def checkNonCyclic(defn: Tree, tpt: Tree): scala.Unit = if (!checkNonCyclic(defn.pos, tpt.tpe, defn.symbol)) {
      tpt.setType(ErrorType)
      {
        defn.symbol.setInfo(ErrorType)
        ()
      }
    }
    def checkParamsConvertible(tree: Tree, tpe0: Type): scala.Unit = {
      def checkParamsConvertible0(tpe: Type) = tpe match {
        case MethodType(formals, restpe) =>
          if (tpe.isDependentMethodType) DependentMethodTpeConversionToFunctionError(tree, tpe);
          checkParamsConvertible(tree, restpe)
        case _ =>
      }
      checkParamsConvertible0(tpe0)
    }
    object checkNoEscaping extends TypeMap {
      private var owner: Symbol = _
      private var scope: Scope = _
      private var hiddenSymbols: List[Symbol] = _
      def privates[T <: Tree](owner: Symbol, tree: T): T = check(owner, EmptyScope, WildcardType, tree)
      private def check[T <: Tree](owner: Symbol, scope: Scope, pt: Type, tree: T): T = {
        this.owner = owner
        this.scope = scope
        hiddenSymbols = List()
        val tp1 = apply(tree.tpe)
        if (hiddenSymbols.isEmpty) tree.setType(tp1) else if (hiddenSymbols.exists(_.isErroneous)) HiddenSymbolWithError(tree) else if (isFullyDefined(pt)) tree.setType(pt) else if (tp1.typeSymbol.isAnonymousClass) check(owner, scope, pt, tree.setType(tp1.typeSymbol.classBound)) else if (owner == NoSymbol) tree.setType(packSymbols(hiddenSymbols.reverse, tp1)) else if (!isPastTyper) {
          val badSymbol = hiddenSymbols.head
          SymbolEscapesScopeError(tree, badSymbol)
        } else tree
      }
      def addHidden(sym: Symbol) = if (!hiddenSymbols.contains(sym)) hiddenSymbols = sym :: hiddenSymbols
      override def apply(t: Type): Type = {
        def checkNoEscape(sym: Symbol): scala.Unit = if (sym.isPrivate && !sym.hasFlag(274877906944)) {
          var o = owner
          while (o != NoSymbol && o != sym.owner && o != sym.owner.linkedClassOfClass && !o.isLocalToBlock && !o.isPrivate && !o.privateWithin.hasTransOwner(sym.owner)) o = o.owner
          if (o == sym.owner || o == sym.owner.linkedClassOfClass) addHidden(sym)
        } else if (sym.owner.isTerm && !sym.isTypeParameterOrSkolem) {
          var e = scope.lookupEntry(sym.name)
          var found = false
          while (!found && e.ne(null) && e.owner == scope) if (e.sym == sym) {
            found = true
            addHidden(sym)
          } else e = scope.lookupNextEntry(e)
        }
        mapOver(t match {
          case TypeRef(_, sym, args) =>
            checkNoEscape(sym);
            if (!hiddenSymbols.isEmpty && hiddenSymbols.head == sym && sym.isAliasType && sameLength(sym.typeParams, args)) {
              hiddenSymbols = hiddenSymbols.tail
              t.dealias
            } else t
          case SingleType(_, sym) =>
            checkNoEscape(sym);
            t
          case _ =>
            t
        })
      }
    }
    def reenterValueParams(vparamss: List[List[ValDef]]): scala.Unit = vparamss.foreach(vparams => vparams.foreach(vparam => context.scope.enter(vparam.symbol)))
    def reenterTypeParams(tparams: List[TypeDef]): List[Symbol] = tparams.map(tparam => {
      context.scope.enter(tparam.symbol)
      tparam.symbol.deSkolemize
    })
    def qualifyingClass(tree: Tree, qual: Name, packageOK: Boolean) = context.enclClass.owner.ownerChain.find(o => qual.isEmpty || o.isClass && o.name == qual) match {
      case Some(c) if packageOK || !c.isPackageClass =>
        c
      case _ =>
        QualifyingClassError(tree, qual);
        NoSymbol
    }
    final def constrTyperIf(inConstr: Boolean): Typer = if (inConstr) {
      assert(context.undetparams.isEmpty, context.undetparams)
      newTyper(context.makeConstructorContext)
    } else this
    @inline final def withCondConstrTyper[T](inConstr: Boolean)(f: Typer => T): T = if (inConstr) {
      assert(context.undetparams.isEmpty, context.undetparams)
      val c = context.makeConstructorContext
      typerWithLocalContext(c)(f)
    } else f(this)
    @inline final def typerWithCondLocalContext[T](c: => Context)(cond: Boolean)(f: Typer => T): T = if (cond) typerWithLocalContext(c)(f) else f(this)
    @inline final def typerWithLocalContext[T](c: Context)(f: Typer => T): T = {
      val res = f(newTyper(c))
      if (c.hasErrors) context.updateBuffer(c.flushAndReturnBuffer())
      res
    }
    @inline final def withSavedContext[T](c: Context)(f: => T) = {
      val savedErrors = c.flushAndReturnBuffer()
      val res = f
      c.updateBuffer(savedErrors)
      res
    }
    def labelTyper(ldef: LabelDef): Typer = if (ldef.symbol == NoSymbol) {
      val typer1 = newTyper(context.makeNewScope(ldef, context.owner))
      typer1.enterLabelDef(ldef)
      typer1
    } else this
    def reallyExists(sym: Symbol) = {
      if (isStale(sym)) sym.setInfo(NoType)
      sym.exists
    }
    def isStale(sym: Symbol): Boolean = sym.rawInfo.isInstanceOf[loaders.ClassfileLoader] && {
      sym.rawInfo.load(sym)
      sym.sourceFile.ne(null) && currentRun.compiledFiles.contains(sym.sourceFile.path)
    }
    private def isStableContext(tree: Tree, mode: Mode, pt: Type) = {
      def ptSym = pt.typeSymbol
      def expectsStable = pt.isStable || mode.inQualMode && !tree.symbol.isConstant || !(tree.tpe <:< pt) && (ptSym.isAbstractType && pt.bounds.lo.isStable || ptSym.isRefinementClass)
      isNarrowable(tree.tpe) && mode.typingExprNotLhs && expectsStable
    }
    private def makeAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): (Tree, Type) = if (context.isInPackageObject(sym, pre.typeSymbol)) {
      if (pre.typeSymbol == ScalaPackageClass && sym.isTerm) {
        def dealias(sym: Symbol) = (atPos(tree.pos.makeTransparent)(gen.mkAttributedRef(sym)).setPos(tree.pos), sym.owner.thisType)
        sym.name match {
          case nme.List =>
            return dealias(ListModule)
          case nme.Seq =>
            return dealias(SeqModule)
          case nme.Nil =>
            return dealias(NilModule)
          case _ =>
        }
      }
      val qual = typedQualifier(atPos(tree.pos.makeTransparent)(tree match {
        case Ident(_) =>
          Ident(rootMirror.getPackageObjectWithMember(pre, sym))
        case Select(qual, _) =>
          Select(qual, nme.PACKAGEkw)
        case SelectFromTypeTree(qual, _) =>
          Select(qual, nme.PACKAGEkw)
      }))
      val tree1 = atPos(tree.pos)(tree match {
        case Ident(name) =>
          Select(qual, name)
        case Select(_, name) =>
          Select(qual, name)
        case SelectFromTypeTree(_, name) =>
          SelectFromTypeTree(qual, name)
      })
      (checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe)
    } else (checkAccessible(tree, sym, pre, site), pre)
    protected def stabilize(tree: Tree, pre: Type, mode: Mode, pt: Type): Tree = {
      if (tree.symbol.isOverloaded && !mode.inFunMode) inferExprAlternative(tree, pt)
      val sym = tree.symbol
      val isStableIdPattern = mode.typingPatternNotConstructor && tree.isTerm
      def isModuleTypedExpr = treeInfo.admitsTypeSelection(tree) && (isStableContext(tree, mode, pt) || sym.isModuleNotMethod)
      def isStableValueRequired = isStableIdPattern || mode.in(all = EXPRmode, none = QUALmode) && !phase.erasedTypes
      def isGetClassCall = isGetClass(sym) && pre.typeSymbol.isPublic
      def narrowIf(tree: Tree, condition: Boolean) = if (condition) tree.setType(singleType(pre, sym)) else tree
      def checkStable(tree: Tree): Tree = if (treeInfo.isStableIdentifierPattern(tree)) tree else UnstableTreeError(tree)
      if (tree.isErrorTyped) tree else if (!sym.isValue && isStableValueRequired) NotAValueError(tree, sym) else if (isStableIdPattern) narrowIf(checkStable(tree), sym.isModuleNotMethod) else if (isModuleTypedExpr) narrowIf(tree, true) else if (isGetClassCall) tree.setType(MethodType(Nil, getClassReturnType(pre))) else tree
    }
    private def isNarrowable(tpe: Type): Boolean = unwrapWrapperTypes(tpe) match {
      case TypeRef(_, _, _) | RefinedType(_, _) =>
        true
      case _ =>
        !phase.erasedTypes
    }
    def stabilizeFun(tree: Tree, mode: Mode, pt: Type): Tree = {
      val sym = tree.symbol
      val pre = tree match {
        case Select(qual, _) =>
          qual.tpe
        case _ =>
          NoPrefix
      }
      def stabilizable = pre.isStable && sym.tpe.params.isEmpty && (isStableContext(tree, mode, pt) || sym.isModule)
      tree.tpe match {
        case MethodType(_, _) if stabilizable =>
          tree.setType(MethodType(Nil, singleType(pre, sym)))
        case _ =>
          tree
      }
    }
    def member(qual: Tree, name: Name) = {
      def callSiteWithinClass(clazz: Symbol) = context.enclClass.owner.hasTransOwner(clazz)
      val includeLocals = qual.tpe match {
        case ThisType(clazz) if callSiteWithinClass(clazz) =>
          true
        case SuperType(clazz, _) if callSiteWithinClass(clazz.typeSymbol) =>
          true
        case _ =>
          phase.next.erasedTypes
      }
      if (includeLocals) qual.tpe.member(name) else qual.tpe.nonLocalMember(name)
    }
    def silent[T](op: Typer => T, reportAmbiguousErrors: Boolean = context.ambiguousErrors, newtree: Tree = context.tree): SilentResult[T] = {
      val rawTypeStart = if (Statistics.canEnable) Statistics.startCounter(rawTypeFailed) else null
      val findMemberStart = if (Statistics.canEnable) Statistics.startCounter(findMemberFailed) else null
      val subtypeStart = if (Statistics.canEnable) Statistics.startCounter(subtypeFailed) else null
      val failedSilentStart = if (Statistics.canEnable) Statistics.startTimer(failedSilentNanos) else null
      def stopStats() = {
        if (Statistics.canEnable) Statistics.stopCounter(rawTypeFailed, rawTypeStart)
        if (Statistics.canEnable) Statistics.stopCounter(findMemberFailed, findMemberStart)
        if (Statistics.canEnable) Statistics.stopCounter(subtypeFailed, subtypeStart)
        if (Statistics.canEnable) Statistics.stopTimer(failedSilentNanos, failedSilentStart)
      }
      try if (context.reportErrors || reportAmbiguousErrors != context.ambiguousErrors || newtree != context.tree) {
        val context1 = context.makeSilent(reportAmbiguousErrors, newtree)
        context1.undetparams = context.undetparams
        context1.savedTypeBounds = context.savedTypeBounds
        context1.namedApplyBlockInfo = context.namedApplyBlockInfo
        val typer1 = newTyper(context1)
        val result = op(typer1)
        context.undetparams = context1.undetparams
        context.savedTypeBounds = context1.savedTypeBounds
        context.namedApplyBlockInfo = context1.namedApplyBlockInfo
        if (context1.hasErrors) {
          stopStats()
          SilentTypeError(context1.errors: _*)
        } else {
          context1.flushAndIssueWarnings()
          SilentResultValue(result)
        }
      } else {
        assert(context.bufferErrors || isPastTyper, "silent mode is not available past typer")
        withSavedContext(context) {
          val res = op(this)
          val errorsToReport = context.flushAndReturnBuffer()
          if (errorsToReport.isEmpty) SilentResultValue(res) else SilentTypeError(errorsToReport.head)
        }
      } catch {
        case ex: CyclicReference =>
          throw ex
        case ex: TypeError =>
          stopStats();
          SilentTypeError(TypeErrorWrapper(ex))
      }
    }
    def checkFeature(pos: Position, featureTrait: Symbol, construct: => String = "", immediate: Boolean = false): Boolean = if (isPastTyper) true else {
      val nestedOwners = featureTrait.owner.ownerChain.takeWhile(_ != languageFeatureModule.moduleClass).reverse
      val featureName = nestedOwners.map(_.name + ".").mkString + featureTrait.name
      def action(): Boolean = {
        def hasImport = inferImplicit(EmptyTree: Tree, featureTrait.tpe, reportAmbiguous = true, isView = false, context).isSuccess
        def hasOption = settings.language.contains(featureName)
        val OK = hasImport || hasOption
        if (!OK) {
          val Some(AnnotationInfo(_, List(Literal(Constant(featureDesc: String)), Literal(Constant(required: Boolean))), _)) = featureTrait.getAnnotation(LanguageFeatureAnnot)
          context.featureWarning(pos, featureName, featureDesc, featureTrait, construct, required)
        }
        OK
      }
      if (immediate) action() else {
        unit.toCheck += { () => 
          action()
          ()
        }
        true
      }
    }
    def checkExistentialsFeature(pos: Position, tpe: Type, prefix: String) = tpe match {
      case extp: ExistentialType if !extp.isRepresentableWithWildcards =>
        checkFeature(pos, ExistentialsFeature, prefix + " " + tpe)
      case _ =>
    }
    protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
      def hasUndets = context.undetparams.nonEmpty
      def hasUndetsInMonoMode = hasUndets && !mode.inPolyMode
      def adaptToImplicitMethod(mt: MethodType): Tree = {
        if (hasUndets) context.undetparams = inferExprInstance(tree, context.extractUndetparams(), pt, mt.approximate, keepNothings = false, useWeaklyCompatible = true)
        if (context.hasErrors) setError(tree) else withCondConstrTyper(treeInfo.isSelfOrSuperConstrCall(tree))(typer1 => if (original != EmptyTree && pt != WildcardType) typer1.silent(tpr => {
          val withImplicitArgs = tpr.applyImplicitArgs(tree)
          if (tpr.context.hasErrors) tree else tpr.typed(withImplicitArgs, mode, pt)
        }).orElse(_ => {
          val resetTree = resetAttrs(original)
          debuglog(s"fallback on implicits: $tree/$resetTree")
          val tree1 = typed(resetTree, mode)
          tree1.setType(pluginsTyped(tree1.tpe, this, tree1, mode, pt))
          if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt, EmptyTree)
        }) else typer1.typed(typer1.applyImplicitArgs(tree), mode, pt))
      }
      def instantiateToMethodType(mt: MethodType): Tree = {
        val meth = tree match {
          case Block(_, tree1) =>
            tree1.symbol
          case _ =>
            tree.symbol
        }
        if (!meth.isConstructor && isFunctionType(pt)) {
          debuglog(s"eta-expanding $tree: ${tree.tpe} to $pt")
          checkParamsConvertible(tree, tree.tpe)
          val tree0 = etaExpand(context.unit, tree, this)
          if (hasUndets) instantiate(typed(tree0, mode), mode, pt) else typed(tree0, mode, pt)
        } else if (!meth.isConstructor && mt.params.isEmpty) adapt(typed(Apply(tree, Nil).setPos(tree.pos)), mode, pt, original) else if (context.implicitsEnabled) MissingArgsForMethodTpeError(tree, meth) else setError(tree)
      }
      def adaptType(): Tree = {
        def properTypeRequired = tree.hasSymbolField && !context.inTypeConstructorAllowed && !(tree.symbol.isJavaDefined && context.unit.isJava)
        def kindArityMismatch = context.inTypeConstructorAllowed && !sameLength(tree.tpe.typeParams, pt.typeParams)
        def kindArityMismatchOk = tree.tpe.typeSymbol match {
          case NothingClass | AnyClass =>
            true
          case _ =>
            pt == WildcardType
        }
        if (mode.inFunMode) tree else if (properTypeRequired && tree.symbol.typeParams.nonEmpty) MissingTypeParametersError(tree) else if (kindArityMismatch && !kindArityMismatchOk) KindArityMismatchError(tree, pt) else tree match {
          case TypeTree() =>
            tree
          case _ =>
            TypeTree(tree.tpe).setOriginal(tree)
        }
      }
      def insertApply(): Tree = {
        assert(!context.inTypeConstructorAllowed, mode)
        val adapted = adaptToName(tree, nme)
        def stabilize0(pre: Type): Tree = stabilize(adapted, pre, MonoQualifierModes, WildcardType)
        val qual = adapted match {
          case This(_) =>
            gen.stabilize(adapted)
          case Ident(_) =>
            val owner = adapted.symbol.owner;
            val pre = if (owner.isPackageClass) owner.thisType else if (owner.isClass) context.enclosingSubClassContext(owner).prefix else NoPrefix;
            stabilize0(pre)
          case Select(qualqual, _) =>
            stabilize0(qualqual.tpe)
          case other =>
            other
        }
        typedPos(tree.pos, mode, pt)(Select(qual.setPos(tree.pos.makeTransparent), nme))
      }
      def adaptConstant(value: Constant): Tree = {
        val sym = tree.symbol
        if (sym != null && sym.isDeprecated) context.deprecationWarning(tree.pos, sym)
        treeCopy.Literal(tree, value)
      }
      def adaptMismatchedSkolems() = {
        def canIgnoreMismatch = !context.reportErrors && isPastTyper || tree.hasAttachment[MacroExpansionAttachment]
        def bound = pt match {
          case ExistentialType(qs, _) =>
            qs
          case _ =>
            Nil
        }
        def msg = sm"""
          |Recovering from existential or skolem type error in
          |  $tree
          |with type: ${tree.tpe}
          |       pt: $pt
          |  context: ${context.tree}
          |  adapted
          """.trim()
        val boundOrSkolems = if (canIgnoreMismatch) bound ++ pt.skolemsExceptMethodTypeParams else Nil
        boundOrSkolems match {
          case Nil =>
            AdaptTypeError(tree, tree.tpe, pt);
            setError(tree)
          case _ =>
            logResult(msg)(adapt(tree, mode, deriveTypeWithWildcards(boundOrSkolems)(pt)))
        }
      }
      def fallbackAfterVanillaAdapt(): Tree = {
        def isPopulatedPattern = {
          if (tree.symbol.ne(null) && tree.symbol.isModule) inferModulePattern(tree, pt)
          isPopulated(tree.tpe, approximateAbstracts(pt))
        }
        if (mode.inPatternMode && isPopulatedPattern) return tree
        val tree1 = constfold(tree, pt)
        if (tree1.tpe <:< pt) return adapt(tree1, mode, pt, original)
        if (mode.typingExprNotFun) {
          if (tree.tpe <:< AnyTpe) pt.dealias match {
            case TypeRef(_, UnitClass, _) =>
              if (settings.warnValueDiscard) context.warning(tree.pos, "discarded non-Unit value");
              return typedPos(tree.pos, mode, pt)(Block(List(tree), Literal(Constant(()))))
            case TypeRef(_, sym, _) if isNumericValueClass(sym) && isNumericSubType(tree.tpe, pt) =>
              if (settings.warnNumericWiden) context.warning(tree.pos, "implicit numeric widening");
              return typedPos(tree.pos, mode, pt)(Select(tree, "to" + sym.name))
            case _ =>
          }
          if (pt.dealias.annotations.nonEmpty && canAdaptAnnotations(tree, this, mode, pt)) return typed(adaptAnnotations(tree, this, mode, pt), mode, pt)
          if (hasUndets) return instantiate(tree, mode, pt)
          if (context.implicitsEnabled && !pt.isError && !tree.isErrorTyped) {
            debuglog("inferring view from " + tree.tpe + " to " + pt)
            inferView(tree, tree.tpe, pt, reportAmbiguous = true) match {
              case EmptyTree =>
              case coercion =>
                def msg = "inferred view from " + tree.tpe + " to " + pt + " = " + coercion + ":" + coercion.tpe;
                if (settings.logImplicitConv) context.echo(tree.pos, msg);
                debuglog(msg);
                val silentContext = context.makeImplicit(context.ambiguousErrors);
                val res = newTyper(silentContext).typed(new ApplyImplicitView(coercion, List(tree)).setPos(tree.pos), mode, pt);
                silentContext.firstError match {
                  case Some(err) =>
                    context.issue(err)
                  case None =>
                    return res
                }
            }
          }
        }
        debuglog("error tree = " + tree)
        if (settings.debug && settings.explaintypes) explainTypes(tree.tpe, pt)
        if (tree.tpe.isErroneous || pt.isErroneous) setError(tree) else adaptMismatchedSkolems()
      }
      def vanillaAdapt(tree: Tree) = {
        def applyPossible = {
          def applyMeth = member(adaptToName(tree, nme), nme)
          def hasPolymorphicApply = applyMeth.alternatives.exists(_.tpe.typeParams.nonEmpty)
          def hasMonomorphicApply = applyMeth.alternatives.exists(_.tpe.paramSectionCount > 0)
          dyna.acceptsApplyDynamic(tree.tpe) || if (mode.inTappMode) tree.tpe.typeParams.isEmpty && hasPolymorphicApply else hasMonomorphicApply
        }
        def shouldInsertApply(tree: Tree) = mode.typingExprFun && tree.tpe match {
          case _: MethodType | _: OverloadedType | _: PolyType =>
            false
          case _ =>
            applyPossible
        }
        if (tree.isType) adaptType() else if (mode.typingExprNotFun && treeInfo.isMacroApplication(tree) && !isMacroExpansionSuppressed(tree)) macroExpand(this, tree, mode, pt) else if (mode.typingConstructorPattern) typedConstructorPattern(tree, pt) else if (shouldInsertApply(tree)) insertApply() else if (hasUndetsInMonoMode) {
          assert(!context.inTypeConstructorAllowed, context)
          instantiatePossiblyExpectingUnit(tree, mode, pt)
        } else if (tree.tpe <:< pt) tree else fallbackAfterVanillaAdapt()
      }
      if (isMacroImplRef(tree)) if (treeInfo.isMacroApplication(tree)) adapt(unmarkMacroImplRef(tree), mode, pt, original) else tree else tree.tpe match {
        case atp @ AnnotatedType(_, _) if canAdaptAnnotations(tree, this, mode, pt) =>
          adaptAnnotations(tree, this, mode, pt)
        case ct @ ConstantType(value) if mode.inNone(TYPEmode | FUNmode) && ct <:< pt && canAdaptConstantTypeToLiteral =>
          adaptConstant(value)
        case OverloadedType(pre, alts) if !mode.inFunMode =>
          inferExprAlternative(tree, pt);
          adapt(tree, mode, pt, original)
        case NullaryMethodType(restpe) =>
          adapt(tree.setType(restpe), mode, pt, original)
        case TypeRef(_, ByNameParamClass, arg :: Nil) if mode.inExprMode =>
          adapt(tree.setType(arg), mode, pt, original)
        case tp if mode.typingExprNotLhs && isExistentialType(tp) =>
          adapt(tree.setType(tp.dealias.skolemizeExistential(context.owner, tree)), mode, pt, original)
        case PolyType(tparams, restpe) if mode.inNone(TAPPmode | PATTERNmode) && !context.inTypeConstructorAllowed =>
          val tparams1 = cloneSymbols(tparams);
          val tree1 = if (tree.isType) tree else TypeApply(tree, tparams1.map(tparam => TypeTree(tparam.tpeHK).setPos(tree.pos.focus))).setPos(tree.pos);
          {
            ev$1.undetparams = ev$1.undetparams ++ tparams1
          };
          notifyUndetparamsAdded(tparams1);
          adapt(tree1.setType(restpe.substSym(tparams, tparams1)), mode, pt, original)
        case mt: MethodType if mode.typingExprNotFunNotLhs && mt.isImplicit =>
          adaptToImplicitMethod(mt)
        case mt: MethodType if mode.typingExprNotFunNotLhs && !hasUndetsInMonoMode && !treeInfo.isMacroApplicationOrBlock(tree) =>
          instantiateToMethodType(mt)
        case _ =>
          vanillaAdapt(tree)
      }
    }
    def instantiate(tree: Tree, mode: Mode, pt: Type): Tree = {
      inferExprInstance(tree, context.extractUndetparams(), pt)
      adapt(tree, mode, pt)
    }
    def instantiateExpectingUnit(tree: Tree, mode: Mode): Tree = {
      val savedUndetparams = context.undetparams
      silent(_.instantiate(tree, mode, UnitTpe)).orElse(_ => {
        context.undetparams = savedUndetparams
        val valueDiscard = atPos(tree.pos)(Block(List(instantiate(tree, mode, WildcardType)), Literal(Constant(()))))
        typed(valueDiscard, mode, UnitTpe)
      })
    }
    def instantiatePossiblyExpectingUnit(tree: Tree, mode: Mode, pt: Type): Tree = if (mode.typingExprNotFun && pt.typeSymbol == UnitClass) instantiateExpectingUnit(tree, mode) else instantiate(tree, mode, pt)
    private def isAdaptableWithView(qual: Tree) = {
      val qtpe = qual.tpe.widen
      !isPastTyper && qual.isTerm && !qual.isInstanceOf[Super] && (qual.symbol.eq(null) || !qual.symbol.isTerm || qual.symbol.isValue) && !qtpe.isError && !qtpe.typeSymbol.isBottomClass && qtpe != WildcardType && !qual.isInstanceOf[ApplyImplicitView] && (context.implicitsEnabled || context.enrichmentEnabled)
    }
    def adaptToMember(qual: Tree, searchTemplate: Type, reportAmbiguous: Boolean = true, saveErrors: Boolean = true): Tree = if (isAdaptableWithView(qual)) {
      qual.tpe.dealiasWiden match {
        case et: ExistentialType =>
          qual.setType(et.skolemizeExistential(context.owner, qual))
        case _ =>
      }
      inferView(qual, qual.tpe, searchTemplate, reportAmbiguous, saveErrors) match {
        case EmptyTree =>
          qual
        case coercion =>
          if (settings.logImplicitConv) context.echo(qual.pos, "applied implicit conversion from %s to %s = %s".format(qual.tpe, searchTemplate, coercion.symbol.defString));
          typedQualifier(atPos(qual.pos)(new ApplyImplicitView(coercion, List(qual))))
      }
    } else qual
    def adaptToArguments(qual: Tree, name: Name, args: List[Tree], pt: Type, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      def doAdapt(restpe: Type) = adaptToMember(qual, HasMethodMatching(name, args.map(_.tpe), restpe), reportAmbiguous, saveErrors)
      if (pt == WildcardType) doAdapt(pt) else silent(_ => doAdapt(pt)).filter(_ != qual).orElse(_ => logResult(s"fallback on implicits in adaptToArguments: $qual.$name")(doAdapt(WildcardType)))
    }
    def adaptToMemberWithArgs(tree: Tree, qual: Tree, name: Name, mode: Mode, reportAmbiguous: Boolean, saveErrors: Boolean): Tree = {
      def onError(reportError: => Tree): Tree = context.tree match {
        case Apply(tree1, args) if tree1.eq(tree) && args.nonEmpty =>
          silent(_.typedArgs(args.map(_.duplicate), mode)).filter(xs => !xs.exists(_.isErrorTyped)).map(xs => adaptToArguments(qual, name, xs, WildcardType, reportAmbiguous, saveErrors)).orElse(_ => reportError)
        case _ =>
          reportError
      }
      silent(_.adaptToMember(qual, HasMember(name), reportAmbiguous = false)).orElse(errs => onError {
        if (reportAmbiguous) errs.foreach(context.issue(_))
        setError(tree)
      })
    }
    def adaptToName(qual: Tree, name: Name) = if (member(qual, name) != NoSymbol) qual else adaptToMember(qual, HasMember(name))
    private def validateNoCaseAncestor(clazz: Symbol) = if (!phase.erasedTypes) clazz.ancestors.find(_.isCase).foreach(ancestor => context.error(clazz.pos, "case %s has case ancestor %s, but case-to-case inheritance is prohibited. To overcome this limitation, use extractors to pattern match on non-leaf nodes.".format(clazz, ancestor.fullName)))
    private def checkEphemeral(clazz: Symbol, body: List[Tree]) = {
      val isValueClass = !clazz.isTrait
      def where = if (isValueClass) "value class" else "universal trait extending from class Any"
      def implRestriction(tree: Tree, what: String) = context.error(tree.pos, s"implementation restriction: $what is not allowed in $where" + """
This restriction is planned to be removed in subsequent releases.""")
      object checkEphemeralDeep extends Traverser {
        override def traverse(tree: Tree): Unit = if (isValueClass) {
          tree match {
            case _: ModuleDef =>
              implRestriction(tree, "nested object")
            case cd: ClassDef if !cd.symbol.isAnonymousClass =>
              implRestriction(tree, "nested class")
            case Select(sup @ Super(qual, mix), selector) if selector != nme.CONSTRUCTOR && qual.symbol == clazz && mix != tpnme.EMPTY =>
              implRestriction(sup, "qualified super reference")
            case _ =>
          }
          super.traverse(tree)
        }
      }
      body.foreach(stat => {
        def notAllowed(what: String) = context.error(stat.pos, s"$what is not allowed in $where")
        stat match {
          case ClassDef(mods, _, _, _) if isValueClass =>
            implRestriction(stat, s"nested ${if (mods.isTrait) "trait" else "class"}")
          case _: Import | _: ClassDef | _: TypeDef | EmptyTree =>
          case DefDef(_, name, _, _, _, rhs) =>
            if (stat.symbol.isAuxiliaryConstructor) notAllowed("secondary constructor") else if (isValueClass && (name == nme.equals_ || name == nme.hashCode_) && !stat.symbol.isSynthetic) notAllowed(s"redefinition of $name method. See SIP-15, criterion 4.") else if (stat.symbol != null && stat.symbol.isParamAccessor) notAllowed("additional parameter");
            checkEphemeralDeep.traverse(rhs)
          case _: ValDef =>
            notAllowed("field definition")
          case _: ModuleDef =>
            implRestriction(stat, "nested object")
          case _ =>
            notAllowed("this statement")
        }
      })
    }
    private def validateDerivedValueClass(clazz: Symbol, body: List[Tree]) = {
      if (clazz.isTrait) context.error(clazz.pos, "only classes (not traits) are allowed to extend AnyVal")
      if (!clazz.isStatic) context.error(clazz.pos, "value class may not be a " + if (clazz.owner.isTerm) "local class" else "member of another class")
      if (!clazz.isPrimitiveValueClass) clazz.primaryConstructor.paramss match {
        case List(List(param)) =>
          val decls = clazz.info.decls;
          val paramAccessor = clazz.constrParamAccessors.head;
          if (paramAccessor.isMutable) context.error(paramAccessor.pos, "value class parameter must not be a var");
          val accessor = decls.toList.find(x => x.isMethod && x.accessedOrSelf == paramAccessor);
          accessor match {
            case None =>
              context.error(paramAccessor.pos, "value class parameter must be a val and not be private[this]")
            case Some(acc) if acc.isProtectedLocal =>
              context.error(paramAccessor.pos, "value class parameter must not be protected[this]")
            case Some(acc) =>
              if (acc.tpe.typeSymbol.isDerivedValueClass) context.error(acc.pos, "value class may not wrap another user-defined value class");
              checkEphemeral(clazz, body.filterNot(stat => stat.symbol != null && stat.symbol.accessedOrSelf == paramAccessor))
          }
        case _ =>
          context.error(clazz.pos, "value class needs to have exactly one val parameter")
      }
      clazz.typeParams.foreach(tparam => if (tparam.hasAnnotation(definitions.SpecializedClass)) context.error(tparam.pos, "type parameter of value class may not be specialized"))
    }
    private def typedParentType(encodedtpt: Tree, templ: Template, inMixinPosition: Boolean): Tree = {
      val app = treeInfo.dissectApplied(encodedtpt)
      val (treeInfo.Applied(core, _, argss), decodedtpt) = (app, app.callee)
      val argssAreTrivial = argss == Nil || argss == ListOfNil
      var probe = typedTypeConstructor(core.duplicate).tpe.typeSymbol
      if (probe == null) probe = NoSymbol
      probe.initialize
      if (probe.isTrait || inMixinPosition) {
        if (!argssAreTrivial) if (probe.isTrait) ConstrArgsInParentWhichIsTraitError(encodedtpt, probe)
        typedType(decodedtpt)
      } else {
        val supertpt = typedTypeConstructor(decodedtpt)
        val supertparams = if (supertpt.hasSymbolField) supertpt.symbol.typeParams else Nil
        def inferParentTypeArgs: Tree = typedPrimaryConstrBody(templ) {
          val supertpe = PolyType(supertparams, appliedType(supertpt.tpe, supertparams.map(_.tpeHK)))
          val supercall = New(supertpe, mmap(argss)(_.duplicate))
          val treeInfo.Applied(Select(ctor, nme.CONSTRUCTOR), _, _) = supercall
          ctor.setType(supertpe)
          atPos(supertpt.pos.focus)(supercall)
        } match {
          case EmptyTree =>
            MissingTypeArgumentsParentTpeError(supertpt);
            supertpt
          case tpt =>
            TypeTree(tpt.tpe).setPos(supertpt.pos)
        }
        val supertptWithTargs = if (supertparams.isEmpty || context.unit.isJava) supertpt else inferParentTypeArgs
        if (argssAreTrivial) supertptWithTargs else supertptWithTargs.updateAttachment(SuperArgsAttachment(argss))
      }
    }
    private def typedPrimaryConstrBody(templ: Template)(actualSuperCall: => Tree): Tree = treeInfo.firstConstructor(templ.body) match {
      case ctor @ DefDef(_, _, _, vparamss, _, cbody @ Block(cstats, cunit)) =>
        val (preSuperStats, superCall) = {
          val (stats, rest) = cstats.span(x => !treeInfo.isSuperConstrCall(x))
          (stats.map(_.duplicate), if (rest.isEmpty) EmptyTree else rest.head.duplicate)
        };
        val superCall1 = superCall match {
          case global.pendingSuperCall =>
            actualSuperCall
          case EmptyTree =>
            EmptyTree
        }.orElse(cunit);
        val cbody1 = treeCopy.Block(cbody, preSuperStats, superCall1);
        val clazz = context.owner;
        assert(clazz != NoSymbol, templ);
        val cscope = context.outer.makeNewScope(ctor, context.outer.owner);
        val cbody2 = {
          val typer1 = newTyper(cscope)
          clazz.unsafeTypeParams.foreach(sym => typer1.context.scope.enter(sym))
          typer1.namer.enterValueParams(vparamss.map(_.map(_.duplicate)))
          typer1.typed(cbody1)
        };
        val preSuperVals = treeInfo.preSuperFields(templ.body);
        if (preSuperVals.isEmpty && preSuperStats.nonEmpty) devWarning("Wanted to zip empty presuper val list with " + preSuperStats) else map2(preSuperStats, preSuperVals)((ldef, gdef) => gdef.tpt.setType(ldef.symbol.tpe));
        if (superCall1 == cunit) EmptyTree else cbody2 match {
          case Block(_, expr) =>
            expr
          case tree =>
            tree
        }
      case _ =>
        EmptyTree
    }
    private def normalizeFirstParent(parents: List[Tree]): List[Tree] = {
      @annotation.tailrec def explode0(parents: List[Tree]): List[Tree] = {
        val supertpt :: rest = parents
        if (supertpt.tpe.typeSymbol == AnyClass) {
          supertpt.setType(AnyRefTpe)
          parents
        } else if (treeInfo.isTraitRef(supertpt)) {
          val supertpt1 = typedType(supertpt)
          def supersuper = TypeTree(supertpt1.tpe.firstParent).setPos(supertpt.pos.focus)
          if (supertpt1.isErrorTyped) rest else explode0(supersuper :: supertpt1 :: rest)
        } else parents
      }
      def explode(parents: List[Tree]) = if (treeInfo.isTraitRef(parents.head)) explode0(parents) else parents
      if (parents.isEmpty) Nil else explode(parents)
    }
    private def fixDuplicateSyntheticParents(parents: List[Tree]): List[Tree] = parents match {
      case Nil =>
        Nil
      case x :: xs =>
        val sym = x.symbol;
        x :: (fixDuplicateSyntheticParents(if (isPossibleSyntheticParent(sym)) xs.filterNot(_.symbol == sym) else xs))
    }
    def typedParentTypes(templ: Template): List[Tree] = templ.parents match {
      case Nil =>
        List(atPos(templ.pos)(TypeTree(AnyRefTpe)))
      case first :: rest =>
        try {
          val supertpts = fixDuplicateSyntheticParents(normalizeFirstParent((typedParentType(first, templ, inMixinPosition = false)) +: (rest.map(typedParentType(_, templ, inMixinPosition = true)))))
          if (treeInfo.hasUntypedPreSuperFields(templ.body)) typedPrimaryConstrBody(templ)(EmptyTree)
          supertpts.mapConserve(tpt => checkNoEscaping.privates(context.owner, tpt))
        } catch {
          case ex: TypeError =>
            log("Type error calculating parents in template " + templ);
            log("Error: " + ex);
            ParentTypesError(templ, ex);
            List(TypeTree(AnyRefTpe))
        }
    }
    def validateParentClasses(parents: List[Tree], selfType: Type): scala.Unit = {
      val pending = scala.collection.mutable.ListBuffer[AbsTypeError]()
      def validateDynamicParent(parent: Symbol, parentPos: Position) = if (parent == DynamicClass) checkFeature(parentPos, DynamicsFeature)
      def validateParentClass(parent: Tree, superclazz: Symbol) = if (!parent.isErrorTyped) {
        val psym = parent.tpe.typeSymbol.initialize
        checkStablePrefixClassType(parent)
        if (psym != superclazz) if (psym.isTrait) {
          val ps = psym.info.parents
          if (!ps.isEmpty && !superclazz.isSubClass(ps.head.typeSymbol)) pending += ParentSuperSubclassError(parent, superclazz, ps.head.typeSymbol, psym)
        } else pending += ParentNotATraitMixinError(parent, psym)
        if (psym.isFinal) pending += ParentFinalInheritanceError(parent, psym)
        val sameSourceFile = context.unit.source.file == psym.sourceFile
        if (!isPastTyper && psym.hasDeprecatedInheritanceAnnotation && !sameSourceFile) {
          val suffix = psym.deprecatedInheritanceMessage.map(": " + _).getOrElse("")
          val msg = s"inheritance from ${psym.fullLocationString} is deprecated$suffix"
          context.deprecationWarning(parent.pos, psym, msg)
        }
        if (psym.isSealed && !phase.erasedTypes) if (sameSourceFile) psym.addChild(context.owner) else pending += ParentSealedInheritanceError(parent, psym)
        val parentTypeOfThis = parent.tpe.dealias.typeOfThis
        if (!(selfType <:< parentTypeOfThis) && !phase.erasedTypes && !context.owner.isSynthetic && !selfType.isErroneous && !parent.tpe.isErroneous) {
          pending += ParentSelfTypeConformanceError(parent, selfType)
          if (settings.explaintypes) explainTypes(selfType, parentTypeOfThis)
        }
        if (parents.exists(p => p != parent && p.tpe.typeSymbol == psym && !psym.isError)) pending += ParentInheritedTwiceError(parent, psym)
        validateDynamicParent(psym, parent.pos)
      }
      if (!parents.isEmpty && parents.forall(!_.isErrorTyped)) {
        val superclazz = parents.head.tpe.typeSymbol
        parents.foreach(p => validateParentClass(p, superclazz))
      }
      pending.foreach {
        err => ErrorUtils.issueTypeError(err)
      }
    }
    def checkFinitary(classinfo: ClassInfoType): scala.Unit = {
      val clazz = classinfo.typeSymbol
      clazz.typeParams.foreach(tparam => if (classinfo.expansiveRefs(tparam).contains(tparam)) {
        val newinfo = ClassInfoType(classinfo.parents.map(_.instantiateTypeParams(List(tparam), List(AnyRefTpe))), classinfo.decls, clazz)
        clazz.setInfo(clazz.info match {
          case PolyType(tparams, _) =>
            PolyType(tparams, newinfo)
          case _ =>
            newinfo
        })
        FinitaryError(tparam)
      })
    }
    def typedClassDef(cdef: ClassDef): Tree = {
      val clazz = cdef.symbol
      val typedMods = typedModifiers(cdef.mods)
      assert(clazz != NoSymbol, cdef)
      reenterTypeParams(cdef.tparams)
      val tparams1 = cdef.tparams.mapConserve {
        tdef => typedTypeDef(tdef)
      }
      val impl1 = newTyper(context.make(cdef.impl, clazz, newScope)).typedTemplate(cdef.impl, typedParentTypes(cdef.impl))
      val impl2 = finishMethodSynthesis(impl1, clazz, context)
      if (clazz.isTrait && clazz.info.parents.nonEmpty && clazz.info.firstParent.typeSymbol == AnyClass) checkEphemeral(clazz, impl2.body)
      if (clazz.isNonBottomSubClass(ClassfileAnnotationClass) && clazz != ClassfileAnnotationClass) if (!clazz.owner.isPackageClass) context.error(clazz.pos, "inner classes cannot be classfile annotations") else restrictionWarning(cdef.pos, unit, """|subclassing Classfile does not
             |make your annotation visible at runtime.  If that is what
             |you want, you must write the annotation class in Java.""".stripMargin)
      if (!isPastTyper) clazz.getAnnotation(DeprecatedAttr).foreach(ann => {
        val m = companionSymbolOf(clazz, context)
        if (m != NoSymbol) m.moduleClass.addAnnotation(AnnotationInfo(ann.atp, ann.args, List()))
      })
      treeCopy.ClassDef(cdef, typedMods, cdef.name, tparams1, impl2).setType(NoType)
    }
    def typedModuleDef(mdef: ModuleDef): Tree = {
      val linkedClass = companionSymbolOf(mdef.symbol, context)
      if (linkedClass != NoSymbol) linkedClass.info.decl(nme.CONSTRUCTOR).alternatives.foreach(_.initialize)
      val clazz = mdef.symbol.moduleClass
      val typedMods = typedModifiers(mdef.mods)
      assert(clazz != NoSymbol, mdef)
      val noSerializable = linkedClass.eq(NoSymbol) || linkedClass.isErroneous || !linkedClass.isSerializable || clazz.isSerializable
      val impl1 = newTyper(context.make(mdef.impl, clazz, newScope)).typedTemplate(mdef.impl, typedParentTypes(mdef.impl) ++ if (noSerializable) Nil else {
        clazz.makeSerializable()
        List(TypeTree(SerializableTpe).setPos(clazz.pos.focus))
      })
      val impl2 = finishMethodSynthesis(impl1, clazz, context)
      if (settings.isScala211 && mdef.symbol == PredefModule) ensurePredefParentsAreInSameSourceFile(impl2)
      treeCopy.ModuleDef(mdef, typedMods, mdef.name, impl2).setType(NoType)
    }
    private def ensurePredefParentsAreInSameSourceFile(template: Template) = {
      val parentSyms = template.parents.map(_.symbol).filterNot(_ == AnyRefClass)
      if (parentSyms.exists(_.associatedFile != PredefModule.associatedFile)) context.error(template.pos, s"All parents of Predef must be defined in ${PredefModule.associatedFile}.")
    }
    protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template = addSyntheticMethods(templ, clazz, context)
    def rewrappingWrapperTrees(f: Tree => List[Tree]): Tree => List[Tree] = {
      case dd @ DocDef(comment, defn) =>
        f(defn).map(stat => DocDef(comment, stat).setPos(dd.pos))
      case Annotated(annot, defn) =>
        f(defn).map(stat => Annotated(annot, stat))
      case tree =>
        f(tree)
    }
    protected def enterSyms(txt: Context, trees: List[Tree]) = {
      var txt0 = txt
      trees.foreach(tree => txt0 = enterSym(txt0, tree))
    }
    protected def enterSym(txt: Context, tree: Tree): Context = if (txt.eq(context)) namer.enterSym(tree) else newNamer(txt).enterSym(tree)
    def typedTemplate(templ0: Template, parents1: List[Tree]): Template = {
      val templ = templ0
      val clazz = context.owner
      clazz.annotations.map(_.completeInfo())
      if (templ.symbol == NoSymbol) templ.setSymbol(clazz.newLocalDummy(templ.pos))
      val self1 = templ.self match {
        case vd @ ValDef(_, _, tpt, EmptyTree) =>
          val tpt1 = checkNoEscaping.privates(clazz.thisSym, treeCopy.TypeTree(tpt).setOriginal(tpt).setType(vd.symbol.tpe));
          copyValDef(vd)(tpt = tpt1, rhs = EmptyTree).setType(NoType)
      }
      if (self1.name != nme.WILDCARD) context.scope.enter(self1.symbol)
      val selfType = if (clazz.isAnonymousClass && !phase.erasedTypes) intersectionType(clazz.info.parents, clazz.owner) else clazz.typeOfThis
      assert(clazz.info.decls != EmptyScope, clazz)
      val body1 = pluginsEnterStats(this, templ.body)
      enterSyms(context.outer.make(templ, clazz, clazz.info.decls), body1)
      if (!templ.isErrorTyped) validateParentClasses(parents1, selfType)
      if (clazz.isCase) validateNoCaseAncestor(clazz)
      if (clazz.isTrait && hasSuperArgs(parents1.head)) ConstrArgsInParentOfTraitError(parents1.head, clazz)
      if (clazz.isSubClass(ClassfileAnnotationClass) && !clazz.isTopLevel) context.error(clazz.pos, "inner classes cannot be classfile annotations")
      if (!phase.erasedTypes && !clazz.info.resultType.isError) checkFinitary(clazz.info.resultType.asInstanceOf[ClassInfoType])
      val body2 = {
        val body2 = if (isPastTyper || reporter.hasErrors) body1 else body1.flatMap(rewrappingWrapperTrees(namer.addDerivedTrees(Typer.this, _)))
        val primaryCtor = treeInfo.firstConstructor(body2)
        val primaryCtor1 = primaryCtor match {
          case DefDef(_, _, _, _, _, Block(earlyVals :+ global.pendingSuperCall, unit)) =>
            val argss = superArgs(parents1.head).getOrElse(Nil);
            val pos = wrappingPos(parents1.head.pos, primaryCtor :: argss.flatten).makeTransparent;
            val superCall = atPos(pos)(PrimarySuperCall(argss));
            deriveDefDef(primaryCtor)(block => Block(earlyVals :+ superCall, unit).setPos(pos)).setPos(pos)
          case _ =>
            primaryCtor
        }
        body2.mapConserve({
          case `primaryCtor` =>
            primaryCtor1
          case stat =>
            stat
        })
      }
      val body3 = typedStats(body2, templ.symbol)
      if (clazz.info.firstParent.typeSymbol == AnyValClass) validateDerivedValueClass(clazz, body3)
      if (clazz.isTrait) clazz.info.decls.withFilter(decl => decl.isTerm && decl.isEarlyInitialized).foreach(decl => context.warning(decl.pos, "Implementation restriction: early definitions in traits are not initialized before the super class is initialized."))
      treeCopy.Template(templ, parents1, self1, body3).setType(clazz.tpe_*)
    }
    def typedModifiers(mods: Modifiers): Modifiers = mods.copy(annotations = Nil).setPositions(mods.positions)
    def typedValDef(vdef: ValDef): ValDef = {
      val sym = vdef.symbol
      val valDefTyper = {
        val maybeConstrCtx = if ((sym.isParameter || sym.isEarlyInitialized) && sym.owner.isConstructor) context.makeConstructorContext else context
        newTyper(maybeConstrCtx.makeNewScope(vdef, sym))
      }
      valDefTyper.typedValDefImpl(vdef)
    }
    private def typedValDefImpl(vdef: ValDef) = {
      val sym = vdef.symbol.initialize
      val typedMods = typedModifiers(vdef.mods)
      sym.annotations.map(_.completeInfo())
      val tpt1 = checkNoEscaping.privates(sym, typedType(vdef.tpt))
      checkNonCyclic(vdef, tpt1)
      if (sym.hasAnnotation(definitions.VolatileAttr) && !sym.isMutable) VolatileValueError(vdef)
      val rhs1 = if (vdef.rhs.isEmpty) {
        if (sym.isVariable && sym.owner.isTerm && !sym.isLazy && !isPastTyper) LocalVarUninitializedError(vdef)
        vdef.rhs
      } else {
        val tpt2 = if (sym.hasDefault) {
          val tparams = sym.owner.skipConstructor.info.typeParams
          val subst = new SubstTypeMap(tparams, tparams.map(_ => WildcardType)) { override def matches(sym: Symbol, sym1: Symbol) = if (sym.isSkolem) matches(sym.deSkolemize, sym1) else if (sym1.isSkolem) matches(sym, sym1.deSkolemize) else super[SubstTypeMap].matches(sym, sym1) }
          if (sym.hasFlag(65536)) if (tpt1.tpe.typeArgs.isEmpty) WildcardType else subst(tpt1.tpe.typeArgs(0)) else subst(tpt1.tpe)
        } else tpt1.tpe
        transformedOrTyped(vdef.rhs, EXPRmode | BYVALmode, tpt2)
      }
      treeCopy.ValDef(vdef, typedMods, vdef.name, tpt1, checkDead(rhs1)).setType(NoType)
    }
    def computeParamAliases(clazz: Symbol, vparamss: List[List[ValDef]], rhs: Tree): scala.Unit = {
      debuglog(s"computing param aliases for $clazz:${clazz.primaryConstructor.tpe}:$rhs")
      val pending = scala.collection.mutable.ListBuffer[AbsTypeError]()
      def decompose(call: Tree): (Tree, List[Tree]) = call match {
        case _ if call.isErrorTyped =>
          (call, Nil)
        case Apply(fn, args) =>
          foreachSubTreeBoundTo(args, clazz)(tree => {
            if (tree.symbol.isModule) pending += SuperConstrReferenceError(tree)
            tree match {
              case This(qual) =>
                pending += SuperConstrArgsThisReferenceError(tree);
                ()
              case _ =>
                ()
            }
          });
          val (superConstr, preArgs) = decompose(fn);
          val params = fn.tpe.params;
          val applyArgs = if (args.length < params.length) args :+ EmptyTree else args.take(params.length);
          assert(sameLength(applyArgs, params) || call.isErrorTyped, s"arity mismatch but call is not error typed: $clazz (params=$params, args=$applyArgs)");
          (superConstr, preArgs ::: applyArgs)
        case Block(_ :+ superCall, _) =>
          decompose(superCall)
        case _ =>
          (call, Nil)
      }
      val (superConstr, superArgs) = decompose(rhs)
      assert(superConstr.symbol.ne(null), superConstr)
      def superClazz = superConstr.symbol.owner
      def superParamAccessors = superClazz.constrParamAccessors
      if (superConstr.symbol.isPrimaryConstructor && !superClazz.isJavaDefined && sameLength(superParamAccessors, superArgs)) superParamAccessors.zip(superArgs).withFilter(check$ifrefutable$1 => check$ifrefutable$1: @scala.unchecked match {
        case (superAcc, superArg @ Ident(name)) =>
          true
        case _ =>
          false
      }).foreach(_: @scala.unchecked match {
        case (superAcc, superArg @ Ident(name)) =>
          if (mexists(vparamss)(_.symbol == superArg.symbol)) {
            val alias = superAcc.initialize.alias.orElse(superAcc.getter(superAcc.owner)).filter(alias => superClazz.info.nonPrivateMember(alias.name) == alias)
            if (alias.exists && !alias.accessed.isVariable && !isRepeatedParamType(alias.accessed.info)) {
              val ownAcc = clazz.info.decl(name).suchThat(_.isParamAccessor) match {
                case acc if !acc.isDeferred && acc.hasAccessorFlag =>
                  acc.accessed
                case acc =>
                  acc
              }
              ownAcc match {
                case acc: TermSymbol if !acc.isVariable =>
                  debuglog(s"$acc has alias ${alias.fullLocationString}");
                  acc.setAlias(alias)
                case _ =>
              }
            }
          }
      })
      pending.foreach {
        err => ErrorUtils.issueTypeError(err)
      }
    }
    private def checkSelfConstructorArgs(ddef: DefDef, clazz: Symbol): scala.Unit = {
      val pending = scala.collection.mutable.ListBuffer[AbsTypeError]()
      ddef.rhs match {
        case Block(stats, expr) =>
          val selfConstructorCall = stats.headOption.getOrElse(expr);
          foreachSubTreeBoundTo(List(selfConstructorCall), clazz)({
            case tree @ This(qual) =>
              pending += SelfConstrArgsThisReferenceError(tree);
              ()
            case _ =>
              ()
          })
        case _ =>
      }
      pending.foreach {
        err => ErrorUtils.issueTypeError(err)
      }
    }
    private def foreachSubTreeBoundTo[A](trees: List[Tree], clazz: Symbol)(f: Tree => Unit): Unit = trees.foreach(tree => tree.foreach(subTree => {
      val sym = subTree.symbol
      if (sym != null && sym.info.baseClasses.contains(clazz)) f(subTree)
    }))
    def checkMethodStructuralCompatible(ddef: DefDef): Unit = {
      val meth = ddef.symbol
      def parentString = meth.owner.parentSymbols.filterNot(_ == ObjectClass) match {
        case Nil =>
          ""
        case xs =>
          xs.map(_.nameString).mkString(" (of ", " with ", ")")
      }
      def fail(pos: Position, msg: String): Boolean = {
        context.error(pos, msg)
        false
      }
      def paramssTypes(tp: Type): List[List[Type]] = tp match {
        case mt @ MethodType(_, restpe) =>
          val x$59 = mt.paramTypes;
          (paramssTypes(restpe)) :: x$59
        case PolyType(_, restpe) =>
          paramssTypes(restpe)
        case _ =>
          Nil
      }
      def resultType = meth.tpe_*.finalResultType
      def nthParamPos(n1: Int, n2: Int) = try ddef.vparamss(n1)(n2).pos catch {
        case _: IndexOutOfBoundsException =>
          meth.pos
      }
      def failStruct(pos: Position, what: String, where: String = "Parameter type") = fail(pos, s"$where in structural refinement may not refer to $what")
      foreachWithIndex(paramssTypes(meth.tpe))((paramList, listIdx) => foreachWithIndex(paramList)((paramType, paramIdx) => {
        val sym = paramType.typeSymbol
        def paramPos = nthParamPos(listIdx, paramIdx)
        def checkAbstract(tp0: Type, what: String): Boolean = {
          def check(sym: Symbol): Boolean = !sym.isAbstractType || {
            log(s"checking $tp0 in refinement$parentString at ${meth.owner.owner.fullLocationString}")
            !sym.hasTransOwner(meth.owner) && failStruct(paramPos, "an abstract type defined outside that refinement", what) || !sym.hasTransOwner(meth) && failStruct(paramPos, "a type member of that refinement", what) || checkAbstract(sym.info.bounds.hi, "Type bound")
          }
          tp0.dealiasWidenChain.forall(t => check(t.typeSymbol))
        }
        checkAbstract(paramType, "Parameter type")
        if (sym.isDerivedValueClass) failStruct(paramPos, "a user-defined value class")
        if (paramType.isInstanceOf[ThisType] && sym == meth.owner) {
          failStruct(paramPos, "the type of that refinement (self type)")
          ()
        }
      }))
      if (resultType.typeSymbol.isDerivedValueClass) {
        failStruct(ddef.tpt.pos, "a user-defined value class", where = "Result type")
        ()
      }
    }
    def typedDefDef(ddef: DefDef): DefDef = {
      val meth = ddef.symbol.initialize
      reenterTypeParams(ddef.tparams)
      reenterValueParams(ddef.vparamss)
      if (!isPastTyper && meth.isPrimaryConstructor) ddef.vparamss.foreach(vparams => vparams.foreach(vd => if (vd.mods.isParamAccessor) namer.validateParam(vd)))
      val tparams1 = ddef.tparams.mapConserve {
        tdef => typedTypeDef(tdef)
      }
      val vparamss1 = ddef.vparamss.mapConserve(_.mapConserve {
        vdef => typedValDef(vdef)
      })
      meth.annotations.map(_.completeInfo())
      vparamss1.foreach(vparams1 => vparams1.dropRight(1).foreach(vparam1 => if (isRepeatedParamType(vparam1.symbol.tpe)) StarParamNotLastError(vparam1)))
      val tpt1 = checkNoEscaping.privates(meth, typedType(ddef.tpt))
      checkNonCyclic(ddef, tpt1)
      ddef.tpt.setType(tpt1.tpe)
      val typedMods = typedModifiers(ddef.mods)
      var rhs1 = if (ddef.name == nme.CONSTRUCTOR && !ddef.symbol.hasStaticFlag) {
        if (!meth.isPrimaryConstructor && (!meth.owner.isClass || meth.owner.isModuleClass || meth.owner.isAnonOrRefinementClass)) InvalidConstructorDefError(ddef)
        typed(ddef.rhs)
      } else if (meth.isMacro) transformedOr(ddef.rhs, typedMacroBody(this, ddef)) else transformedOrTyped(ddef.rhs, EXPRmode, tpt1.tpe)
      if (meth.isClassConstructor && !isPastTyper && !meth.owner.isSubClass(AnyValClass)) if (meth.isPrimaryConstructor) computeParamAliases(meth.owner, vparamss1, rhs1) else checkSelfConstructorArgs(ddef, meth.owner)
      if (tpt1.tpe.typeSymbol != NothingClass && !context.returnsSeen && rhs1.tpe.typeSymbol != NothingClass) rhs1 = checkDead(rhs1)
      if (!isPastTyper && meth.owner.isClass && meth.paramss.exists(ps => ps.exists(_.hasDefault) && isRepeatedParamType(ps.last.tpe))) StarWithDefaultError(meth)
      if (!isPastTyper) {
        val allParams = meth.paramss.flatten
        allParams.foreach(p => p.deprecatedParamName.foreach(n => if (allParams.exists(p1 => p1.name == n || p != p1 && p1.deprecatedParamName.exists(_ == n))) DeprecatedParamNameError(p, n)))
        if (meth.isStructuralRefinementMember) checkMethodStructuralCompatible(ddef)
        if (meth.isImplicit && !meth.isSynthetic) meth.info.paramss match {
          case List(param) :: _ if !param.isImplicit =>
            checkFeature(ddef.pos, ImplicitConversionsFeature, meth.toString())
          case _ =>
        }
      }
      treeCopy.DefDef(ddef, typedMods, ddef.name, tparams1, vparamss1, tpt1, rhs1).setType(NoType)
    }
    def typedTypeDef(tdef: TypeDef): TypeDef = typerWithCondLocalContext(context.makeNewScope(tdef, tdef.symbol))(tdef.tparams.nonEmpty)(_.typedTypeDefImpl(tdef))
    private def typedTypeDefImpl(tdef: TypeDef): TypeDef = {
      tdef.symbol.initialize
      reenterTypeParams(tdef.tparams)
      val tparams1 = tdef.tparams.mapConserve {
        tdef => typedTypeDef(tdef)
      }
      val typedMods = typedModifiers(tdef.mods)
      tdef.symbol.annotations.map(_.completeInfo())
      if (settings.nospecialization && currentRun.compiles(tdef.symbol)) {
        tdef.symbol.removeAnnotation(definitions.SpecializedClass)
        tdef.symbol.deSkolemize.removeAnnotation(definitions.SpecializedClass)
      }
      val rhs1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.rhs))
      checkNonCyclic(tdef.symbol)
      if (tdef.symbol.owner.isType) rhs1.tpe match {
        case TypeBounds(lo1, hi1) if !(lo1 <:< hi1) =>
          LowerBoundError(tdef, lo1, hi1)
        case _ =>
          ()
      }
      if (tdef.symbol.isDeferred && tdef.symbol.info.isHigherKinded) checkFeature(tdef.pos, HigherKindsFeature)
      treeCopy.TypeDef(tdef, typedMods, tdef.name, tparams1, rhs1).setType(NoType)
    }
    private def enterLabelDef(stat: Tree): scala.Unit = stat match {
      case ldef @ LabelDef(_, _, _) =>
        if (ldef.symbol == NoSymbol) ldef.symbol = namer.enterInScope(context.owner.newLabel(ldef.name, ldef.pos).setInfo(MethodType(List(), UnitTpe)))
      case _ =>
    }
    def typedLabelDef(ldef: LabelDef): LabelDef = if (!nme.isLoopHeaderLabel(ldef.symbol.name) || isPastTyper) {
      val restpe = ldef.symbol.tpe.resultType
      val rhs1 = typed(ldef.rhs, restpe)
      ldef.params.foreach(param => param.setType(param.symbol.tpe))
      deriveLabelDef(ldef)(_ => rhs1).setType(restpe)
    } else {
      val initpe = ldef.symbol.tpe.resultType
      val rhs1 = typed(ldef.rhs)
      val restpe = rhs1.tpe
      if (restpe == initpe) {
        ldef.params.foreach(param => param.setType(param.symbol.tpe))
        treeCopy.LabelDef(ldef, ldef.name, ldef.params, rhs1).setType(restpe)
      } else {
        context.scope.unlink(ldef.symbol)
        val sym2 = namer.enterInScope(context.owner.newLabel(ldef.name, ldef.pos).setInfo(MethodType(List(), restpe)))
        val LabelDef(_, _, rhs1) = resetAttrs(ldef)
        val rhs2 = typed(brutallyResetAttrs(rhs1), restpe)
        ldef.params.foreach(param => param.setType(param.symbol.tpe))
        deriveLabelDef(ldef)(_ => rhs2).setSymbol(sym2).setType(restpe)
      }
    }
    def typedBlock(block0: Block, mode: Mode, pt: Type): Block = {
      val syntheticPrivates = new scala.collection.mutable.ListBuffer[Symbol]
      try {
        namer.enterSyms(block0.stats)
        val block = treeCopy.Block(block0, pluginsEnterStats(this, block0.stats), block0.expr)
        block.stats.foreach(stat => enterLabelDef(stat))
        if (phaseId(currentPeriod) <= currentRun.typerPhase.id) block match {
          case Block(List(classDef @ ClassDef(_, _, _, _)), Apply(Select(New(_), _), _)) =>
            val classDecls = classDef.symbol.info.decls;
            val visibleMembers = pt match {
              case WildcardType =>
                classDecls.toList
              case BoundedWildcardType(TypeBounds(lo, _)) =>
                lo.members
              case _ =>
                pt.members
            };
            def matchesVisibleMember(member: Symbol) = visibleMembers.exists(vis => member.name == vis.name && member.tpe <:< vis.tpe.substThis(vis.owner, classDef.symbol));
            val toHide = classDecls.filter(member => member.isTerm && member.isPossibleInRefinement && member.isPublic && !matchesVisibleMember(member)).map(member => member.resetFlag(524289).setFlag(274877906948).setPrivateWithin(NoSymbol));
            syntheticPrivates ++= toHide
          case _ =>
        }
        val stats1 = if (isPastTyper) block.stats else block.stats.flatMap(stat => stat match {
          case vd @ ValDef(_, _, _, _) if vd.symbol.isLazy =>
            namer.addDerivedTrees(Typer.this, vd)
          case _ =>
            val x$68 = stat;
            Nil :: x$68
        })
        val stats2 = typedStats(stats1, context.owner)
        val expr1 = typed(block.expr, mode &~ (FUNmode | QUALmode), pt)
        treeCopy.Block(block, stats2, expr1).setType(if (treeInfo.isExprSafeToInline(block)) expr1.tpe else expr1.tpe.deconst)
      } finally syntheticPrivates.foreach(_.resetFlag(274877906944))
    }
    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef = {
      cdef.pat.withFilter(check$ifrefutable$2 => check$ifrefutable$2: @scala.unchecked match {
        case Apply(_, xs) =>
          true
        case _ =>
          false
      }).foreach(_: @scala.unchecked match {
        case Apply(_, xs) =>
          xs.dropRight(1).withFilter(x => treeInfo.isStar(x)).foreach(x => StarPositionInPatternError(x))
      })
      val pat1 = typedPattern(cdef.pat, pattpe.withoutAnnotations)
      if (pat1.tpe.paramSectionCount > 0) pat1.modifyType(_.finalResultType)
      cdef.pat.withFilter(check$ifrefutable$3 => check$ifrefutable$3: @scala.unchecked match {
        case bind @ Bind(name, _) =>
          true
        case _ =>
          false
      }).foreach(_: @scala.unchecked match {
        case bind @ Bind(name, _) =>
          val sym = bind.symbol;
          if (name.toTermName != nme.WILDCARD && sym != null) if (sym == NoSymbol) if (context.scope.lookup(name) == NoSymbol) namer.enterInScope(context.owner.newErrorSymbol(name)) else namer.enterIfNotThere(sym)
      })
      val guard1: Tree = if (cdef.guard == EmptyTree) EmptyTree else typed(cdef.guard, BooleanTpe)
      var body1: Tree = typed(cdef.body, pt)
      if (context.enclosingCaseDef.savedTypeBounds.nonEmpty) {
        body1.modifyType {
          tp => eta$0$1.restoreTypeBounds(tp)
        }
        if (isFullyDefined(pt) && !(body1.tpe <:< pt)) {
          log(s"Adding cast to pattern because ${body1.tpe} does not conform to expected type $pt")
          body1 = typedPos(body1.pos)(gen.mkCast(body1, pt.dealiasWiden))
        }
      }
      treeCopy.CaseDef(cdef, pat1, guard1, body1).setType(body1.tpe)
    }
    def typedCases(cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef] = cases.mapConserve(cdef => newTyper(context.makeNewScope(cdef, context.owner)).typedCase(cdef, pattp, pt))
    def adaptCase(cdef: CaseDef, mode: Mode, tpe: Type): CaseDef = deriveCaseDef(cdef)(adapt(_, mode, tpe))
    def packedTypes(trees: List[Tree]): List[Type] = trees.map(c => packedType(c, context.owner).deconst)
    def typedMatch(selector: Tree, cases: List[CaseDef], mode: Mode, pt: Type, tree: Tree = EmptyTree): Match = {
      val selector1 = checkDead(typedByValueExpr(selector))
      val selectorTp = packCaptured(selector1.tpe.widen).skolemizeExistential(context.owner, selector)
      val casesTyped = typedCases(cases, selectorTp, pt)
      def finish(cases: List[CaseDef], matchType: Type) = treeCopy.Match(tree, selector1, cases).setType(matchType)
      if (isFullyDefined(pt)) finish(casesTyped, pt) else packedTypes(casesTyped) match {
        case packed if sameWeakLubAsLub(packed) =>
          finish(casesTyped, lub(packed))
        case packed =>
          val lub = weakLub(packed);
          finish(casesTyped.map(adaptCase(_, mode, lub)), lub)
      }
    }
    def virtualizedMatch(match_: Match, mode: Mode, pt: Type) = {
      import patmat.{ vpmName, PureMatchTranslator }
      val matchStrategy: Tree = if (!(settings.Xexperimental && context.isNameInScope(vpmName._match))) null else qual$4.silent(_.typed(Ident(vpmName._match)), reportAmbiguousErrors = false).orElse(_ => null)
      if (matchStrategy.ne(null)) typed(new PureMatchTranslator(this.asInstanceOf[patmat.global.analyzer.Typer], matchStrategy).translateMatch(match_), mode, pt) else match_
    }
    def synthesizePartialFunction(paramName: TermName, paramPos: Position, paramSynthetic: Boolean, tree: Tree, mode: Mode, pt: Type): Tree = {
      assert(pt.typeSymbol == PartialFunctionClass, s"PartialFunction synthesis for match in $tree requires PartialFunction expected type, but got $pt.")
      val targs = pt.dealiasWiden.typeArgs
      targs match {
        case argTp :: _ if isFullyDefined(argTp) =>
        case _ =>
          MissingParameterTypeAnonMatchError(tree, pt);
          return setError(tree)
      }
      val argTp :: resTp :: Nil = targs
      val targsValidParams = targs.forall(_ <:< AnyTpe)
      val anonClass = context.owner.newAnonymousFunctionClass(tree.pos).addAnnotation(SerialVersionUIDAnnotation)
      import CODE._
      val Match(sel, cases) = tree
      val casesTrue = cases.map(c => deriveCaseDef(c)(x => atPos(x.pos.focus)(TRUE)).duplicate.asInstanceOf[CaseDef])
      def selector(paramSym: Symbol): Tree = gen.mkUnchecked(if (sel != EmptyTree) sel.duplicate else atPos(tree.pos.focusStart)(gen.mkCastPreservingAnnotations(Ident(paramSym), argTp)))
      def mkParam(methodSym: Symbol, tp: Type = argTp) = methodSym.newValueParameter(paramName, paramPos.focus, 2097152).setInfo(tp)
      def mkDefaultCase(body: Tree) = atPos(tree.pos.makeTransparent)(CaseDef(Bind(nme.DEFAULT_CASE, Ident(nme.WILDCARD)), body))
      def applyOrElseMethodDef = {
        val methodSym = anonClass.newMethod(nme.applyOrElse, tree.pos, 34)
        val A1 = methodSym.newTypeParameter(newTypeName("A1")).setInfo(TypeBounds.upper(argTp))
        val x = mkParam(methodSym, A1.tpe)
        val B1 = methodSym.newTypeParameter(newTypeName("B1")).setInfo(TypeBounds.empty)
        val default = methodSym.newValueParameter(newTermName("default"), tree.pos.focus, 2097152).setInfo(functionType(List(A1.tpe), B1.tpe))
        val paramSyms = List(x, default)
        methodSym.setInfo(polyType(List(A1, B1), MethodType(paramSyms, B1.tpe)))
        val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym))
        if (!paramSynthetic) methodBodyTyper.context.scope.enter(x)
        val match0 = methodBodyTyper.typedMatch(selector(x), cases, mode, resTp)
        val matchResTp = match0.tpe
        B1.setInfo(TypeBounds.lower(matchResTp))
        val match_ = {
          val defaultCase = methodBodyTyper.typedCase(mkDefaultCase(methodBodyTyper.typed1(REF(default).APPLY(REF(x)), mode, B1.tpe).setType(B1.tpe)), argTp, B1.tpe)
          treeCopy.Match(match0, match0.selector, match0.cases :+ defaultCase)
        }
        match_.setType(B1.tpe)
        val originals: Map[Symbol, Tree] = {
          def typedIdent(sym: Symbol) = methodBodyTyper.typedType(Ident(sym), mode)
          val A1Tpt = typedIdent(A1)
          val B1Tpt = typedIdent(B1)
          Map(x -> A1Tpt, default -> gen.scalaFunctionConstr(List(A1Tpt), B1Tpt))
        }
        def newParam(param: Symbol): ValDef = {
          val vd = ValDef(param, EmptyTree)
          val tt @ TypeTree() = vd.tpt
          tt.setOriginal(originals(param).setPos(param.pos.focus))
          vd
        }
        val rhs = methodBodyTyper.virtualizedMatch(match_, mode, B1.tpe)
        val defdef = newDefDef(methodSym, rhs)(vparamss = mapParamss(methodSym) {
          param => newParam(param)
        }, tpt = TypeTree(B1.tpe))
        (defdef, matchResTp)
      }
      def isDefinedAtMethod = {
        val methodSym = anonClass.newMethod(nme.isDefinedAt, tree.pos.makeTransparent, 32)
        val paramSym = mkParam(methodSym)
        val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym))
        if (!paramSynthetic) methodBodyTyper.context.scope.enter(paramSym)
        methodSym.setInfo(MethodType(List(paramSym), BooleanTpe))
        val defaultCase = mkDefaultCase(FALSE)
        val match_ = methodBodyTyper.typedMatch(selector(paramSym), casesTrue :+ defaultCase, mode, BooleanTpe)
        DefDef(methodSym, methodBodyTyper.virtualizedMatch(match_, mode, BooleanTpe))
      }
      def applyMethod = {
        val methodSym = anonClass.newMethod(nme, tree.pos, 34)
        val paramSym = mkParam(methodSym)
        methodSym.setInfo(MethodType(List(paramSym), AnyTpe))
        val methodBodyTyper = newTyper(context.makeNewScope(context.tree, methodSym))
        if (!paramSynthetic) methodBodyTyper.context.scope.enter(paramSym)
        val match_ = methodBodyTyper.typedMatch(selector(paramSym), cases, mode, resTp)
        val matchResTp = match_.tpe
        methodSym.setInfo(MethodType(List(paramSym), matchResTp))
        (DefDef(methodSym, methodBodyTyper.virtualizedMatch(match_, mode, matchResTp)), matchResTp)
      }
      def parents(resTp: Type) = addSerializable(appliedType(AbstractPartialFunctionClass.typeConstructor, List(argTp, resTp)))
      val members = {
        val (applyMeth, matchResTp) = {
          anonClass.setInfo(ClassInfoType(parents(resTp), newScope, anonClass))
          if (targsValidParams) applyOrElseMethodDef else applyMethod
        }
        anonClass.setInfo(ClassInfoType(parents(matchResTp), newScope, anonClass))
        List(applyMeth, isDefinedAtMethod)
      }
      members.foreach(m => anonClass.info.decls.enter(m.symbol))
      val typedBlock = typedPos(tree.pos, mode, pt)(Block(ClassDef(anonClass, NoMods, ListOfNil, members, tree.pos.focus), atPos(tree.pos.focus)(Apply(Select(New(Ident(anonClass.name).setSymbol(anonClass)), nme.CONSTRUCTOR), List()))))
      if (typedBlock.isErrorTyped) typedBlock else typedPos(tree.pos, mode, pt)(Typed(typedBlock, TypeTree(typedBlock.tpe.baseType(PartialFunctionClass))))
    }
    def synthesizeSAMFunction(sam: Symbol, fun: Function, resPt: Type, samClassTp: Type, mode: Mode): Tree = {
      val sampos = fun.pos
      val samDefTp = if (isFullyDefined(resPt)) resPt else NoType
      val bodyName = newTermName(sam.name + "$body")
      val samBodyDef = DefDef(NoMods, bodyName, Nil, List(fun.vparams.map(_.duplicate)), TypeTree(samDefTp).setPos(sampos.focus), fun.body)
      var nestedTyper = this
      val samClassTpFullyDefined = if (isFullyDefined(samClassTp)) samClassTp else try {
        val nestedCtx = enterSym(context.makeNewScope(context.tree, context.owner).makeSilent(), samBodyDef)
        nestedTyper = newTyper(nestedCtx)
        val actualSamType = samBodyDef.symbol.info
        val samTyCon = samClassTp.typeSymbol.typeConstructor
        val tparams = samClassTp.typeSymbol.typeParams
        val tvars = tparams.map {
          tparam => freshVar(tparam)
        }
        val samClassTpMoreDefined = appliedType(samTyCon, (samClassTp.typeArgs, tparams, tvars).zipped.map((x0$4, x1$1, x2$1) => (x0$4, x1$1, x2$1) match {
          case (a, _, tv) if isFullyDefined(a) =>
            tv =:= a;
            a
          case (_, p, _) =>
            p.typeConstructor
        }))
        val expectedSamType = samClassTpMoreDefined.memberInfo(sam)
        actualSamType <:< expectedSamType.substituteTypes(tparams, tvars)
        val targs = solvedTypes(tvars, tparams, tparams.map {
          tparam => varianceInType(eta$0$2)(tparam)
        }, upper = false, lubDepth(sam.info :: Nil))
        debuglog(s"sam infer: $samClassTp --> ${appliedType(samTyCon, targs)} by $actualSamType <:< $expectedSamType --> $targs for $tparams")
        appliedType(samTyCon, targs)
      } catch {
        case _: NoInstance | _: TypeError =>
          devWarning(sampos, s"Could not define type $samClassTp using ${samBodyDef.symbol.rawInfo} <:< ${samClassTp.memberInfo(sam)} (for $sam)");
          samClassTp
      }
      val samDef = DefDef(Modifiers(2097186), sam.name.toTermName, Nil, List(fun.vparams), TypeTree(samBodyDef.tpt.tpe).setPos(sampos.focus), Apply(Ident(bodyName), fun.vparams.map(p => Ident(p.name))))
      val serializableParentAddendum = if (typeIsSubTypeOfSerializable(samClassTp)) Nil else List(TypeTree(SerializableTpe))
      val classDef = ClassDef(Modifiers(32), tpnme.ANON_FUN_NAME, tparams = Nil, gen.mkTemplate(parents = (TypeTree(samClassTpFullyDefined)) :: serializableParentAddendum, self = emptyValDef, constrMods = NoMods, vparamss = ListOfNil, body = List(samDef), superPos = sampos.focus))
      val block = nestedTyper.typedPos(sampos, mode, samClassTpFullyDefined)(Block(samBodyDef, classDef, Apply(Select(New(Ident(tpnme.ANON_FUN_NAME)), nme.CONSTRUCTOR), Nil)))
      classDef.symbol.addAnnotation(SerialVersionUIDAnnotation)
      block
    }
    private def typedFunction(fun: Function, mode: Mode, pt: Type): Tree = {
      val numVparams = fun.vparams.length
      val FunctionSymbol = if (numVparams > definitions.MaxFunctionArity) NoSymbol else FunctionClass(numVparams)
      val sam = if (!settings.Xexperimental || pt.typeSymbol == FunctionSymbol) NoSymbol else samOf(pt)
      val samViable = sam.exists && sameLength(sam.info.params, fun.vparams)
      val (argpts, respt) = if (samViable) {
        val samInfo = pt.memberInfo(sam)
        (samInfo.paramTypes, samInfo.resultType)
      } else pt.baseType(FunctionSymbol) match {
        case TypeRef(_, FunctionSymbol, args :+ res) =>
          (args, res)
        case _ =>
          (fun.vparams.map(_ => if (pt == ErrorType) ErrorType else NoType), WildcardType)
      }
      if (!FunctionSymbol.exists) MaxFunctionArityError(fun) else if (argpts.lengthCompare(numVparams) != 0) WrongNumberOfParametersError(fun, argpts) else {
        var issuedMissingParameterTypeError = false
        foreach2(fun.vparams, argpts)((vparam, argpt) => if (vparam.tpt.isEmpty) {
          vparam.tpt.tpe = if (isFullyDefined(argpt)) argpt else {
            fun match {
              case etaExpansion(vparams, fn, args) =>
                silent(_.typed(fn, mode.forFunMode, pt)).filter(_ => context.undetparams.isEmpty).map(fn1 => {
                  val ftpe = normalize(fn1.tpe).baseType(FunctionClass(numVparams))
                  if (isFunctionType(ftpe) && isFullyDefined(ftpe)) return typedFunction(fun, mode, ftpe)
                })
              case _ =>
            }
            MissingParameterTypeError(fun, vparam, pt, withTupleAddendum = !issuedMissingParameterTypeError)
            issuedMissingParameterTypeError = true
            ErrorType
          }
          if (!vparam.tpt.pos.isDefined) {
            vparam.tpt.setPos(vparam.pos.focus)
            ()
          }
        })
        fun.body match {
          case Match(sel, cases) if sel.ne(EmptyTree) && pt.typeSymbol == PartialFunctionClass =>
            val outerTyper = newTyper(context.outer);
            val p = fun.vparams.head;
            if (p.tpt.tpe == null) p.tpt.setType(outerTyper.typedType(p.tpt).tpe);
            outerTyper.synthesizePartialFunction(p.name, p.pos, paramSynthetic = false, fun.body, mode, pt)
          case _ if samViable =>
            newTyper(context.outer).synthesizeSAMFunction(sam, fun, respt, pt, mode)
          case _ =>
            val vparamSyms = fun.vparams.map(vparam => {
              enterSym(context, vparam)
              if (context.retyping) context.scope.enter(vparam.symbol)
              vparam.symbol
            });
            val vparams = fun.vparams.mapConserve {
              vdef => typedValDef(vdef)
            };
            val formals = vparamSyms.map(_.tpe);
            val body1 = typed(fun.body, respt);
            val restpe = packedType(body1, fun.symbol).deconst.resultType;
            val funtpe = appliedType(FunctionSymbol, formals :+ restpe: _*);
            treeCopy.Function(fun, vparams, body1).setType(funtpe)
        }
      }
    }
    def typedRefinement(templ: Template): scala.Unit = {
      val stats = templ.body
      namer.enterSyms(stats)
      {
        unit.toCheck += () => {
          val stats1 = typedStats(stats, NoSymbol)
          val att = templ.attachments.get[CompoundTypeTreeOriginalAttachment].getOrElse(CompoundTypeTreeOriginalAttachment(Nil, Nil))
          templ.removeAttachment[CompoundTypeTreeOriginalAttachment]
          templ.updateAttachment(att.copy(stats = stats1))
          stats1.withFilter(stat => stat.isDef && stat.symbol.isOverridingSymbol).foreach(stat => stat.symbol.setFlag(2))
        }
        ()
      }
    }
    def typedImport(imp: Import): Import = transformed.remove(imp) match {
      case Some(imp1: Import) =>
        imp1
      case _ =>
        log("unhandled import: " + imp + " in " + unit);
        imp
    }
    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val inBlock = exprOwner == context.owner
      def includesTargetPos(tree: Tree) = tree.pos.isRange && context.unit.exists && tree.pos.includes(context.unit.targetPos)
      val localTarget = stats.exists {
        tree => includesTargetPos(tree)
      }
      def typedStat(stat: Tree): Tree = if (context.owner.isRefinementClass && !treeInfo.isDeclarationOrTypeDef(stat)) OnlyDeclarationsError(stat) else stat match {
        case imp @ Import(_, _) =>
          imp.symbol.initialize;
          if (!imp.symbol.isError) {
            context = context.make(imp)
            typedImport(imp)
          } else EmptyTree
        case _ =>
          if (localTarget && !includesTargetPos(stat)) stat else {
            val localTyper = if (inBlock || stat.isDef && !stat.isInstanceOf[LabelDef]) this else newTyper(context.make(stat, exprOwner))
            val result = checkDead(localTyper.typedByValueExpr(stat))
            if (treeInfo.isSelfOrSuperConstrCall(result)) {
              context.inConstructorSuffix = true
              if (treeInfo.isSelfConstrCall(result) && result.symbol.pos.pointOrElse(0) >= exprOwner.enclMethod.pos.pointOrElse(0)) ConstructorsOrderError(stat)
            }
            if (treeInfo.isPureExprForWarningPurposes(result)) context.warning(stat.pos, "a pure expression does nothing in statement position; you may be omitting necessary parentheses")
            result
          }
      }
      def accesses(looker: Symbol, accessed: Symbol) = accessed.isLocalToThis && (accessed.isParamAccessor || looker.hasAccessorFlag && !accessed.hasAccessorFlag && accessed.isPrivate)
      def checkNoDoubleDefs: Unit = {
        val scope = if (inBlock) context.scope else context.owner.info.decls
        var e = scope.elems
        while (e.ne(null) && e.owner == scope) {
          var e1 = scope.lookupNextEntry(e)
          while (e1.ne(null) && e1.owner == scope) {
            if (!accesses(e.sym, e1.sym) && !accesses(e1.sym, e.sym) && (e.sym.isType || inBlock || e.sym.tpe.matches(e1.sym.tpe))) if (!e.sym.isErroneous && !e1.sym.isErroneous && !e.sym.hasDefault && !e.sym.hasAnnotation(BridgeClass) && !e1.sym.hasAnnotation(BridgeClass)) {
              log("""Double definition detected:
  """ + (e.sym.getClass(), e.sym.info, e.sym.ownerChain) + """
  """ + (e1.sym.getClass(), e1.sym.info, e1.sym.ownerChain))
              DefDefinedTwiceError(e.sym, e1.sym)
              scope.unlink(e1)
            }
            e1 = scope.lookupNextEntry(e1)
          }
          e = e.next
        }
      }
      def addSynthetics(stats: List[Tree]): List[Tree] = {
        val scope = if (inBlock) context.scope else context.owner.info.decls
        var newStats = new scala.collection.mutable.ListBuffer[Tree]
        var moreToAdd = true
        while (moreToAdd) {
          val initElems = scope.elems
          def shouldAdd(sym: Symbol) = inBlock || !context.isInPackageObject(sym, context.owner)
          scope.foreach(sym => context.unit.synthetics.get(sym).withFilter(tree => shouldAdd(sym)).foreach(tree => {
            newStats += typedStat(tree)
            context.unit.synthetics -= sym
          }))
          moreToAdd = scope.elems.ne(initElems)
        }
        if (newStats.isEmpty) stats else {
          def matches(stat: Tree, synt: Tree) = (stat, synt) match {
            case (DefDef(_, statName, _, _, _, _), DefDef(mods, syntName, _, _, _, _)) =>
              mods.hasDefault && syntName.toString().startsWith(statName.toString())
            case (ClassDef(_, className, _, _), ModuleDef(_, moduleName, _)) =>
              className.toTermName == moduleName
            case (ClassDef(cmods, cname, _, _), DefDef(dmods, dname, _, _, _, _)) =>
              cmods.isImplicit && dmods.isImplicit && cname.toTermName == dname
            case _ =>
              false
          }
          def matching(stat: Tree): List[Tree] = {
            val (pos, neg) = newStats.partition(synt => matches(stat, synt))
            newStats = neg
            pos.toList
          }
          (stats.foldRight(List[Tree]())((stat, res) => stat :: (matching(stat)) ::: res)) ::: newStats.toList
        }
      }
      val stats1 = stats.mapConserve {
        stat => typedStat(stat)
      }
      if (phase.erasedTypes) stats1 else {
        if (!context.owner.isPackageClass) checkNoDoubleDefs
        addSynthetics(stats1)
      }
    }
    def typedArg(arg: Tree, mode: Mode, newmode: Mode, pt: Type): Tree = {
      val typedMode = mode.onlySticky | newmode
      val t = withCondConstrTyper(mode.inSccMode)(_.typed(arg, typedMode, pt))
      checkDead.inMode(typedMode, t)
    }
    def typedArgs(args: List[Tree], mode: Mode) = args.mapConserve(arg => typedArg(arg, mode, NOmode, WildcardType))
    def needsInstantiation(tparams: List[Symbol], formals: List[Type], args: List[Tree]) = {
      def isLowerBounded(tparam: Symbol) = !tparam.info.bounds.lo.typeSymbol.isBottomClass
      exists2(formals, args)((x0$5, x1$2) => (x0$5, x1$2) match {
        case (formal, Function(vparams, _)) =>
          vparams.exists(_.tpt.isEmpty) && vparams.length <= MaxFunctionArity && formal.baseType(FunctionClass(vparams.length)) match {
            case TypeRef(_, _, formalargs) =>
              exists2(formalargs, vparams)((formal, vparam) => vparam.tpt.isEmpty && tparams.exists {
                sym => formal.contains(sym)
              }) && tparams.forall {
                tparam => isLowerBounded(tparam)
              }
            case _ =>
              false
          }
        case _ =>
          false
      })
    }
    def isNamedApplyBlock(tree: Tree) = context.namedApplyBlockInfo.exists(_._1 == tree)
    def callToCompanionConstr(context: Context, calledFun: Symbol) = calledFun.isConstructor && {
      val methCtx = context.enclMethod
      methCtx != NoContext && {
        val contextFun = methCtx.tree.symbol
        contextFun.isPrimaryConstructor && contextFun.owner.isModuleClass && companionSymbolOf(calledFun.owner, context).moduleClass == contextFun.owner
      }
    }
    def doTypedApply(tree: Tree, fun0: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
      def duplErrTree = setError(treeCopy.Apply(tree, fun0, args))
      def duplErrorTree(err: AbsTypeError) = {
        issue(err)
        duplErrTree
      }
      def preSelectOverloaded(fun: Tree): Tree = if (fun.hasSymbolField && fun.symbol.isOverloaded) {
        def shapeType(arg: Tree): Type = arg match {
          case Function(vparams, body) =>
            functionType(vparams.map(_ => AnyTpe), shapeType(body))
          case AssignOrNamedArg(Ident(name), rhs) =>
            NamedType(name, shapeType(rhs))
          case _ =>
            NothingTpe
        }
        val argtypes = args.map {
          arg => shapeType(arg)
        }
        val pre = fun.symbol.tpe.prefix
        var sym = fun.symbol.filter(alt => isApplicableSafe(context.undetparams, followApply(pre.memberType(alt)), argtypes, pt))
        if (sym.isOverloaded) {
          val sym1 = sym.filter(alt => isApplicableBasedOnArity(pre.memberType(alt), argtypes.length, varargsStar = false, tuplingAllowed = false) || alt.tpe.params.exists(_.hasDefault))
          if (sym1 != NoSymbol) sym = sym1
        }
        if (sym == NoSymbol) fun else adapt(fun.setSymbol(sym).setType(pre.memberType(sym)), mode.forFunMode, WildcardType)
      } else fun
      val fun = preSelectOverloaded(fun0)
      fun.tpe match {
        case OverloadedType(pre, alts) =>
          def handleOverloaded = {
            val undetparams = context.undetparams
            val (args1, argTpes) = qual$10.savingUndeterminedTypeParams()(x$217())
            if (context.hasErrors) setError(tree) else {
              inferMethodAlternative(fun, undetparams, argTpes, pt)
              doTypedApply(tree, adapt(fun, mode.forFunMode, WildcardType), args1, mode, pt)
            }
          };
          handleOverloaded
        case mt @ MethodType(params, _) =>
          val paramTypes = mt.paramTypes;
          val argslen = args.length;
          val formals = formalTypes(paramTypes, argslen);
          def tryTupleApply: Tree = if (eligibleForTupleConversion(paramTypes, argslen) && !phase.erasedTypes) {
            val tupleArgs = List(atPos(tree.pos.makeTransparent)(gen.mkTuple(args)))
            val savedUndetparams = context.undetparams
            silent(_.doTypedApply(tree, fun, tupleArgs, mode, pt)).map(t => {
              val keepTree = !mode.typingExprNotFun || t.symbol == null || checkValidAdaptation(t, args)
              if (keepTree) t else EmptyTree
            }).orElse(_ => {
              context.undetparams = savedUndetparams
              EmptyTree
            })
          } else EmptyTree;
          def tryNamesDefaults: Tree = {
            val lencmp = compareLengths(args, formals)
            def checkNotMacro() = if (treeInfo.isMacroApplication(fun)) tryTupleApply.orElse(duplErrorTree(NamedAndDefaultArgumentsNotSupportedForMacros(tree, fun)))
            if (mt.isErroneous) duplErrTree else if (mode.inPatternMode) duplErrorTree(WrongNumberOfArgsError(tree, fun)) else if (lencmp > 0) tryTupleApply.orElse(duplErrorTree(TooManyArgsNamesDefaultsError(tree, fun))) else if (lencmp == 0) {
              val (namelessArgs, argPos) = removeNames(Typer.this)(args, params)
              if (namelessArgs.exists(_.isErroneous)) duplErrTree else if (!allArgsArePositional(argPos) && !sameLength(formals, params)) duplErrorTree(MultipleVarargError(tree)) else if (allArgsArePositional(argPos) && !isNamedApplyBlock(fun)) {
                checkNotMacro()
                doTypedApply(tree, fun, namelessArgs, mode, pt)
              } else {
                checkNotMacro()
                transformNamedApplication(Typer.this, mode, pt)(treeCopy.Apply(tree, fun, namelessArgs), argPos)
              }
            } else {
              checkNotMacro()
              def ownerOf(sym: Symbol) = if (sym == null || sym == NoSymbol) NoSymbol else sym.owner
              val symsOwnedByContextOwner = tree.collect({
                case t @ _: DefTree | _: Function if ownerOf(t.symbol) == context.owner =>
                  t.symbol
              })
              def rollbackNamesDefaultsOwnerChanges(): scala.Unit = symsOwnedByContextOwner.foreach(_.owner = context.owner)
              val fun1 = transformNamedApplication(Typer.this, mode, pt)(fun, x => x)
              if (fun1.isErroneous) duplErrTree else {
                assert(isNamedApplyBlock(fun1), fun1)
                val NamedApplyInfo(qual, targs, previousArgss, _) = context.namedApplyBlockInfo.get._2
                val blockIsEmpty = fun1 match {
                  case Block(Nil, _) =>
                    context.namedApplyBlockInfo = None;
                    true
                  case _ =>
                    false
                }
                val (allArgs, missing) = addDefaults(args, qual, targs, previousArgss, params, fun.pos.focus, context)
                val funSym = fun1 match {
                  case Block(_, expr) =>
                    expr.symbol
                }
                val lencmp2 = compareLengths(allArgs, formals)
                if (!sameLength(allArgs, args) && callToCompanionConstr(context, funSym)) duplErrorTree(ModuleUsingCompanionClassDefaultArgsErrror(tree)) else if (lencmp2 > 0) {
                  removeNames(Typer.this)(allArgs, params)
                  duplErrTree
                } else if (lencmp2 == 0) {
                  val note = "Error occurred in an application involving default arguments."
                  if (!context.diagnostic.contains(note)) context.diagnostic = note :: context.diagnostic
                  doTypedApply(tree, if (blockIsEmpty) fun else fun1, allArgs, mode, pt)
                } else {
                  rollbackNamesDefaultsOwnerChanges()
                  tryTupleApply.orElse(duplErrorTree(NotEnoughArgsError(tree, fun, missing)))
                }
              }
            }
          };
          if (!sameLength(formals, args) || args.exists {
            arg => isNamedArg(arg)
          } || isNamedApplyBlock(fun)) if (dyna.isApplyDynamicNamed(fun) && isDynamicRewrite(fun)) dyna.typedNamedApply(tree, fun, args, mode, pt) else tryNamesDefaults else {
            val tparams = context.extractUndetparams()
            if (tparams.isEmpty) {
              def handleMonomorphicCall: Tree = {
                def noExpectedType = !phase.erasedTypes && fun.symbol.isLabel && treeInfo.isSynthCaseSymbol(fun.symbol)
                val args1 = if (noExpectedType) typedArgs(args, forArgMode(fun, mode)) else typedArgsForFormals(args, paramTypes, forArgMode(fun, mode))
                val restpe = mt.resultType(mapList(args1)(arg => gen.stableTypeFor(arg).orElse(arg.tpe)))
                def ifPatternSkipFormals(tp: Type) = tp match {
                  case MethodType(_, rtp) if mode.inPatternMode =>
                    rtp
                  case _ =>
                    tp
                }
                if (args.isEmpty && canTranslateEmptyListToNil && fun.symbol.isInitialized && ListModule.hasCompleteInfo && fun.symbol == List_apply) atPos(tree.pos)(gen.mkNil.setType(restpe)) else constfold(treeCopy.Apply(tree, fun, args1).setType(ifPatternSkipFormals(restpe)))
              }
              checkDead.updateExpr(fun)(handleMonomorphicCall)
            } else if (needsInstantiation(tparams, formals, args)) {
              inferExprInstance(fun, tparams)
              doTypedApply(tree, fun, args, mode, pt)
            } else {
              def handlePolymorphicCall = {
                assert(!mode.inPatternMode, mode)
                val lenientTargs = protoTypeArgs(tparams, formals, mt.resultApprox, pt)
                val strictTargs = map2(lenientTargs, tparams)((targ, tparam) => if (targ == WildcardType) tparam.tpeHK else targ)
                var remainingParams = paramTypes
                def typedArgToPoly(arg: Tree, formal: Type): Tree = {
                  val lenientPt = formal.instantiateTypeParams(tparams, lenientTargs)
                  val newmode = if (isByNameParamType(remainingParams.head)) POLYmode else POLYmode | BYVALmode
                  if (remainingParams.tail.nonEmpty) remainingParams = remainingParams.tail
                  val arg1 = typedArg(arg, forArgMode(fun, mode), newmode, lenientPt)
                  val argtparams = context.extractUndetparams()
                  if (!argtparams.isEmpty) {
                    val strictPt = formal.instantiateTypeParams(tparams, strictTargs)
                    inferArgumentInstance(arg1, argtparams, strictPt, lenientPt)
                    arg1
                  } else arg1
                }
                val args1 = map2(args, formals) {
                  (arg, formal) => typedArgToPoly(arg, formal)
                }
                if (args1.exists(_.isErrorTyped)) duplErrTree else {
                  debuglog("infer method inst " + fun + ", tparams = " + tparams + ", args = " + args1.map(_.tpe) + ", pt = " + pt + ", lobounds = " + tparams.map(_.tpe.bounds.lo) + ", parambounds = " + tparams.map(_.info))
                  val undetparams = inferMethodInstance(fun, tparams, args1, pt)
                  try doTypedApply(tree, fun, args1, mode, pt) finally context.undetparams = undetparams
                }
              }
              handlePolymorphicCall
            }
          }
        case SingleType(_, _) =>
          doTypedApply(tree, fun.setType(fun.tpe.widen), args, mode, pt)
        case ErrorType =>
          if (!tree.isErrorTyped) setError(tree) else tree
        case HasUnapply(unapply) if mode.inPatternMode && fun.isTerm =>
          doTypedUnapply(tree, fun0, fun, args, mode, pt)
        case _ =>
          if (treeInfo.isMacroApplication(tree)) duplErrorTree(MacroTooManyArgumentListsError(tree, fun.symbol)) else duplErrorTree(ApplyWithoutArgsError(tree, fun))
      }
    }
    def typedAnnotation(ann: Tree, mode: Mode = EXPRmode): AnnotationInfo = {
      var hasError: Boolean = false
      val pending = scala.collection.mutable.ListBuffer[AbsTypeError]()
      def finish(res: AnnotationInfo): AnnotationInfo = if (hasError) {
        pending.foreach {
          err => ErrorUtils.issueTypeError(err)
        }
        ErroneousAnnotation
      } else res
      def reportAnnotationError(err: AbsTypeError) = {
        pending += err
        hasError = true
        ErroneousAnnotation
      }
      def tryConst(tr: Tree, pt: Type): Option[LiteralAnnotArg] = {
        val ttree = typed(constfold(tr), pt)
        val const: Constant = ttree match {
          case l @ Literal(c) if !l.isErroneous =>
            c
          case tree =>
            tree.tpe match {
              case ConstantType(c) =>
                c
              case tpe =>
                null
            }
        }
        if (const == null) {
          reportAnnotationError(AnnotationNotAConstantError(ttree))
          None
        } else if (const.value == null) {
          reportAnnotationError(AnnotationArgNullError(tr))
          None
        } else Some(LiteralAnnotArg(const))
      }
      def tree2ConstArg(tree: Tree, pt: Type): Option[ClassfileAnnotArg] = tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) if pt.typeSymbol == ArrayClass =>
          reportAnnotationError(ArrayConstantsError(tree));
          None
        case ann @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val annInfo = typedAnnotation(ann, mode);
          val annType = annInfo.tpe;
          if (!annType.typeSymbol.isSubClass(pt.typeSymbol)) reportAnnotationError(AnnotationTypeMismatchError(tpt, annType, annType)) else if (!annType.typeSymbol.isSubClass(ClassfileAnnotationClass)) reportAnnotationError(NestedAnnotationError(ann, annType));
          if (annInfo.atp.isErroneous) {
            hasError = true
            None
          } else Some(NestedAnnotArg(annInfo))
        case Apply(fun, args) =>
          val typedFun = typed(fun, mode.forFunMode);
          if (typedFun.symbol.owner == ArrayModule.moduleClass && typedFun.symbol.name == nme) pt match {
            case TypeRef(_, ArrayClass, targ :: _) =>
              trees2ConstArg(args, targ)
            case _ =>
              reportAnnotationError(ArrayConstantsTypeMismatchError(tree, pt));
              None
          } else tryConst(tree, pt)
        case Typed(t, _) =>
          tree2ConstArg(t, pt)
        case tree =>
          tryConst(tree, pt)
      }
      def trees2ConstArg(trees: List[Tree], pt: Type): Option[ArrayAnnotArg] = {
        val args = trees.map(tree2ConstArg(_, pt))
        if (args.exists(_.isEmpty)) None else Some(ArrayAnnotArg(args.flatten.toArray))
      }
      val treeInfo.Applied(fun0, targs, argss) = ann
      if (fun0.isErroneous) return finish(ErroneousAnnotation)
      val typedFun0 = typed(fun0, mode.forFunMode)
      val typedFunPart = if (targs.isEmpty && typedFun0.exists(t => t.tpe != null && isDummyAppliedType(t.tpe))) logResult(s"Retyped $typedFun0 to find type args")(typed(argss.foldLeft(fun0)(Apply(_, _)))) else typedFun0
      val treeInfo.Applied(typedFun @ Select(New(annTpt), _), _, _) = typedFunPart
      val annType = annTpt.tpe
      finish(if (typedFun.isErroneous) ErroneousAnnotation else if (annType.typeSymbol.isNonBottomSubClass(ClassfileAnnotationClass)) {
        val isJava = typedFun.symbol.owner.isJavaDefined
        if (argss.length > 1) reportAnnotationError(MultipleArgumentListForAnnotationError(ann)) else {
          val annScope = annType.decls.filter(sym => sym.isMethod && !sym.isConstructor && sym.isJavaDefined)
          val names = scala.collection.mutable.Set[Symbol]()
          names ++= if (isJava) annScope.iterator else typedFun.tpe.params.iterator
          def hasValue = names.exists(_.name == nme.value)
          val args = argss match {
            case arg :: Nil :: Nil if !isNamedArg(arg) && hasValue =>
              val x$118 = gen.mkNamedArg(nme.value, arg);
              Nil :: x$118
            case args :: Nil =>
              args
          }
          val nvPairs = args.map({
            case arg @ AssignOrNamedArg(Ident(name), rhs) =>
              val sym = if (isJava) annScope.lookup(name) else findSymbol(typedFun.tpe.params)(_.name == name);
              if (sym == NoSymbol) {
                reportAnnotationError(UnknownAnnotationNameError(arg, name))
                (nme.ERROR, None)
              } else if (!names.contains(sym)) {
                reportAnnotationError(DuplicateValueAnnotationError(arg, name))
                (nme.ERROR, None)
              } else {
                names -= sym
                if (isJava) sym.cookJavaRawInfo()
                val annArg = tree2ConstArg(rhs, sym.tpe.resultType)
                (sym.name, annArg)
              }
            case arg =>
              reportAnnotationError(ClassfileAnnotationsAsNamedArgsError(arg));
              (nme.ERROR, None)
          })
          names.foreach(sym => {
            sym.initialize
            if (!sym.hasAnnotation(AnnotationDefaultAttr) && !sym.hasDefault) reportAnnotationError(AnnotationMissingArgError(ann, annType, sym))
          })
          if (hasError) ErroneousAnnotation else AnnotationInfo(annType, List(), nvPairs.map(p => (p._1, p._2.get))).setOriginal(Apply(typedFun, args).setPos(ann.pos))
        }
      } else {
        val typedAnn: Tree = {
          val localTyper = newTyper(context.make(ann, context.owner.newLocalDummy(ann.pos)))
          localTyper.typed(ann, mode, annType)
        }
        def annInfo(t: Tree): AnnotationInfo = t match {
          case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
            AnnotationInfo(annType, args, List()).setOriginal(typedAnn).setPos(t.pos)
          case Block(stats, expr) =>
            context.warning(t.pos, """Usage of named or default arguments transformed this annotation
constructor call into a block. The corresponding AnnotationInfo
will contain references to local values and default getters instead
of the actual argument trees""");
            annInfo(expr)
          case Apply(fun, args) =>
            context.warning(t.pos, """Implementation limitation: multiple argument lists on annotations are
currently not supported; ignoring arguments """ + args);
            annInfo(fun)
          case _ =>
            reportAnnotationError(UnexpectedTreeAnnotationError(t, typedAnn))
        }
        if (annType.typeSymbol == DeprecatedAttr && argss.flatten.size < 2) context.deprecationWarning(ann.pos, DeprecatedAttr, "@deprecated now takes two arguments; see the scaladoc.")
        if (typedAnn.tpe == null || typedAnn.tpe.isErroneous) ErroneousAnnotation else annInfo(typedAnn)
      })
    }
    def packSymbols(hidden: List[Symbol], tp: Type): Type = global.packSymbols(hidden, tp, context0.owner)
    def isReferencedFrom(ctx: Context, sym: Symbol): Boolean = ctx.owner.isTerm && ctx.scope.exists(dcl => dcl.isInitialized && dcl.info.contains(sym)) || {
      var ctx1 = ctx.outer
      while (ctx1 != NoContext && ctx1.scope.eq(ctx.scope)) ctx1 = ctx1.outer
      ctx1 != NoContext && isReferencedFrom(ctx1, sym)
    }
    def isCapturedExistential(sym: Symbol) = sym.hasAllFlags(34359803904) && {
      val start = if (Statistics.canEnable) Statistics.startTimer(isReferencedNanos) else null
      try !isReferencedFrom(context, sym) finally if (Statistics.canEnable) Statistics.stopTimer(isReferencedNanos, start)
    }
    def packCaptured(tpe: Type): Type = {
      val captured = scala.collection.mutable.Set[Symbol]()
      tpe.foreach(tp => if (isCapturedExistential(tp.typeSymbol)) {
        captured += tp.typeSymbol
        ()
      })
      existentialAbstraction(captured.toList, tpe)
    }
    def packedType(tree: Tree, owner: Symbol): Type = {
      def defines(tree: Tree, sym: Symbol) = sym.isExistentialSkolem && sym.unpackLocation == tree || tree.isDef && tree.symbol == sym
      def isVisibleParameter(sym: Symbol) = sym.isParameter && sym.owner == owner && (sym.isType || !owner.isAnonymousFunction)
      def containsDef(owner: Symbol, sym: Symbol): Boolean = !sym.hasPackageFlag && {
        var o = sym.owner
        while (o != owner && o != NoSymbol && !o.hasPackageFlag) o = o.owner
        o == owner && !isVisibleParameter(sym)
      }
      var localSyms = scala.collection.immutable.Set[Symbol]()
      var boundSyms = scala.collection.immutable.Set[Symbol]()
      def isLocal(sym: Symbol): Boolean = if (sym == NoSymbol || sym.isRefinementClass || sym.isLocalDummy) false else if (owner == NoSymbol) tree.exists(defines(_, sym)) else containsDef(owner, sym) || isRawParameter(sym) || isCapturedExistential(sym)
      def containsLocal(tp: Type): Boolean = tp.exists(t => isLocal(t.typeSymbol) || isLocal(t.termSymbol))
      val dealiasLocals = new TypeMap {
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isAliasType && containsLocal(tp) && tp.dealias.ne(tp)) apply(tp.dealias) else {
              if (pre.isVolatile) InferTypeWithVolatileTypeSelectionError(tree, pre)
              mapOver(tp)
            }
          case _ =>
            mapOver(tp)
        }
      }
      def addLocals(tp: Type): scala.Unit = {
        val remainingSyms = new scala.collection.mutable.ListBuffer[Symbol]
        def addIfLocal(sym: Symbol, tp: Type): scala.Unit = if (isLocal(sym) && !localSyms(sym) && !boundSyms(sym)) if (sym.typeParams.isEmpty) {
          localSyms = localSyms + sym
          {
            remainingSyms += sym
            ()
          }
        } else AbstractExistentiallyOverParamerizedTpeError(tree, tp)
        tp.foreach(t => {
          t match {
            case ExistentialType(tparams, _) =>
              boundSyms = boundSyms ++ tparams
            case AnnotatedType(annots, _) =>
              annots.foreach(annot => annot.args.foreach(arg => arg match {
                case Ident(_) =>
                  if (!arg.tpe.typeSymbol.hasFlag(34359738368)) addIfLocal(arg.symbol, arg.tpe)
                case _ =>
                  ()
              }))
            case _ =>
          }
          addIfLocal(t.termSymbol, t)
          addIfLocal(t.typeSymbol, t)
        })
        remainingSyms.foreach(sym => addLocals(sym.existentialBound))
      }
      val dealiasedType = dealiasLocals(tree.tpe)
      addLocals(dealiasedType)
      packSymbols(localSyms.toList, dealiasedType)
    }
    def typedClassOf(tree: Tree, tpt: Tree, noGen: Boolean = false) = if (!checkClassType(tpt) && noGen) tpt else atPos(tree.pos)(gen.mkClassOf(tpt.tpe))
    protected def typedExistentialTypeTree(tree: ExistentialTypeTree, mode: Mode): Tree = {
      tree.whereClauses.foreach(wc => if (wc.symbol == NoSymbol) {
        namer.enterSym(wc)
        wc.symbol.setFlag(34359738368)
      } else context.scope.enter(wc.symbol))
      val whereClauses1 = typedStats(tree.whereClauses, context.owner)
      whereClauses1.withFilter(check$ifrefutable$4 => check$ifrefutable$4: @scala.unchecked match {
        case vd @ ValDef(_, _, _, _) =>
          true
        case _ =>
          false
      }).foreach(_: @scala.unchecked match {
        case vd @ ValDef(_, _, _, _) =>
          if (vd.symbol.tpe.isVolatile) AbstractionFromVolatileTypeError(vd)
      })
      val tpt1 = typedType(tree.tpt, mode)
      existentialTransform(whereClauses1.map(_.symbol), tpt1.tpe)((tparams, tp) => {
        val original = tpt1 match {
          case tpt: TypeTree =>
            atPos(tree.pos)(ExistentialTypeTree(tpt.original, tree.whereClauses))
          case _ =>
            debuglog(s"cannot reconstruct the original for $tree, because $tpt1 is not a TypeTree");
            tree
        }
        TypeTree(newExistentialType(tparams, tp)).setOriginal(original)
      })
    }
    protected def typedTypeApply(tree: Tree, mode: Mode, fun: Tree, args: List[Tree]): Tree = fun.tpe match {
      case OverloadedType(pre, alts) =>
        inferPolyAlternatives(fun, mapList(args)(treeTpe));
        val tparams = fun.symbol.typeParams;
        val args1 = if (sameLength(args, tparams)) map2Conserve(args, tparams)((arg, tparam) => typedHigherKindedType(arg, mode, Kind.FromParams(tparam.typeParams))) else return TypedApplyWrongNumberOfTpeParametersError(fun, fun);
        typedTypeApply(tree, mode, fun, args1)
      case SingleType(_, _) =>
        typedTypeApply(tree, mode, fun.setType(fun.tpe.widen), args)
      case PolyType(tparams, restpe) if tparams.nonEmpty =>
        if (sameLength(tparams, args)) {
          val targs = mapList(args)(treeTpe)
          checkBounds(tree, NoPrefix, NoSymbol, tparams, targs, "")
          if (isPredefClassOf(fun.symbol)) typedClassOf(tree, args.head, noGen = true) else {
            if (!isPastTyper && fun.symbol == Any_isInstanceOf && targs.nonEmpty) {
              val scrutineeType = fun match {
                case Select(qual, _) =>
                  qual.tpe
                case _ =>
                  AnyTpe
              }
              checkCheckable(tree, targs.head, scrutineeType, inPattern = false)
            }
            val resultpe = restpe.instantiateTypeParams(tparams, targs)
            treeCopy.TypeApply(tree, fun, args).setType(resultpe)
          }
        } else TypedApplyWrongNumberOfTpeParametersError(tree, fun)
      case ErrorType =>
        setError(treeCopy.TypeApply(tree, fun, args))
      case _ =>
        fun match {
          case treeInfo.DynamicApplication(_, _) =>
            fun
          case _ =>
            TypedApplyDoesNotTakeTpeParametersError(tree, fun)
        }
    }
    object dyna {
      import treeInfo.{ isApplyDynamicName, DynamicUpdate, DynamicApplicationNamed }
      def acceptsApplyDynamic(tp: Type) = tp.typeSymbol.isNonBottomSubClass(DynamicClass)
      def acceptsApplyDynamicWithType(qual: Tree, name: Name): Option[Type] = if (!isApplyDynamicName(name) && acceptsApplyDynamic(qual.tpe.widen)) Some(NoType) else None
      def isDynamicallyUpdatable(tree: Tree) = tree match {
        case DynamicUpdate(qual, name) =>
          acceptsApplyDynamic(qual.tpe)
        case _ =>
          false
      }
      def isApplyDynamicNamed(fun: Tree): Boolean = fun match {
        case DynamicApplicationNamed(qual, _) if acceptsApplyDynamic(qual.tpe.widen) =>
          true
        case _ =>
          false
      }
      def typedNamedApply(orig: Tree, fun: Tree, args: List[Tree], mode: Mode, pt: Type): Tree = {
        def argToBinding(arg: Tree): Tree = arg match {
          case AssignOrNamedArg(i @ Ident(name), rhs) =>
            atPos(i.pos.withEnd(rhs.pos.end))(gen.mkTuple(List(atPos(i.pos)(CODE.LIT(name.toString())), rhs)))
          case _ =>
            gen.mkTuple(List(CODE.LIT(""), arg))
        }
        val t = treeCopy.Apply(orig, unmarkDynamicRewrite(fun), args.map {
          arg => argToBinding(arg)
        })
        wrapErrors(t, _.typed(t, mode, pt))
      }
      def mkInvoke(context: Context, tree: Tree, qual: Tree, name: Name): Option[Tree] = {
        val cxTree = context.enclosingNonImportContext.tree
        debuglog(s"dyna.mkInvoke($cxTree, $tree, $qual, $name)")
        val treeInfo.Applied(treeSelection, _, _) = tree
        def isDesugaredApply = {
          val protoQual = macroExpandee(qual).orElse(qual)
          treeSelection match {
            case Select(`protoQual`, nme) =>
              true
            case _ =>
              false
          }
        }
        acceptsApplyDynamicWithType(qual, name).map(tp => {
          def hasNamed(args: List[Tree]): Boolean = args.exists(_.isInstanceOf[AssignOrNamedArg])
          def hasStar(args: List[Tree]) = treeInfo.isWildcardStarArgList(args)
          def applyOp(args: List[Tree]) = if (hasNamed(args)) nme.applyDynamicNamed else nme.applyDynamic
          def matches(t: Tree) = isDesugaredApply || treeInfo.dissectApplied(t).core == treeSelection
          def findSelection(t: Tree): Option[(TermName, Tree)] = t match {
            case Apply(fn, args) if hasStar(args) =>
              DynamicVarArgUnsupported(tree, applyOp(args));
              None
            case Apply(fn, args) if matches(fn) =>
              Some((applyOp(args), fn))
            case Assign(lhs, _) if matches(lhs) =>
              Some((nme.updateDynamic, lhs))
            case _ if matches(t) =>
              Some((nme.selectDynamic, t))
            case _ =>
              t.children.flatMap {
                t => findSelection(t)
              }.headOption
          }
          findSelection(cxTree) match {
            case Some((opName, treeInfo.Applied(_, targs, _))) =>
              val fun = gen.mkTypeApply(Select(qual, opName), targs);
              if (opName == nme.updateDynamic) suppressMacroExpansion(fun);
              val nameStringLit = atPos(treeSelection.pos.withStart(treeSelection.pos.point).makeTransparent)(Literal(Constant(name.decode)));
              markDynamicRewrite(atPos(qual.pos)(Apply(fun, List(nameStringLit))))
            case _ =>
              setError(tree)
          }
        })
      }
      def wrapErrors(tree: Tree, typeTree: Typer => Tree): Tree = silent(typeTree).orElse(err => DynamicRewriteError(tree, err.head))
    }
    def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      def lookupInOwner(owner: Symbol, name: Name): Symbol = if (mode.inQualMode) rootMirror.missingHook(owner, name) else NoSymbol
      def lookupInRoot(name: Name): Symbol = lookupInOwner(rootMirror.RootClass, name)
      def lookupInEmpty(name: Name): Symbol = rootMirror.EmptyPackageClass.info.member(name)
      def lookupInQualifier(qual: Tree, name: Name): Symbol = if (name == nme.ERROR || qual.tpe.widen.isErroneous) NoSymbol else lookupInOwner(qual.tpe.typeSymbol, name).orElse {
        NotAMemberError(tree, qual, name)
        NoSymbol
      }
      def typedAnnotated(atd: Annotated): Tree = {
        val ann = atd.annot
        val arg1 = typed(atd.arg, mode, pt)
        val annotMode = mode &~ TYPEmode | EXPRmode
        def resultingTypeTree(tpe: Type) = {
          val original = arg1 match {
            case tt @ TypeTree() if tt.original != null =>
              Annotated(ann, tt.original)
            case _ =>
              Annotated(ann, arg1)
          }
          original.setType(ann.tpe)
          TypeTree(tpe).setOriginal(original).setPos(tree.pos.focus)
        }
        if (arg1.isType) if (ann.tpe == null) {
          val ainfo = typedAnnotation(ann, annotMode)
          val atype = arg1.tpe.withAnnotation(ainfo)
          if (ainfo.isErroneous) arg1 else {
            ann.setType(atype)
            resultingTypeTree(atype)
          }
        } else resultingTypeTree(ann.tpe) else {
          if (ann.tpe == null) {
            val annotInfo = typedAnnotation(ann, annotMode)
            ann.setType(arg1.tpe.withAnnotation(annotInfo))
          }
          val atype = ann.tpe
          Typed(arg1, resultingTypeTree(atype)).setPos(tree.pos).setType(atype)
        }
      }
      def typedBind(tree: Bind) = {
        val name = tree.name
        val body = tree.body
        name match {
          case name: TypeName =>
            assert(body == EmptyTree, context.unit + " typedBind: " + name.debugString + " " + body + " " + body.getClass());
            val sym = if (tree.symbol != NoSymbol) tree.symbol else if (isFullyDefined(pt)) context.owner.newAliasType(name, tree.pos).setInfo(pt) else context.owner.newAbstractType(name, tree.pos).setInfo(TypeBounds.empty);
            if (name != tpnme.WILDCARD) namer.enterInScope(sym) else context.scope.enter(sym);
            tree.setSymbol(sym).setType(sym.tpeHK)
          case name: TermName =>
            val sym = if (tree.symbol != NoSymbol) tree.symbol else context.owner.newValue(name, tree.pos);
            if (name != nme.WILDCARD) {
              if (context.inPatAlternative) VariableInPatternAlternativeError(tree)
              namer.enterInScope(sym)
            };
            val body1 = typed(body, mode, pt);
            val impliedType = patmat.binderTypeImpliedByPattern(body1, pt, sym);
            val symTp = if (treeInfo.isSequenceValued(body)) seqType(impliedType) else impliedType;
            sym.setInfo(symTp);
            tree.setSymbol(sym);
            treeCopy.Bind(tree, name, body1).setSymbol(sym).setType(body1.tpe)
        }
      }
      def typedArrayValue(tree: ArrayValue) = {
        val elemtpt1 = typedType(tree.elemtpt, mode)
        val elems1 = tree.elems.mapConserve(elem => typed(elem, mode, elemtpt1.tpe))
        val tpe1 = if (isFullyDefined(pt) && !phase.erasedTypes) pt else arrayType(elemtpt1.tpe)
        treeCopy.ArrayValue(tree, elemtpt1, elems1).setType(tpe1)
      }
      def typedAssign(lhs: Tree, rhs: Tree): Tree = {
        def typedLhs(lhs: Tree) = typed(lhs, EXPRmode | LHSmode)
        val lhs1 = unsuppressMacroExpansion(typedLhs(suppressMacroExpansion(lhs)))
        val varsym = lhs1.symbol
        def fail() = if (lhs1.isErrorTyped) lhs1 else AssignmentError(tree, varsym)
        if (varsym == null) return fail()
        if (treeInfo.mayBeVarGetter(varsym)) lhs1 match {
          case treeInfo.Applied(Select(qual, name), _, _) =>
            val sel = Select(qual, name.setterName).setPos(lhs.pos);
            val app = Apply(sel, List(rhs)).setPos(tree.pos);
            return typed(app, mode, pt)
          case _ =>
        }
        if (varsym.isVariable || varsym.isValue && phase.erasedTypes) {
          val rhs1 = typedByValueExpr(rhs, lhs1.tpe)
          treeCopy.Assign(tree, lhs1, checkDead(rhs1)).setType(UnitTpe)
        } else if (dyna.isDynamicallyUpdatable(lhs1)) {
          val rhs1 = typedByValueExpr(rhs)
          val t = atPos(lhs1.pos.withEnd(rhs1.pos.end))(Apply(lhs1, List(rhs1)))
          dyna.wrapErrors(t, _.typed1(t, mode, pt))
        } else fail()
      }
      def typedIf(tree: If): If = {
        val cond1 = checkDead(typedByValueExpr(tree.cond, BooleanTpe))
        if (tree.elsep.isEmpty) return treeCopy.If(tree, cond1, typed(tree.thenp, UnitTpe), tree.elsep).setType(UnitTpe)
        val thenp1 = typed(tree.thenp, pt)
        val elsep1 = typed(tree.elsep, pt)
        def samePackedTypes = !isPastTyper && thenp1.tpe.annotations.isEmpty && elsep1.tpe.annotations.isEmpty && packedType(thenp1, context.owner) =:= packedType(elsep1, context.owner)
        def finish(ownType: Type) = treeCopy.If(tree, cond1, thenp1, elsep1).setType(ownType)
        if (isFullyDefined(pt)) finish(pt) else thenp1.tpe.deconst :: elsep1.tpe.deconst :: Nil match {
          case tp :: _ if samePackedTypes =>
            finish(tp)
          case tpes if sameWeakLubAsLub(tpes) =>
            finish(lub(tpes))
          case tpes =>
            val lub = weakLub(tpes);
            treeCopy.If(tree, cond1, adapt(thenp1, mode, lub), adapt(elsep1, mode, lub)).setType(lub)
        }
      }
      def typedVirtualizedMatch(tree: Match): Tree = {
        val selector = tree.selector
        val cases = tree.cases
        if (selector == EmptyTree) if (pt.typeSymbol == PartialFunctionClass) synthesizePartialFunction(newTermName(context.unit.fresh.newName("x")), tree.pos, paramSynthetic = true, tree, mode, pt) else {
          val arity = if (isFunctionType(pt)) pt.dealiasWiden.typeArgs.length - 1 else 1
          val params = List.range(0, arity).map(i => atPos(tree.pos.focusStart)(ValDef(Modifiers(2105344), unit.freshTermName("x" + i + "$"), TypeTree(), EmptyTree)))
          val ids = params.map(p => Ident(p.name))
          val selector1 = atPos(tree.pos.focusStart)(if (arity == 1) ids.head else gen.mkTuple(ids))
          val body = treeCopy.Match(tree, selector1, cases.map {
            tree => duplicateAndKeepPositions(tree)
          }.asInstanceOf[List[CaseDef]])
          typed1(atPos(tree.pos)(Function(params, body)), mode, pt)
        } else virtualizedMatch(typedMatch(selector, cases, mode, pt, tree), mode, pt)
      }
      def typedReturn(tree: Return) = {
        val expr = tree.expr
        val enclMethod = context.enclMethod
        if (enclMethod == NoContext || enclMethod.owner.isConstructor || context.enclClass.enclMethod == enclMethod) ReturnOutsideOfDefError(tree) else {
          val DefDef(_, name, _, _, restpt, _) = enclMethod.tree
          if (restpt.tpe.eq(null)) ReturnWithoutTypeError(tree, enclMethod.owner) else {
            val expr1 = context.withinReturnExpr(typedByValueExpr(expr, restpt.tpe))
            if (restpt.tpe.typeSymbol == UnitClass) if (typed(expr).tpe.typeSymbol != UnitClass) context.warning(tree.pos, "enclosing method " + name + " has result type Unit: return value discarded")
            val res = treeCopy.Return(tree, checkDead(expr1)).setSymbol(enclMethod.owner)
            val tp = pluginsTypedReturn(NothingTpe, this, res, restpt.tpe)
            res.setType(tp)
          }
        }
      }
      def typedNew(tree: New) = {
        val tpt = tree.tpt
        val tpt1 = {
          val tpt0 = typedTypeConstructor(tpt).modifyType(_.dealias)
          if (checkStablePrefixClassType(tpt0)) if (tpt0.hasSymbolField && !tpt0.symbol.typeParams.isEmpty) {
            context.undetparams = cloneSymbols(tpt0.symbol.typeParams)
            notifyUndetparamsAdded(context.undetparams)
            TypeTree().setOriginal(tpt0).setType(appliedType(tpt0.tpe, context.undetparams.map(_.tpeHK)))
          } else tpt0 else tpt0
        }
        def narrowRhs(tp: Type) = {
          val sym = context.tree.symbol
          context.tree match {
            case ValDef(mods, _, _, Apply(Select(`tree`, _), _)) if !mods.isMutable && sym != null && sym != NoSymbol =>
              val sym1 = if (sym.owner.isClass && sym.getter(sym.owner) != NoSymbol) sym.getter(sym.owner) else sym.lazyAccessorOrSelf;
              val pre = if (sym1.owner.isClass) sym1.owner.thisType else NoPrefix;
              intersectionType(List(tp, singleType(pre, sym1)))
            case _ =>
              tp
          }
        }
        val tp = tpt1.tpe
        val sym = tp.typeSymbol.initialize
        if (sym.isAbstractType || sym.hasAbstractFlag) IsAbstractError(tree, sym) else if (isPrimitiveValueClass(sym)) {
          NotAMemberError(tpt, TypeTree(tp), nme.CONSTRUCTOR)
          setError(tpt)
        } else if (!(tp == sym.typeOfThis || narrowRhs(tp) <:< tp.typeOfThis || phase.erasedTypes)) DoesNotConformToSelfTypeError(tree, sym, tp.typeOfThis) else treeCopy.New(tree, tpt1).setType(tp)
      }
      def functionTypeWildcard(tree: Tree, arity: Int): Type = {
        val tp = functionType(List.fill(arity)(WildcardType), WildcardType)
        if (tp == NoType) MaxFunctionArityError(tree)
        tp
      }
      def typedEta(expr1: Tree): Tree = expr1.tpe match {
        case TypeRef(_, ByNameParamClass, _) =>
          val expr2 = Function(List(), expr1).setPos(expr1.pos);
          new ChangeOwnerTraverser(context.owner, expr2.symbol).traverse(expr2);
          typed1(expr2, mode, pt)
        case NullaryMethodType(restpe) =>
          val expr2 = Function(List(), expr1).setPos(expr1.pos);
          new ChangeOwnerTraverser(context.owner, expr2.symbol).traverse(expr2);
          typed1(expr2, mode, pt)
        case PolyType(_, MethodType(formals, _)) =>
          if (isFunctionType(pt)) expr1 else adapt(expr1, mode, functionTypeWildcard(expr1, formals.length))
        case MethodType(formals, _) =>
          if (isFunctionType(pt)) expr1 else adapt(expr1, mode, functionTypeWildcard(expr1, formals.length))
        case ErrorType =>
          expr1
        case _ =>
          UnderscoreEtaError(expr1)
      }
      def tryTypedArgs(args: List[Tree], mode: Mode): Option[List[Tree]] = {
        val c = context.makeSilent(reportAmbiguousErrors = false)
        c.retyping = true
        try {
          val res = newTyper(c).typedArgs(args, mode)
          if (c.hasErrors) None else Some(res)
        } catch {
          case ex: CyclicReference =>
            throw ex
          case te: TypeError =>
            None
        }
      }
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree = {
        val start = if (Statistics.canEnable) Statistics.startTimer(failedApplyNanos) else null
        def onError(typeErrors: Seq[AbsTypeError]): Tree = {
          if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, start)
          if (fun.symbol.ne(null) && fun.symbol.isJavaDefined) {
            val newtpe = rawToExistential(fun.tpe)
            if (fun.tpe.ne(newtpe)) return tryTypedApply(fun.setType(newtpe), args)
          }
          def treesInResult(tree: Tree): List[Tree] = tree :: tree match {
            case Block(_, r) =>
              treesInResult(r)
            case Match(_, cases) =>
              cases
            case CaseDef(_, _, r) =>
              treesInResult(r)
            case Annotated(_, r) =>
              treesInResult(r)
            case If(_, t, e) =>
              treesInResult(t) ++ treesInResult(e)
            case Try(b, catches, _) =>
              treesInResult(b) ++ catches
            case Typed(r, Function(Nil, EmptyTree)) =>
              treesInResult(r)
            case Select(qual, name) =>
              treesInResult(qual)
            case Apply(fun, args) =>
              treesInResult(fun) ++ args.flatMap {
                tree => treesInResult(tree)
              }
            case TypeApply(fun, args) =>
              treesInResult(fun) ++ args.flatMap {
                tree => treesInResult(tree)
              }
            case _ =>
              Nil
          }
          def errorInResult(tree: Tree) = treesInResult(tree).exists(err => typeErrors.exists(_.errPos == err.pos))
          val retry = typeErrors.forall(_.errPos != null) && (fun :: tree :: args).exists {
            tree => errorInResult(tree)
          }
          typingStack.printTyping {
            val funStr = ptTree(fun) + " and " + args.map {
              t => ptTree(t)
            }.mkString(", ")
            if (retry) "second try: " + funStr else "no second try: " + funStr + " because error not in result: " + typeErrors.head.errPos + "!=" + tree.pos
          }
          if (retry) {
            val Select(qual, name) = fun
            tryTypedArgs(args, forArgMode(fun, mode)) match {
              case Some(args1) if !args1.exists(arg => arg.exists(_.isErroneous)) =>
                val qual1 = if (!pt.isError) adaptToArguments(qual, name, args1, pt, reportAmbiguous = true, saveErrors = true) else qual;
                if (qual1.ne(qual)) {
                  val tree1 = Apply(Select(qual1, name).setPos(fun.pos), args1).setPos(tree.pos)
                  return context.withinSecondTry(typed1(tree1, mode, pt))
                }
              case _ =>
                ()
            }
          }
          typeErrors.foreach {
            err => issue(err)
          }
          setError(treeCopy.Apply(tree, fun, args))
        }
        silent(_.doTypedApply(tree, fun, args, mode, pt)).orElse {
          typeErrors => onError(typeErrors)
        }
      }
      def normalTypedApply(tree: Tree, fun: Tree, args: List[Tree]) = {
        val stableApplication = fun.symbol.ne(null) && fun.symbol.isMethod && fun.symbol.isStable
        val funpt = if (mode.inPatternMode) pt else WildcardType
        val appStart = if (Statistics.canEnable) Statistics.startTimer(failedApplyNanos) else null
        val opeqStart = if (Statistics.canEnable) Statistics.startTimer(failedOpEqNanos) else null
        def onError(reportError: => Tree): Tree = fun match {
          case Select(qual, name) if !mode.inPatternMode && nme.isOpAssignmentName(newTermName(name.decode)) =>
            val qual1 = typedQualifier(qual);
            if (treeInfo.isVariableOrGetter(qual1)) {
              if (Statistics.canEnable) Statistics.stopTimer(failedOpEqNanos, opeqStart)
              convertToAssignment(fun, qual1, name, args)
            } else {
              if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, appStart)
              reportError
            }
          case _ =>
            if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, appStart);
            reportError
        }
        val silentResult = silent(op = _.typed(fun, mode.forFunMode, funpt), reportAmbiguousErrors = !mode.inExprMode && context.ambiguousErrors, newtree = if (mode.inExprMode) tree else context.tree)
        silentResult match {
          case SilentResultValue(fun1) =>
            val fun2 = if (stableApplication) stabilizeFun(fun1, mode, pt) else fun1;
            if (Statistics.canEnable) Statistics.incCounter(typedApplyCount);
            val noSecondTry = isPastTyper || context.inSecondTry || fun2.symbol.ne(null) && fun2.symbol.isConstructor || isImplicitMethodType(fun2.tpe);
            val isFirstTry = fun2 match {
              case Select(_, _) =>
                !noSecondTry && mode.inExprMode
              case _ =>
                false
            };
            if (isFirstTry) tryTypedApply(fun2, args) else doTypedApply(tree, fun2, args, mode, pt)
          case err: SilentTypeError =>
            onError {
              err.reportableErrors.foreach {
                err => issue(err)
              }
              args.foreach(arg => typed(arg, mode, ErrorType))
              setError(tree)
            }
        }
      }
      object ArrayInstantiation {
        def unapply(tree: Apply) = tree match {
          case Apply(Select(New(tpt), name), arg :: Nil) if tpt.tpe != null && tpt.tpe.typeSymbol == ArrayClass =>
            Some(tpt.tpe).collect({
              case erasure.GenericArray(level, componentType) =>
                val tagType = 1.until(level).foldLeft(componentType)(arrayType(res));
                resolveClassTag(tree.pos, tagType) match {
                  case EmptyTree =>
                    MissingClassTagError(tree, tagType)
                  case tag =>
                    atPos(tree.pos)(new ApplyToImplicitArgs(Select(tag, nme.newArray), arg :: Nil))
                }
            })
          case _ =>
            None
        }
      }
      def typedApply(tree: Apply) = tree match {
        case Apply(Block(stats, expr), args) =>
          typed1(atPos(tree.pos)(Block(stats, Apply(expr, args).setPos(tree.pos.makeTransparent))), mode, pt)
        case Apply(fun, args) =>
          normalTypedApply(tree, fun, args) match {
            case ArrayInstantiation(tree1) =>
              typed(tree1, mode, pt)
            case Apply(Select(fun, nme), _) if treeInfo.isSuperConstrCall(fun) =>
              TooManyArgumentListsForConstructor(tree)
            case tree1 =>
              tree1
          }
      }
      def convertToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree]): Tree = {
        val prefix = name.toTermName.stripSuffix(nme.EQL)
        def mkAssign(vble: Tree): Tree = Assign(vble, Apply(Select(vble.duplicate, prefix).setPos(fun.pos.focus), args).setPos(tree.pos.makeTransparent)).setPos(tree.pos)
        def mkUpdate(table: Tree, indices: List[Tree]) = gen.evalOnceAll(table :: indices, context.owner, context.unit)({
          case tab :: is =>
            def mkCall(name: Name, extraArgs: Tree*) = Apply(Select(tab(), name).setPos(table.pos), is.map(i => i()) ++ extraArgs).setPos(tree.pos);
            mkCall(nme.update, Apply(Select(mkCall(nme), prefix).setPos(fun.pos), args).setPos(tree.pos))
          case _ =>
            EmptyTree
        })
        val tree1 = qual match {
          case Ident(_) =>
            mkAssign(qual)
          case Select(qualqual, vname) =>
            gen.evalOnce(qualqual, context.owner, context.unit)(qq => {
              val qq1 = qq()
              mkAssign(Select(qq1, vname).setPos(qual.pos))
            })
          case Apply(fn, indices) =>
            fn match {
              case treeInfo.Applied(Select(table, nme), _, _) =>
                mkUpdate(table, indices)
              case _ =>
                UnexpectedTreeAssignmentConversionError(qual)
            }
        }
        typed1(tree1, mode, pt)
      }
      def typedSuper(tree: Super) = {
        val mix = tree.mix
        val qual1 = typed(tree.qual)
        val clazz = qual1 match {
          case This(_) =>
            qual1.symbol
          case _ =>
            qual1.tpe.typeSymbol
        }
        def findMixinSuper(site: Type): Type = {
          var ps = site.parents.filter(_.typeSymbol.name == mix)
          if (ps.isEmpty) ps = site.parents.filter(_.typeSymbol.toInterface.name == mix)
          if (ps.isEmpty) {
            debuglog("Fatal: couldn't find site " + site + " in " + site.parents.map(_.typeSymbol.name))
            if (phase.erasedTypes && context.enclClass.owner.isImplClass) {
              restrictionError(tree.pos, unit, "traits may not select fields or methods from super[C] where C is a class")
              ErrorType
            } else {
              MixinMissingParentClassNameError(tree, mix, clazz)
              ErrorType
            }
          } else if (!ps.tail.isEmpty) {
            AmbiguousParentClassError(tree)
            ErrorType
          } else ps.head
        }
        val owntype = if (!mix.isEmpty) findMixinSuper(clazz.tpe) else if (context.inSuperInit) clazz.info.firstParent else intersectionType(clazz.info.parents)
        treeCopy.Super(tree, qual1, mix).setType(SuperType(clazz.thisType, owntype))
      }
      def typedThis(tree: This) = tree.symbol.orElse(qualifyingClass(tree, tree.qual, packageOK = false)) match {
        case NoSymbol =>
          tree
        case clazz =>
          tree.setSymbol(clazz).setType(clazz.thisType.underlying);
          if (isStableContext(tree, mode, pt)) tree.setType(clazz.thisType) else tree
      }
      def typedSelect(tree: Tree, qual: Tree, name: Name): Tree = {
        val t = typedSelectInternal(tree, qual, name)
        if (isPastTyper) t.tpe match {
          case OverloadedType(pre, alts) =>
            if (alts.forall(s => s.owner == ObjectClass || s.owner == AnyClass || isPrimitiveValueClass(s.owner))) () else if (settings.debug) printCaller(s"""|Select received overloaded type during $phase, but typer is over.
                  |If this type reaches the backend, we are likely doomed to crash.
                  |$t has these overloads:
                  |${alts.map(s => "  " + s.defStringSeenAs(pre.memberType(s))).mkString("""
""")}
                  |""".stripMargin)("")
          case _ =>
        }
        t
      }
      def typedSelectInternal(tree: Tree, qual: Tree, name: Name): Tree = {
        def asDynamicCall = dyna.mkInvoke(context, tree, qual, name).map(t => dyna.wrapErrors(t, _.typed1(t, mode, pt)))
        val sym = tree.symbol.orElse(member(qual, name)).orElse {
          if (name != nme.CONSTRUCTOR && mode.inAny(EXPRmode | PATTERNmode)) {
            val qual1 = adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = true, saveErrors = true)
            if (qual1.ne(qual) && !qual1.isErrorTyped) return typed(treeCopy.Select(tree, qual1, name), mode, pt)
          }
          NoSymbol
        }
        if (phase.erasedTypes && qual.isInstanceOf[Super] && tree.symbol != NoSymbol) qual.setType(tree.symbol.owner.tpe)
        if (!reallyExists(sym)) {
          def handleMissing: Tree = {
            def errorTree = missingSelectErrorTree(tree, qual, name)
            def asTypeSelection = if (context.unit.isJava && name.isTypeName) atPos(tree.pos)(gen.convertToSelectFromType(qual, name)) match {
              case EmptyTree =>
                None
              case tree1 =>
                Some(typed1(tree1, mode, pt))
            } else None
            debuglog(s"""
              |qual=$qual:${qual.tpe}
              |symbol=${qual.tpe.termSymbol.defString}
              |scope-id=${qual.tpe.termSymbol.info.decls.hashCode()}
              |members=${qual.tpe.members.mkString(", ")}
              |name=$name
              |found=$sym
              |owner=${context.enclClass.owner}
              """.stripMargin)
            asTypeSelection.orElse(asDynamicCall).getOrElse(lookupInQualifier(qual, name) match {
              case NoSymbol =>
                setError(errorTree)
              case found =>
                typed1(tree.setSymbol(found), mode, pt)
            })
          }
          handleMissing
        } else {
          val tree1 = tree match {
            case Select(_, _) =>
              treeCopy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) =>
              treeCopy.SelectFromTypeTree(tree, qual, name)
          }
          val (result, accessibleError) = silent(_.makeAccessible(tree1, sym, qual.tpe, qual)) match {
            case SilentTypeError(err: AccessTypeError) =>
              (tree1, Some(err))
            case SilentTypeError(err) =>
              SelectWithUnderlyingError(tree, err);
              return tree
            case SilentResultValue(treeAndPre) =>
              (stabilize(treeAndPre._1, treeAndPre._2, mode, pt), None)
          }
          result match {
            case SelectFromTypeTree(qual @ TypeTree(), name) if qual.tpe.typeArgs.nonEmpty =>
              treeCopy.SelectFromTypeTree(result, TypeTreeWithDeferredRefCheck()(() => {
                val tp = qual.tpe
                val sym = tp.typeSymbolDirect
                checkBounds(qual, tp.prefix, sym.owner, sym.typeParams, tp.typeArgs, "")
                qual
              }).setType(qual.tpe).setPos(qual.pos), name)
            case _ if accessibleError.isDefined =>
              val qual1 = if (name == nme.CONSTRUCTOR) qual else adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = false, saveErrors = false);
              if (!qual1.isErrorTyped && qual1.ne(qual)) typed(Select(qual1, name).setPos(tree.pos), mode, pt) else asDynamicCall.getOrElse {
                issue(accessibleError.get)
                setError(tree)
              }
            case _ =>
              result
          }
        }
      }
      def tryWithFilterAndFilter(tree: Select, qual: Tree): Tree = {
        def warn(sym: Symbol) = context.deprecationWarning(tree.pos, sym, s"`withFilter' method does not yet exist on ${qual.tpe.widen}, using `filter' method instead")
        silent(_ => typedSelect(tree, qual, nme.withFilter)).orElse(_ => silent(_ => typed1(Select(qual, nme.filter).setPos(tree.pos), mode, pt)) match {
          case SilentResultValue(res) =>
            warn(res.symbol);
            res
          case SilentTypeError(err) =>
            WithFilterError(tree, err)
        })
      }
      def typedSelectOrSuperCall(tree: Select) = tree match {
        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          typedSelect(tree, typedSelectOrSuperQualifier(qual), nme.CONSTRUCTOR)
        case Select(qual, name) =>
          if (Statistics.canEnable) Statistics.incCounter(typedSelectCount);
          val qualTyped = checkDead(typedQualifier(qual, mode));
          val qualStableOrError = if (qualTyped.isErrorTyped || !name.isTypeName || treeInfo.admitsTypeSelection(qualTyped)) qualTyped else UnstableTreeError(qualTyped);
          val tree1 = name match {
            case nme.withFilter if !settings.future =>
              tryWithFilterAndFilter(tree, qualStableOrError)
            case _ =>
              typedSelect(tree, qualStableOrError, name)
          };
          def sym = tree1.symbol;
          if (tree.isInstanceOf[PostfixSelect]) checkFeature(tree.pos, PostfixOpsFeature, name.decode);
          if (sym != null && sym.isOnlyRefinementMember && !sym.isMacro) checkFeature(tree1.pos, ReflectiveCallsFeature, sym.toString());
          qualStableOrError.symbol match {
            case s: Symbol if s.isRootPackage =>
              treeCopy.Ident(tree1, name)
            case _ =>
              tree1
          }
      }
      def qualifies(sym: Symbol) = sym.hasRawInfo && reallyExists(sym) && !(mode.typingConstructorPattern && sym.isMethod && !sym.isStable)
      def typedIdent(tree: Tree, name: Name): Tree = {
        def inEmptyPackage = if (settings.exposeEmptyPackage) lookupInEmpty(name) else NoSymbol
        def issue(err: AbsTypeError) = {
          val suppress = reporter.hasErrors && name.startsWith(tpnme.ANON_CLASS_NAME)
          if (!suppress) ErrorUtils.issueTypeError(err)
          setError(tree)
        }
        val startContext = if (mode.typingPatternOrTypePat) context.outer else context
        val nameLookup = tree.symbol match {
          case NoSymbol =>
            startContext.lookupSymbol(name, {
              sym => qualifies(sym)
            })
          case sym =>
            LookupSucceeded(EmptyTree, sym)
        }
        import InferErrorGen._
        nameLookup match {
          case LookupAmbiguous(msg) =>
            issue(AmbiguousIdentError(tree, name, msg))
          case LookupInaccessible(sym, msg) =>
            issue(AccessError(tree, sym, context, msg))
          case LookupNotFound =>
            inEmptyPackage.orElse(lookupInRoot(name)) match {
              case NoSymbol =>
                issue(SymbolNotFoundError(tree, name, context.owner, startContext))
              case sym =>
                typed1(tree.setSymbol(sym), mode, pt)
            }
          case LookupSucceeded(qual, sym) =>
            if (sym.isThisSym) typed1(This(sym.owner).setPos(tree.pos), mode, pt) else if (isPredefClassOf(sym) && pt.typeSymbol == ClassClass && pt.typeArgs.nonEmpty) typedClassOf(tree, TypeTree(pt.typeArgs.head)) else {
              val pre1 = if (sym.isTopLevel) sym.owner.thisType else if (qual == EmptyTree) NoPrefix else qual.tpe
              val tree1 = if (qual == EmptyTree) tree else atPos(tree.pos)(Select(atPos(tree.pos.focusStart)(qual), name))
              val (tree2, pre2) = makeAccessible(tree1, sym, pre1, qual)
              stabilize(tree2, pre2, mode, pt).modifyType(dropIllegalStarTypes)
            }.setAttachments(tree.attachments)
        }
      }
      def typedIdentOrWildcard(tree: Ident) = {
        val name = tree.name
        if (Statistics.canEnable) Statistics.incCounter(typedIdentCount)
        if (name == nme.WILDCARD && mode.typingPatternNotConstructor || name == tpnme.WILDCARD && mode.inTypeMode) tree.setType(makeFullyDefined(pt)) else typedIdent(tree, name)
      }
      def typedCompoundTypeTree(tree: CompoundTypeTree) = {
        val templ = tree.templ
        val parents1 = templ.parents.mapConserve(typedType(_, mode))
        templ.body.withFilter(stat => !treeInfo.isDeclarationOrTypeDef(stat)).foreach(stat => OnlyDeclarationsError(stat))
        if ((parents1 ++ templ.body).exists(_.isErrorTyped)) tree.setType(ErrorType) else {
          val decls = newScope
          val self = refinedType(parents1.map(_.tpe), context.enclClass.owner, decls, templ.pos)
          newTyper(context.make(templ, self.typeSymbol, decls)).typedRefinement(templ)
          templ.updateAttachment(CompoundTypeTreeOriginalAttachment(parents1, Nil))
          tree.setType(if (templ.exists(_.isErroneous)) ErrorType else self)
        }
      }
      def typedAppliedTypeTree(tree: AppliedTypeTree) = {
        val tpt = tree.tpt
        val args = tree.args
        val tpt1 = typed1(tpt, mode | FUNmode | TAPPmode, WildcardType)
        def isPoly = tpt1.tpe.isInstanceOf[PolyType]
        def isComplete = tpt1.symbol.rawInfo.isComplete
        if (tpt1.isErrorTyped) tpt1 else if (!tpt1.hasSymbolField) AppliedTypeNoParametersError(tree, tpt1.tpe) else {
          val tparams = tpt1.symbol.typeParams
          if (sameLength(tparams, args)) {
            val args1 = map2Conserve(args, tparams)((arg, tparam) => {
              def ptParams = Kind.FromParams(tparam.typeParams)
              val pt = if (mode.typingPatternOrTypePat) {
                tparam.initialize
                ptParams
              } else if (isComplete) ptParams else Kind.Wildcard
              typedHigherKindedType(arg, mode, pt)
            })
            val argtypes = mapList(args1)(treeTpe)
            foreach2(args, tparams)((arg, tparam) => {
              val asym = arg.symbol
              def abounds = asym.info.bounds
              def tbounds = tparam.info.bounds
              def enhanceBounds(): Unit = {
                val TypeBounds(lo0, hi0) = abounds
                val TypeBounds(lo1, hi1) = tbounds.subst(tparams, argtypes)
                val lo = lub(List(lo0, lo1))
                val hi = glb(List(hi0, hi1))
                if (!(lo =:= lo0 && hi =:= hi0)) {
                  asym.setInfo(logResult(s"Updating bounds of ${asym.fullLocationString} in $tree from '$abounds' to")(TypeBounds(lo, hi)))
                  ()
                }
              }
              if (asym != null && asym.isAbstractType) arg match {
                case Bind(_, _) =>
                  enhanceBounds()
                case _ =>
              }
            })
            val original = treeCopy.AppliedTypeTree(tree, tpt1, args1)
            val result = TypeTree(appliedType(tpt1.tpe, argtypes)).setOriginal(original)
            if (isPoly) TypeTreeWithDeferredRefCheck()(() => {
              checkBounds(result, tpt1.tpe.prefix, tpt1.symbol.owner, tpt1.symbol.typeParams, argtypes, "")
              result
            }).setType(result.tpe).setPos(result.pos) else result
          } else if (tparams.isEmpty) AppliedTypeNoParametersError(tree, tpt1.tpe) else {
            if (settings.debug) Console.println(tpt1 + ":" + tpt1.symbol + ":" + tpt1.symbol.info)
            AppliedTypeWrongNumberOfArgsError(tree, tpt1, tparams)
          }
        }
      }
      val sym: Symbol = tree.symbol
      if (sym.ne(null) && sym.ne(NoSymbol)) sym.initialize
      def typedPackageDef(pdef0: PackageDef) = {
        val pdef = treeCopy.PackageDef(pdef0, pdef0.pid, pluginsEnterStats(this, pdef0.stats))
        val pid1 = typedQualifier(pdef.pid).asInstanceOf[RefTree]
        assert(sym.moduleClass.ne(NoSymbol), sym)
        val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls)).typedStats(pdef.stats, NoSymbol)
        treeCopy.PackageDef(tree, pid1, stats1).setType(NoType)
      }
      def defDefTyper(ddef: DefDef) = {
        val isConstrDefaultGetter = ddef.mods.hasDefault && sym.owner.isModuleClass && nme.defaultGetterToMethod(sym.name) == nme.CONSTRUCTOR
        newTyper(context.makeNewScope(ddef, sym)).constrTyperIf(isConstrDefaultGetter)
      }
      def typedAlternative(alt: Alternative) = context.withinPatAlternative(treeCopy.Alternative(tree, alt.trees.mapConserve(alt => typed(alt, mode, pt))).setType(pt))
      def typedStar(tree: Star) = {
        if (!context.starPatterns && !isPastTyper) StarPatternWithVarargParametersError(tree)
        treeCopy.Star(tree, typed(tree.elem, mode, pt)).setType(makeFullyDefined(pt))
      }
      def issueTryWarnings(tree: Try): Try = {
        def checkForCatchAll(cdef: CaseDef): scala.Unit = {
          def unbound(t: Tree) = t.symbol == null || t.symbol == NoSymbol
          def warn(name: Name) = {
            val msg = s"This catches all Throwables. If this is really intended, use `case ${name.decoded} : Throwable` to clear this warning."
            context.warning(cdef.pat.pos, msg)
          }
          if (cdef.guard.isEmpty) cdef.pat match {
            case Bind(name, i @ Ident(_)) if unbound(i) =>
              warn(name)
            case i @ Ident(name) if unbound(i) =>
              warn(name)
            case _ =>
          }
        }
        if (!isPastTyper) tree match {
          case Try(_, Nil, fin) =>
            if (fin.eq(EmptyTree)) context.warning(tree.pos, "A try without a catch or finally is equivalent to putting its body in a block; no exceptions are handled.")
          case Try(_, catches, _) =>
            catches.foreach {
              cdef => checkForCatchAll(cdef)
            }
        }
        tree
      }
      def typedTry(tree: Try) = {
        val Try(block, catches, fin) = tree
        val block1 = typed(block, pt)
        val catches1 = typedCases(catches, ThrowableTpe, pt)
        val fin1 = if (fin.isEmpty) fin else typed(fin, UnitTpe)
        def finish(ownType: Type) = treeCopy.Try(tree, block1, catches1, fin1).setType(ownType)
        issueTryWarnings(if (isFullyDefined(pt)) finish(pt) else (block1 :: catches1).map(_.tpe.deconst) match {
          case tpes if sameWeakLubAsLub(tpes) =>
            finish(lub(tpes))
          case tpes =>
            val lub = weakLub(tpes);
            val block2 = adapt(block1, mode, lub);
            val catches2 = catches1.map(adaptCase(_, mode, lub));
            treeCopy.Try(tree, block2, catches2, fin1).setType(lub)
        })
      }
      def typedThrow(tree: Throw) = {
        val expr1 = typedByValueExpr(tree.expr, ThrowableTpe)
        treeCopy.Throw(tree, expr1).setType(NothingTpe)
      }
      def typedTyped(tree: Typed) = if (treeInfo.isWildcardStarType(tree.tpt)) typedStarInPattern(tree, mode.onlySticky, pt) else if (mode.inPatternMode) typedInPattern(tree, mode.onlySticky, pt) else tree match {
        case Typed(expr, Function(Nil, EmptyTree)) =>
          typed1(suppressMacroExpansion(expr), mode, pt) match {
            case macroDef if treeInfo.isMacroApplication(macroDef) =>
              MacroEtaError(macroDef)
            case exprTyped =>
              typedEta(checkDead(exprTyped))
          }
        case Typed(expr, tpt) =>
          val tpt1 = typedType(tpt, mode);
          val expr1 = typed(expr, mode.onlySticky, tpt1.tpe.deconst);
          treeCopy.Typed(tree, expr1, tpt1).setType(tpt1.tpe)
      }
      def typedTypeApply(tree: TypeApply) = {
        val fun = tree.fun
        val args = tree.args
        val fun1 = typed(fun, mode.forFunMode | TAPPmode)
        val tparams = if (fun1.symbol == null) Nil else fun1.symbol.typeParams
        val args1 = if (sameLength(args, tparams)) map2Conserve(args, tparams)((arg, tparam) => typedHigherKindedType(arg, mode, Kind.FromParams(tparam.typeParams))) else args.mapConserve(typedHigherKindedType(_, mode))
        Typer.this.typedTypeApply(tree, mode, fun1, args1)
      }
      def typedApplyDynamic(tree: ApplyDynamic) = {
        assert(phase.erasedTypes)
        val qual1 = typed(tree.qual, AnyRefTpe)
        val args1 = tree.args.mapConserve(arg => typed(arg, AnyRefTpe))
        treeCopy.ApplyDynamic(tree, qual1, args1).setType(AnyRefTpe)
      }
      def typedReferenceToBoxed(tree: ReferenceToBoxed) = {
        val id = tree.ident
        val id1 = typed1(id, mode, pt) match {
          case id: Ident =>
            id
        }
        val erasedTypes = phaseId(currentPeriod) >= currentRun.erasurePhase.id
        val tpe = capturedVariableType(id.symbol, erasedTypes = erasedTypes)
        treeCopy.ReferenceToBoxed(tree, id1).setType(tpe)
      }
      def warnMissingInterpolator(lit: Literal): Unit = if (!isPastTyper) {
        def isMacroExpansion = openMacros.exists(_.macroApplication.attachments.get[MacroExpansionAttachment] match {
          case Some(MacroExpansionAttachment(_, t: Tree)) =>
            t.exists(_ == lit)
          case _ =>
            false
        })
        def isRecognizablyNotForInterpolation = context.enclosingApply.tree match {
          case Apply(Select(Apply(RefTree(_, nme.StringContext), _), _), _) =>
            true
          case Apply(Select(New(RefTree(_, tpnme.implicitNotFound)), _), _) =>
            true
          case _ =>
            isMacroExpansion
        }
        def requiresNoArgs(tp: Type): Boolean = tp match {
          case PolyType(_, restpe) =>
            requiresNoArgs(restpe)
          case MethodType(Nil, restpe) =>
            requiresNoArgs(restpe)
          case MethodType(p :: _, _) =>
            p.isImplicit
          case _ =>
            true
        }
        def isPlausible(m: Symbol) = m.alternatives.exists(m => requiresNoArgs(m.info))
        def maybeWarn(s: String): Unit = {
          def warn(message: String) = context.warning(lit.pos, s"possible missing interpolator: $message")
          def suspiciousSym(name: TermName) = context.lookupSymbol(name, _ => true).symbol
          def suspiciousExpr = InterpolatorCodeRegex.findFirstIn(s)
          def suspiciousIdents = InterpolatorIdentRegex.findAllIn(s).map(s => suspiciousSym(s.drop(1)))
          if (s.contains( )) if (suspiciousExpr.nonEmpty) warn("detected an interpolated expression") else suspiciousIdents.find {
            m => isPlausible(m)
          }.foreach(sym => warn(s"detected interpolated identifier `$${sym.name}`"))
        }
        lit match {
          case Literal(Constant(s: String)) if !isRecognizablyNotForInterpolation =>
            maybeWarn(s)
          case _ =>
        }
      }
      def typedLiteral(tree: Literal) = {
        if (settings.warnMissingInterpolator) warnMissingInterpolator(tree)
        tree.setType(if (tree.value.tag == 1) UnitTpe else ConstantType(tree.value))
      }
      def typedSingletonTypeTree(tree: SingletonTypeTree) = {
        val refTyped = context.withImplicitsDisabled(typed(tree.ref, MonoQualifierModes | mode.onlyTypePat, AnyRefTpe))
        if (!refTyped.isErrorTyped) tree.setType(refTyped.tpe.resultType)
        if (treeInfo.admitsTypeSelection(refTyped)) tree else UnstableTreeError(refTyped)
      }
      def typedSelectFromTypeTree(tree: SelectFromTypeTree) = {
        val qual1 = typedType(tree.qualifier, mode)
        if (qual1.tpe.isVolatile) TypeSelectionFromVolatileTypeError(tree, qual1) else typedSelect(tree, qual1, tree.name)
      }
      def typedTypeBoundsTree(tree: TypeBoundsTree) = {
        val lo1 = if (tree.lo.isEmpty) TypeTree(NothingTpe) else typedType(tree.lo, mode)
        val hi1 = if (tree.hi.isEmpty) TypeTree(AnyTpe) else typedType(tree.hi, mode)
        treeCopy.TypeBoundsTree(tree, lo1, hi1).setType(TypeBounds(lo1.tpe, hi1.tpe))
      }
      def typedExistentialTypeTree(tree: ExistentialTypeTree) = {
        val tree1 = typerWithLocalContext(context.makeNewScope(tree, context.owner))(_.typedExistentialTypeTree(tree, mode))
        checkExistentialsFeature(tree1.pos, tree1.tpe, "the existential type")
        tree1
      }
      def typedTypeTree(tree: TypeTree) = if (tree.original != null) {
        val newTpt = typedType(tree.original, mode)
        tree.setType(newTpt.tpe)
        newTpt match {
          case tt @ TypeTree() =>
            tree.setOriginal(tt.original)
          case _ =>
            tree
        }
      } else {
        devWarning(tree.pos, s"Assigning Any type to TypeTree because tree.original is null: tree is $tree/${System.identityHashCode(tree)}, sym=${tree.symbol}, tpe=${tree.tpe}")
        tree.setType(AnyTpe)
      }
      def typedFunction(fun: Function) = {
        if (fun.symbol == NoSymbol) fun.symbol = context.owner.newAnonymousFunctionValue(fun.pos)
        typerWithLocalContext(context.makeNewScope(fun, fun.symbol))(_.typedFunction(fun, mode, pt))
      }
      def typedInPatternMode(tree: Tree): Tree = tree match {
        case tree: Alternative =>
          typedAlternative(tree)
        case tree: Star =>
          typedStar(tree)
        case _ =>
          abort(s"unexpected tree in pattern mode: ${tree.getClass()}\n$tree")
      }
      def typedTypTree(tree: TypTree): Tree = tree match {
        case tree: TypeTree =>
          typedTypeTree(tree)
        case tree: AppliedTypeTree =>
          typedAppliedTypeTree(tree)
        case tree: TypeBoundsTree =>
          typedTypeBoundsTree(tree)
        case tree: SingletonTypeTree =>
          typedSingletonTypeTree(tree)
        case tree: SelectFromTypeTree =>
          typedSelectFromTypeTree(tree)
        case tree: CompoundTypeTree =>
          typedCompoundTypeTree(tree)
        case tree: ExistentialTypeTree =>
          typedExistentialTypeTree(tree)
        case tree: TypeTreeWithDeferredRefCheck =>
          tree
        case _ =>
          abort(s"unexpected type-representing tree: ${tree.getClass()}\n$tree")
      }
      def typedMemberDef(tree: MemberDef): Tree = tree match {
        case tree: ValDef =>
          typedValDef(tree)
        case tree: DefDef =>
          defDefTyper(tree).typedDefDef(tree)
        case tree: ClassDef =>
          newTyper(context.makeNewScope(tree, sym)).typedClassDef(tree)
        case tree: ModuleDef =>
          newTyper(context.makeNewScope(tree, sym.moduleClass)).typedModuleDef(tree)
        case tree: TypeDef =>
          typedTypeDef(tree)
        case tree: PackageDef =>
          typedPackageDef(tree)
        case _ =>
          abort(s"unexpected member def: ${tree.getClass()}\n$tree")
      }
      def typedOutsidePatternMode(tree: Tree): Tree = tree match {
        case tree: Block =>
          typerWithLocalContext(context.makeNewScope(tree, context.owner))(_.typedBlock(tree, mode, pt))
        case tree: If =>
          typedIf(tree)
        case tree: TypeApply =>
          typedTypeApply(tree)
        case tree: Function =>
          typedFunction(tree)
        case tree: Match =>
          typedVirtualizedMatch(tree)
        case tree: New =>
          typedNew(tree)
        case tree: Assign =>
          typedAssign(tree.lhs, tree.rhs)
        case tree: AssignOrNamedArg =>
          typedAssign(tree.lhs, tree.rhs)
        case tree: Super =>
          typedSuper(tree)
        case tree: Annotated =>
          typedAnnotated(tree)
        case tree: Return =>
          typedReturn(tree)
        case tree: Try =>
          typedTry(tree)
        case tree: Throw =>
          typedThrow(tree)
        case tree: ArrayValue =>
          typedArrayValue(tree)
        case tree: ApplyDynamic =>
          typedApplyDynamic(tree)
        case tree: ReferenceToBoxed =>
          typedReferenceToBoxed(tree)
        case tree: LabelDef =>
          labelTyper(tree).typedLabelDef(tree)
        case tree: DocDef =>
          typedDocDef(tree, mode, pt)
        case _ =>
          abort(s"unexpected tree: ${tree.getClass()}\n$tree")
      }
      def typedInAnyMode(tree: Tree): Tree = tree match {
        case tree: Ident =>
          typedIdentOrWildcard(tree)
        case tree: Bind =>
          typedBind(tree)
        case tree: Apply =>
          typedApply(tree)
        case tree: Select =>
          typedSelectOrSuperCall(tree)
        case tree: Literal =>
          typedLiteral(tree)
        case tree: Typed =>
          typedTyped(tree)
        case tree: This =>
          typedThis(tree)
        case tree: UnApply =>
          abort(s"unexpected UnApply $tree")
        case _ =>
          if (mode.inPatternMode) typedInPatternMode(tree) else typedOutsidePatternMode(tree)
      }
      tree match {
        case tree: TypTree =>
          typedTypTree(tree)
        case tree: MemberDef =>
          typedMemberDef(tree)
        case _ =>
          typedInAnyMode(tree)
      }
    }
    def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
      lastTreeToTyper = tree
      def body = if (printTypings && !phase.erasedTypes && !noPrintTyping(tree)) typingStack.nextTyped(tree, mode, pt, context)(typedInternal(tree, mode, pt)) else typedInternal(tree, mode, pt)
      val startByType = if (Statistics.canEnable) Statistics.pushTimer(byTypeStack, byTypeNanos(tree.getClass())) else null
      if (Statistics.canEnable) Statistics.incCounter(visitsByType, tree.getClass())
      try body finally if (Statistics.canEnable) Statistics.popTimer(byTypeStack, startByType)
    }
    private def typedInternal(tree: Tree, mode: Mode, pt: Type): Tree = {
      val ptPlugins = pluginsPt(pt, this, tree, mode)
      def retypingOk = context.retyping && tree.tpe.ne(null) && (tree.tpe.isErroneous || !(tree.tpe <:< ptPlugins))
      def runTyper(): Tree = {
        if (retypingOk) {
          tree.tpe = null
          if (tree.hasSymbol) tree.symbol = NoSymbol
        }
        val alreadyTyped = tree.tpe.ne(null)
        val shouldPrint = !alreadyTyped && !phase.erasedTypes
        val ptWild = if (mode.inPatternMode) ptPlugins else dropExistential(ptPlugins)
        val tree1: Tree = if (alreadyTyped) tree else typed1(tree, mode, ptWild)
        if (shouldPrint) typingStack.showTyped(tree1)
        if (tree1.tpe.eq(null)) return setError(tree)
        tree1.modifyType(pluginsTyped(_, this, tree1, mode, ptPlugins))
        val result = if (tree1.isEmpty) tree1 else {
          val result = adapt(tree1, mode, ptPlugins, tree)
          if (hasPendingMacroExpansions) macroExpandAll(this, result) else result
        }
        if (shouldPrint) typingStack.showAdapt(tree1, result, ptPlugins, context)
        if (!isPastTyper) signalDone(context.asInstanceOf[analyzer.Context], tree, result)
        result
      }
      try runTyper() catch {
        case ex: TypeError =>
          tree.clearType();
          typingStack.printTyping(tree, "caught %s: while typing %s".format(ex, tree));
          reportTypeError(context, tree.pos, ex);
          setError(tree)
        case ex: Exception =>
          devWarning(s"exception when typing $tree, pt=$ptPlugins");
          if (context != null && context.unit.exists && tree != null) logError("AT: " + tree.pos, ex);
          throw ex
      }
    }
    def atOwner(owner: Symbol): Typer = newTyper(context.make(owner = owner))
    def atOwner(tree: Tree, owner: Symbol): Typer = newTyper(context.make(tree, owner))
    def typed(tree: Tree): Tree = {
      val ret = typed(tree, context.defaultModeForTyped, WildcardType)
      ret
    }
    def typedByValueExpr(tree: Tree, pt: Type = WildcardType): Tree = typed(tree, EXPRmode | BYVALmode, pt)
    def typedPos(pos: Position, mode: Mode, pt: Type)(tree: Tree) = typed(atPos(pos)(tree), mode, pt)
    def typedPos(pos: Position)(tree: Tree) = typed(atPos(pos)(tree))
    def typed(tree: Tree, pt: Type): Tree = typed(tree, context.defaultModeForTyped, pt)
    def typed(tree: Tree, mode: Mode): Tree = typed(tree, mode, WildcardType)
    def typedQualifier(tree: Tree, mode: Mode, pt: Type): Tree = typed(tree, PolyQualifierModes | mode.onlyTypePat, pt)
    def typedQualifier(tree: Tree, mode: Mode): Tree = typedQualifier(tree, mode, WildcardType)
    def typedQualifier(tree: Tree): Tree = typedQualifier(tree, NOmode, WildcardType)
    def typedOperator(tree: Tree): Tree = typed(tree, OperatorModes)
    private def typedSelectOrSuperQualifier(qual: Tree) = context.withinSuperInit(typed(qual, PolyQualifierModes))
    def typedPattern(tree: Tree, pt: Type): Tree = typingInPattern(context.withImplicitsDisabledAllowEnrichment(typed(tree, PATTERNmode, pt))) match {
      case tpt if tpt.isType =>
        PatternMustBeValue(tpt, pt);
        tpt
      case pat =>
        pat
    }
    def typedType(tree: Tree, mode: Mode): Tree = typed(tree, mode.forTypeMode, WildcardType)
    def typedType(tree: Tree): Tree = typedType(tree, NOmode)
    def typedHigherKindedType(tree: Tree, mode: Mode, pt: Type): Tree = if (pt != Kind.Wildcard && pt.typeParams.isEmpty) typedType(tree, mode) else context.withinTypeConstructorAllowed(typed(tree, NOmode, pt))
    def typedHigherKindedType(tree: Tree, mode: Mode): Tree = context.withinTypeConstructorAllowed(typed(tree))
    def typedTypeConstructor(tree: Tree, mode: Mode): Tree = {
      val result = typed(tree, mode.forTypeMode | FUNmode, WildcardType)
      result.tpe.dealias match {
        case restpe @ TypeRef(pre, _, _) if !phase.erasedTypes && !pre.isStable && !context.unit.isJava =>
          ConstructorPrefixError(tree, restpe)
        case _ =>
          result
      }
    }
    def typedTypeConstructor(tree: Tree): Tree = typedTypeConstructor(tree, NOmode)
    def computeType(tree: Tree, pt: Type): Type = {
      assert(!context.owner.isMacro, context.owner)
      val tree1 = typed(tree, pt)
      transformed(tree) = tree1
      val tpe = packedType(tree1, context.owner)
      checkExistentialsFeature(tree.pos, tpe, "inferred existential type")
      tpe
    }
    def computeMacroDefType(ddef: DefDef, pt: Type): Type = {
      assert(context.owner.isMacro, context.owner)
      assert(ddef.symbol.isMacro, ddef.symbol)
      val rhs1 = if (transformed.contains(ddef.rhs)) transformed(ddef.rhs) else {
        val rhs1 = typedMacroBody(this, ddef)
        transformed(ddef.rhs) = rhs1
        rhs1
      }
      val isMacroBodyOkay = !ddef.symbol.isErroneous && !rhs1.exists(_.isErroneous) && rhs1 != EmptyTree
      val shouldInheritMacroImplReturnType = ddef.tpt.isEmpty
      if (isMacroBodyOkay && shouldInheritMacroImplReturnType) {
        val commonMessage = "macro defs must have explicitly specified return types"
        def reportFailure() = {
          ddef.symbol.setFlag(4294967296)
          context.error(ddef.pos, commonMessage)
        }
        def reportWarning(inferredType: Type) = {
          val explanation = s"inference of $inferredType from macro impl's c.Expr[$inferredType] is deprecated and is going to stop working in 2.12"
          context.deprecationWarning(ddef.pos, ddef.symbol, s"$commonMessage ($explanation)")
        }
        computeMacroDefTypeFromMacroImplRef(ddef, rhs1) match {
          case ErrorType =>
            ErrorType
          case NothingTpe =>
            NothingTpe
          case NoType =>
            reportFailure();
            AnyTpe
          case tpe =>
            reportWarning(tpe);
            tpe
        }
      } else AnyTpe
    }
    def transformedOr(tree: Tree, op: => Tree): Tree = transformed.remove(tree) match {
      case Some(tree1) =>
        tree1
      case _ =>
        op
    }
    def transformedOrTyped(tree: Tree, mode: Mode, pt: Type): Tree = transformed.remove(tree) match {
      case Some(tree1) =>
        tree1
      case _ =>
        typed(tree, mode, pt)
    }
  }
}
object TypersStats {
  import scala.reflect.internal.TypesStats._
  val typedIdentCount = Statistics.newCounter("#typechecked identifiers")
  val typedSelectCount = Statistics.newCounter("#typechecked selections")
  val typedApplyCount = Statistics.newCounter("#typechecked applications")
  val rawTypeFailed = Statistics.newSubCounter("  of which in failed", rawTypeCount)
  val subtypeFailed = Statistics.newSubCounter("  of which in failed", subtypeCount)
  val findMemberFailed = Statistics.newSubCounter("  of which in failed", findMemberCount)
  val failedSilentNanos = Statistics.newSubTimer("time spent in failed", typerNanos)
  val failedApplyNanos = Statistics.newSubTimer("  failed apply", typerNanos)
  val failedOpEqNanos = Statistics.newSubTimer("  failed op=", typerNanos)
  val isReferencedNanos = Statistics.newSubTimer("time spent ref scanning", typerNanos)
  val visitsByType = Statistics.newByClass("#visits by tree node", "typer")(Statistics.newCounter(""))
  val byTypeNanos = Statistics.newByClass("time spent by tree node", "typer")(Statistics.newStackableTimer("", typerNanos))
  val byTypeStack = Statistics.newTimerStack()
}