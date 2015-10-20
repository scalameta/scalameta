// NOTE: has to be this package or otherwise we won't be able to access Context.issue
package scala.tools.nsc.typechecker

import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import org.scalameta.invariants._
import org.scalameta.reflection._
import scala.meta.internal.hosts.scalac.reflect._
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.reflect.internal.util.{Statistics, ListOfNil}
import scala.tools.nsc.typechecker.TypersStats._
import scala.reflect.internal.Flags
import scala.reflect.internal.Flags._
import scala.collection.mutable

trait ScalahostAnalyzer extends NscAnalyzer with ReflectToolkit {
  val global: Global
  import global._
  import definitions._

  val stableCurrentRun = global.currentRun
  import stableCurrentRun.runDefinitions._

  override def newTyper(context: Context) = new ScalahostTyper(context)
  class ScalahostTyper(context0: Context) extends Typer(context0) {
    import infer._
    import TyperErrorGen._

    override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
      def hasUndets           = context.undetparams.nonEmpty
      def hasUndetsInMonoMode = hasUndets && !mode.inPolyMode

      def adaptToImplicitMethod(mt: MethodType): Tree = {
        if (hasUndets) { // (9) -- should revisit dropped condition `hasUndetsInMonoMode`
          // dropped so that type args of implicit method are inferred even if polymorphic expressions are allowed
          // needed for implicits in 2.8 collection library -- maybe once #3346 is fixed, we can reinstate the condition?
            context.undetparams = inferExprInstance(tree, context.extractUndetparams(), pt,
              // approximate types that depend on arguments since dependency on implicit argument is like dependency on type parameter
              mt.approximate,
              keepNothings = false,
              useWeaklyCompatible = true) // #3808
        }

        // avoid throwing spurious DivergentImplicit errors
        if (context.reporter.hasErrors)
          setError(tree)
        else
          withCondConstrTyper(treeInfo.isSelfOrSuperConstrCall(tree))(typer1 =>
            if (original != EmptyTree && pt != WildcardType) (
              typer1 silent { tpr =>
                val withImplicitArgs = tpr.applyImplicitArgs(tree)
                if (tpr.context.reporter.hasErrors) tree // silent will wrap it in SilentTypeError anyway
                else tpr.typed(withImplicitArgs, mode, pt)
              }
              orElse { _ =>
                val resetTree = resetAttrs(original)
                resetTree match {
                  case treeInfo.Applied(fun, targs, args) =>
                    if (fun.symbol != null && fun.symbol.isError)
                      // SI-9041 Without this, we leak error symbols past the typer!
                      // because the fallback typechecking notices the error-symbol,
                      // refuses to re-attempt typechecking, and presumes that someone
                      // else was responsible for issuing the related type error!
                      fun.setSymbol(NoSymbol)
                  case _ =>
                }
                debuglog(s"fallback on implicits: ${tree}/$resetTree")
                val tree1 = typed(resetTree, mode)
                // Q: `typed` already calls `pluginsTyped` and `adapt`. the only difference here is that
                // we pass `EmptyTree` as the `original`. intended? added in 2009 (53d98e7d42) by martin.
                tree1 setType pluginsTyped(tree1.tpe, this, tree1, mode, pt)
                if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt, EmptyTree)
              }
            )
            else
              typer1.typed(typer1.applyImplicitArgs(tree), mode, pt)
          )
      }

      def instantiateToMethodType(mt: MethodType): Tree = {
        val meth = tree match {
          // a partial named application is a block (see comment in EtaExpansion)
          case Block(_, tree1) => tree1.symbol
          case _               => tree.symbol
        }
        if (!meth.isConstructor && (isFunctionType(pt) || samOf(pt).exists)) { // (4.2)
          debuglog(s"eta-expanding $tree: ${tree.tpe} to $pt")
          checkParamsConvertible(tree, tree.tpe)
          val tree0 = etaExpand(context.unit, tree, this)

          // #2624: need to infer type arguments for eta expansion of a polymorphic method
          // context.undetparams contains clones of meth.typeParams (fresh ones were generated in etaExpand)
          // need to run typer on tree0, since etaExpansion sets the tpe's of its subtrees to null
          // can't type with the expected type, as we can't recreate the setup in (3) without calling typed
          // (note that (3) does not call typed to do the polymorphic type instantiation --
          //  it is called after the tree has been typed with a polymorphic expected result type)
          if (hasUndets)
            instantiate(typed(tree0, mode), mode, pt)
          else
            typed(tree0, mode, pt)
        }
        else if (!meth.isConstructor && mt.params.isEmpty) // (4.3)
          adapt(typed(Apply(tree, Nil) setPos tree.pos), mode, pt, original)
        else if (context.implicitsEnabled)
          MissingArgsForMethodTpeError(tree, meth)
        else
          setError(tree)
      }

      def adaptType(): Tree = {
        // @M When not typing a type constructor (!context.inTypeConstructorAllowed)
        // or raw type, types must be of kind *,
        // and thus parameterized types must be applied to their type arguments
        // @M TODO: why do kind-* tree's have symbols, while higher-kinded ones don't?
        def properTypeRequired = (
             tree.hasSymbolField
          && !context.inTypeConstructorAllowed
          && !context.unit.isJava
        )
        // @M: don't check tree.tpe.symbol.typeParams. check tree.tpe.typeParams!!!
        // (e.g., m[Int] --> tree.tpe.symbol.typeParams.length == 1, tree.tpe.typeParams.length == 0!)
        // @M: removed check for tree.hasSymbolField and replace tree.symbol by tree.tpe.symbol
        // (TypeTree's must also be checked here, and they don't directly have a symbol)
        def kindArityMismatch = (
             context.inTypeConstructorAllowed
          && !sameLength(tree.tpe.typeParams, pt.typeParams)
        )
        // Note that we treat Any and Nothing as kind-polymorphic.
        // We can't perform this check when typing type arguments to an overloaded method before the overload is resolved
        // (or in the case of an error type) -- this is indicated by pt == WildcardType (see case TypeApply in typed1).
        def kindArityMismatchOk = tree.tpe.typeSymbol match {
          case NothingClass | AnyClass => true
          case _                       => pt == WildcardType
        }

        // todo. It would make sense when mode.inFunMode to instead use
        //    tree setType tree.tpe.normalize
        // when typechecking, say, TypeApply(Ident(`some abstract type symbol`), List(...))
        // because otherwise Ident will have its tpe set to a TypeRef, not to a PolyType, and `typedTypeApply` will fail
        // but this needs additional investigation, because it crashes t5228, gadts1 and maybe something else
        if (mode.inFunMode)
          tree
        else if (properTypeRequired && tree.symbol.typeParams.nonEmpty)  // (7)
          MissingTypeParametersError(tree)
        else if (kindArityMismatch && !kindArityMismatchOk)  // (7.1) @M: check kind-arity
          KindArityMismatchError(tree, pt)
        else tree match { // (6)
          case TypeTree() => tree
          case _          => TypeTree(tree.tpe) setOriginal tree
        }
      }

      def insertApply(): Tree = {
        assert(!context.inTypeConstructorAllowed, mode) //@M
        val adapted = adaptToName(tree, nme.apply)
        def stabilize0(pre: Type): Tree = stabilize(adapted, pre, MonoQualifierModes, WildcardType)

        // TODO reconcile the overlap between Typers#stablize and TreeGen.stabilize
        val qual = adapted match {
          case This(_) =>
            gen.stabilize(adapted)
          case Ident(_) =>
            val owner = adapted.symbol.owner
            val pre =
              if (owner.isPackageClass) owner.thisType
              else if (owner.isClass) context.enclosingSubClassContext(owner).prefix
              else NoPrefix
            stabilize0(pre)
          case Select(qualqual, _) =>
            stabilize0(qualqual.tpe)
          case other =>
            other
        }
        typedPos(tree.pos, mode, pt) {
          Select(qual setPos tree.pos.makeTransparent, nme.apply)
        }
      }
      def adaptConstant(value: Constant): Tree = {
        val sym = tree.symbol
        if (sym != null && sym.isDeprecated)
          context.deprecationWarning(tree.pos, sym)

        // NOTE: this is a meaningful difference from the code in Typers.scala
        //-treeCopy.Literal(tree, value)
        val result = treeCopy.Literal(tree, value)
        if (result.hasMetadata("original")) result
        else result.appendMetadata("original" -> tree)
      }

      // Ignore type errors raised in later phases that are due to mismatching types with existential skolems
      // We have lift crashing in 2.9 with an adapt failure in the pattern matcher.
      // Here's my hypothesis why this happens. The pattern matcher defines a variable of type
      //
      //   val x: T = expr
      //
      // where T is the type of expr, but T contains existential skolems ts.
      // In that case, this value definition does not typecheck.
      // The value definition
      //
      //   val x: T forSome { ts } = expr
      //
      // would typecheck. Or one can simply leave out the type of the `val`:
      //
      //   val x = expr
      //
      // SI-6029 shows another case where we also fail (in uncurry), but this time the expected
      // type is an existential type.
      //
      // The reason for both failures have to do with the way we (don't) transform
      // skolem types along with the trees that contain them. We'd need a
      // radically different approach to do it. But before investing a lot of time to
      // to do this (I have already sunk 3 full days with in the end futile attempts
      // to consistently transform skolems and fix 6029), I'd like to
      // investigate ways to avoid skolems completely.
      //
      // upd. The same problem happens when we try to typecheck the result of macro expansion against its expected type
      // (which is the return type of the macro definition instantiated in the context of expandee):
      //
      //   Test.scala:2: error: type mismatch;
      //     found   : $u.Expr[Class[_ <: Object]]
      //     required: reflect.runtime.universe.Expr[Class[?0(in value <local Test>)]] where type ?0(in value <local Test>) <: Object
      //     scala.reflect.runtime.universe.reify(new Object().getClass)
      //                                         ^
      // Therefore following Martin's advice I use this logic to recover from skolem errors after macro expansions
      // (by adding the ` || tree.attachments.get[MacroExpansionAttachment].isDefined` clause to the conditional above).
      //
      def adaptMismatchedSkolems() = {
        def canIgnoreMismatch = (
             !context.reportErrors && isPastTyper
          || tree.hasAttachment[MacroExpansionAttachment]
        )
        def bound = pt match {
          case ExistentialType(qs, _) => qs
          case _                      => Nil
        }
        def msg = sm"""
          |Recovering from existential or skolem type error in
          |  $tree
          |with type: ${tree.tpe}
          |       pt: $pt
          |  context: ${context.tree}
          |  adapted
          """.trim

        val boundOrSkolems = if (canIgnoreMismatch) bound ++ pt.skolemsExceptMethodTypeParams else Nil
        boundOrSkolems match {
          case Nil => AdaptTypeError(tree, tree.tpe, pt) ; setError(tree)
          case _   => logResult(msg)(adapt(tree, mode, deriveTypeWithWildcards(boundOrSkolems)(pt)))
        }
      }

      def fallbackAfterVanillaAdapt(): Tree = {
        def isPopulatedPattern = {
          if ((tree.symbol ne null) && tree.symbol.isModule)
            inferModulePattern(tree, pt)

          isPopulated(tree.tpe, approximateAbstracts(pt))
        }
        if (mode.inPatternMode && isPopulatedPattern)
          return tree

        val tree1 = constfold(tree, pt) // (10) (11)
        if (tree1.tpe <:< pt)
          return adapt(tree1, mode, pt, original)

        if (mode.typingExprNotFun) {
          // The <: Any requirement inhibits attempts to adapt continuation types
          // to non-continuation types.
          if (tree.tpe <:< AnyTpe) pt.dealias match {
            case TypeRef(_, UnitClass, _) => // (12)
              if (!isPastTyper && settings.warnValueDiscard)
                context.warning(tree.pos, "discarded non-Unit value")
              return typedPos(tree.pos, mode, pt)(Block(List(tree), Literal(Constant(()))))
            case TypeRef(_, sym, _) if isNumericValueClass(sym) && isNumericSubType(tree.tpe, pt) =>
              if (!isPastTyper && settings.warnNumericWiden)
                context.warning(tree.pos, "implicit numeric widening")
              return typedPos(tree.pos, mode, pt)(Select(tree, "to" + sym.name))
            case _ =>
          }
          if (pt.dealias.annotations.nonEmpty && canAdaptAnnotations(tree, this, mode, pt)) // (13)
            return typed(adaptAnnotations(tree, this, mode, pt), mode, pt)

          if (hasUndets)
            return instantiate(tree, mode, pt)

          if (context.implicitsEnabled && !pt.isError && !tree.isErrorTyped) {
            // (14); the condition prevents chains of views
            debuglog("inferring view from " + tree.tpe + " to " + pt)
            inferView(tree, tree.tpe, pt, reportAmbiguous = true) match {
              case EmptyTree =>
              case coercion  =>
                def msg = "inferred view from " + tree.tpe + " to " + pt + " = " + coercion + ":" + coercion.tpe
                if (settings.logImplicitConv)
                  context.echo(tree.pos, msg)

                debuglog(msg)
                val silentContext = context.makeImplicit(context.ambiguousErrors)
                val res = newTyper(silentContext).typed(
                  new ApplyImplicitView(coercion, List(tree)) setPos tree.pos, mode, pt)
                silentContext.reporter.firstError match {
                  case Some(err) => context.issue(err)
                  case None      => return res
                }
            }
          }
        }

        debuglog("error tree = " + tree)
        if (settings.debug && settings.explaintypes)
          explainTypes(tree.tpe, pt)

        if (tree.tpe.isErroneous || pt.isErroneous)
          setError(tree)
        else
          adaptMismatchedSkolems()
      }

      def vanillaAdapt(tree: Tree) = {
        def applyPossible = {
          def applyMeth = member(adaptToName(tree, nme.apply), nme.apply)
          def hasPolymorphicApply = applyMeth.alternatives exists (_.tpe.typeParams.nonEmpty)
          def hasMonomorphicApply = applyMeth.alternatives exists (_.tpe.paramSectionCount > 0)

          dyna.acceptsApplyDynamic(tree.tpe) || (
            if (mode.inTappMode)
              tree.tpe.typeParams.isEmpty && hasPolymorphicApply
            else
              hasMonomorphicApply
          )
        }
        def shouldInsertApply(tree: Tree) = mode.typingExprFun && {
          tree.tpe match {
            case _: MethodType | _: OverloadedType | _: PolyType => false
            case _                                               => applyPossible
          }
        }
        if (tree.isType)
          adaptType()
        else if (mode.typingExprNotFun && treeInfo.isMacroApplication(tree) && !isMacroExpansionSuppressed(tree))
          macroExpand(this, tree, mode, pt)
        else if (mode.typingConstructorPattern)
          typedConstructorPattern(tree, pt)
        else if (shouldInsertApply(tree))
          insertApply()
        else if (hasUndetsInMonoMode) { // (9)
          assert(!context.inTypeConstructorAllowed, context) //@M
          instantiatePossiblyExpectingUnit(tree, mode, pt)
        }
        else if (tree.tpe <:< pt)
          tree
        else
          fallbackAfterVanillaAdapt()
      }

      // begin adapt
      if (isMacroImplRef(tree)) {
        if (treeInfo.isMacroApplication(tree)) adapt(unmarkMacroImplRef(tree), mode, pt, original)
        else tree
      } else tree.tpe match {
        case atp @ AnnotatedType(_, _) if canAdaptAnnotations(tree, this, mode, pt) => // (-1)
          adaptAnnotations(tree, this, mode, pt)
        case ct @ ConstantType(value) if mode.inNone(TYPEmode | FUNmode) && (ct <:< pt) && canAdaptConstantTypeToLiteral => // (0)
          adaptConstant(value)
        case OverloadedType(pre, alts) if !mode.inFunMode => // (1)
          inferExprAlternative(tree, pt)
          adaptAfterOverloadResolution(tree, mode, pt, original)
        case NullaryMethodType(restpe) => // (2)
          adapt(tree setType restpe, mode, pt, original)
        case TypeRef(_, ByNameParamClass, arg :: Nil) if mode.inExprMode => // (2)
          adapt(tree setType arg, mode, pt, original)
        case tp if mode.typingExprNotLhs && isExistentialType(tp) =>
          adapt(tree setType tp.dealias.skolemizeExistential(context.owner, tree), mode, pt, original)
        case PolyType(tparams, restpe) if mode.inNone(TAPPmode | PATTERNmode) && !context.inTypeConstructorAllowed => // (3)
          // assert((mode & HKmode) == 0) //@M a PolyType in HKmode represents an anonymous type function,
          // we're in HKmode since a higher-kinded type is expected --> hence, don't implicitly apply it to type params!
          // ticket #2197 triggered turning the assert into a guard
          // I guess this assert wasn't violated before because type aliases weren't expanded as eagerly
          //  (the only way to get a PolyType for an anonymous type function is by normalisation, which applies eta-expansion)
          // -- are we sure we want to expand aliases this early?
          // -- what caused this change in behaviour??
          val tparams1 = cloneSymbols(tparams)
          val tree1 = (
            if (tree.isType) tree
            else TypeApply(tree, tparams1 map (tparam => TypeTree(tparam.tpeHK) setPos tree.pos.focus)) setPos tree.pos
          )
          context.undetparams ++= tparams1
          notifyUndetparamsAdded(tparams1)
          adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt, original)

        case mt: MethodType if mode.typingExprNotFunNotLhs && mt.isImplicit => // (4.1)
          adaptToImplicitMethod(mt)
        case mt: MethodType if mode.typingExprNotFunNotLhs && !hasUndetsInMonoMode && !treeInfo.isMacroApplicationOrBlock(tree) =>
          instantiateToMethodType(mt)
        case _ =>
          vanillaAdapt(tree)
      }
    }
  }
}