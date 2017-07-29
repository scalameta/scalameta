// NOTE: has to be this package or otherwise we won't be able to access private[typechecker] methods
package scala
package tools.nsc
package typechecker

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import scala.reflect.internal.Mode._
import scala.reflect.internal.util._
import scala.reflect.internal.Flags._
import scala.meta.internal.ReflectionToolkit

// NOTE: grep for "scalac deviation" to see what exactly has been changed
// I had to copy/paste a lot of code from Namers and Typers, but only a small percentage is actually relevant

trait ScalahostAnalyzer extends NscAnalyzer with ReflectionToolkit {
  import global._
  import definitions._
  import TypersStats._

  override def newNamer(context: Context) = new ScalahostNamer(context)
  class ScalahostNamer(context0: Context) extends Namer(context0) {
    override def selfTypeCompleter(tree: Tree) = new ScalahostSelfTypeCompleter(tree)
    class ScalahostSelfTypeCompleter(tree: Tree) extends SelfTypeCompleter(tree) {
      override def completeImpl(sym: Symbol): Unit = {
        val tree1 = typer.typedType(tree)
        //+scalac deviation
        tree.rememberSelfTypeOf(tree1.asInstanceOf[TypeTree].original)
        //-scalac deviation
        val selftpe = tree1.tpe
        sym setInfo {
          if (selftpe.typeSymbol isNonBottomSubClass sym.owner) selftpe
          else intersectionType(List(sym.owner.tpe, selftpe))
        }
      }
    }
  }

  override def newTyper(context: Context) = new ScalahostTyper(context)
  class ScalahostTyper(context0: Context) extends Typer(context0) {
    import infer._
    import TyperErrorGen._
    import typeDebug.ptTree
    import runDefinitions._

    // TODO: No idea whether this is gonna work well in the batch compilation mode.
    override def canTranslateEmptyListToNil = false

    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      def typedSingletonTypeTree(tree: SingletonTypeTree) = {
        val refTyped =
          context.withImplicitsDisabled {
            typed(tree.ref, MonoQualifierModes | mode.onlyTypePat, AnyRefTpe)
          }

        if (refTyped.isErrorTyped) {
          setError(tree)
        } else {
          //+scalac deviation
          tree setType refTyped.tpe.resultType.deconst rememberSingletonTypeTreeOf refTyped
          //-scalac deviation
          if (refTyped.isErrorTyped || treeInfo.admitsTypeSelection(refTyped)) tree
          else UnstableTreeError(tree)
        }
      }

      def tryTypedArgs(args: List[Tree], mode: Mode): Option[List[Tree]] = {
        val c = context.makeSilent(reportAmbiguousErrors = false)
        c.retyping = true
        try {
          val res = newTyper(c).typedArgs(args, mode)
          if (c.reporter.hasErrors) None else Some(res)
        } catch {
          case ex: CyclicReference =>
            throw ex
          case te: TypeError =>
            // @H some of typer errors can still leak,
            // for instance in continuations
            None
        }
      }

      /* Try to apply function to arguments; if it does not work, try to convert Java raw to existentials, or try to
       * insert an implicit conversion.
       */
      def tryTypedApply(fun: Tree, args: List[Tree]): Tree = {
        val start = if (Statistics.canEnable) Statistics.startTimer(failedApplyNanos) else null

        def onError(typeErrors: Seq[AbsTypeError], warnings: Seq[(Position, String)]): Tree = {
          if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, start)

          // If the problem is with raw types, convert to existentials and try again.
          // See #4712 for a case where this situation arises,
          if ((fun.symbol ne null) && fun.symbol.isJavaDefined) {
            val newtpe = rawToExistential(fun.tpe)
            if (fun.tpe ne newtpe) {
              // println("late cooking: "+fun+":"+fun.tpe) // DEBUG
              return tryTypedApply(fun setType newtpe, args)
            }
          }
          def treesInResult(tree: Tree): List[Tree] = tree :: (tree match {
            case Block(_, r)                        => treesInResult(r)
            case Match(_, cases)                    => cases
            case CaseDef(_, _, r)                   => treesInResult(r)
            case Annotated(_, r)                    => treesInResult(r)
            case If(_, t, e)                        => treesInResult(t) ++ treesInResult(e)
            case Try(b, catches, _)                 => treesInResult(b) ++ catches
            case Typed(r, Function(Nil, EmptyTree)) => treesInResult(r) // a method value
            case Select(qual, name)                 => treesInResult(qual)
            case Apply(fun, args)                   => treesInResult(fun) ++ args.flatMap(treesInResult)
            case TypeApply(fun, args)               => treesInResult(fun) ++ args.flatMap(treesInResult)
            case _                                  => Nil
          })
          def errorInResult(tree: Tree) = treesInResult(tree) exists (err => typeErrors.exists(_.errPos == err.pos))

          val retry = (typeErrors.forall(_.errPos != null)) && (fun :: tree :: args exists errorInResult)
          typingStack.printTyping({
            val funStr = ptTree(fun) + " and " + (args map ptTree mkString ", ")
            if (retry) "second try: " + funStr
            else "no second try: " + funStr + " because error not in result: " + typeErrors.head.errPos+"!="+tree.pos
          })
          if (retry) {
            val Select(qual, name) = fun
            tryTypedArgs(args, forArgMode(fun, mode)) match {
              case Some(args1) if !args1.exists(arg => arg.exists(_.isErroneous)) =>
                val qual1 =
                  if (!pt.isError) adaptToArguments(qual, name, args1, pt)
                  else qual
                if (qual1 ne qual) {
                  val tree1 = Apply(Select(qual1, name) setPos fun.pos, args1) setPos tree.pos
                  return context withinSecondTry typed1(tree1, mode, pt)
                }
              case _ => ()
            }
          }
          typeErrors foreach context.issue
          warnings foreach { case (p, m) => context.warning(p, m) }
          setError(treeCopy.Apply(tree, fun, args))
        }

        silent(_.doTypedApply(tree, fun, args, mode, pt)) match {
          case SilentResultValue(value) => value
          case e: SilentTypeError => onError(e.errors, e.warnings)
        }
      }

      def normalTypedApply(tree: Tree, fun: Tree, args: List[Tree]) = {
        // TODO: replace `fun.symbol.isStable` by `treeInfo.isStableIdentifierPattern(fun)`
        val stableApplication = (fun.symbol ne null) && fun.symbol.isMethod && fun.symbol.isStable
        val funpt = if (mode.inPatternMode) pt else WildcardType
        val appStart = if (Statistics.canEnable) Statistics.startTimer(failedApplyNanos) else null
        val opeqStart = if (Statistics.canEnable) Statistics.startTimer(failedOpEqNanos) else null

        def isConversionCandidate(qual: Tree, name: Name): Boolean =
          !mode.inPatternMode && nme.isOpAssignmentName(TermName(name.decode)) && !qual.exists(_.isErroneous)

        def reportError(error: SilentTypeError): Tree = {
          error.reportableErrors foreach context.issue
          error.warnings foreach { case (p, m) => context.warning(p, m) }
          args foreach (arg => typed(arg, mode, ErrorType))
          setError(tree)
        }
        def advice1(convo: Tree, errors: List[AbsTypeError], err: SilentTypeError): List[AbsTypeError] =
          errors.map { e =>
            if (e.errPos == tree.pos) {
              val header = f"${e.errMsg}%n  Expression does not convert to assignment because:%n    "
              val expansion = f"%n    expansion: ${show(convo)}"
              NormalTypeError(tree, err.errors.flatMap(_.errMsg.lines.toList).mkString(header, f"%n    ", expansion))
            } else e
          }
        def advice2(errors: List[AbsTypeError]): List[AbsTypeError] =
          errors.map { e =>
            if (e.errPos == tree.pos) {
              val msg = f"${e.errMsg}%n  Expression does not convert to assignment because receiver is not assignable."
              NormalTypeError(tree, msg)
            } else e
          }
        def onError(error: SilentTypeError): Tree = fun match {
          case Select(qual, name) if isConversionCandidate(qual, name) =>
            val qual1 = typedQualifier(qual)
            if (treeInfo.isVariableOrGetter(qual1)) {
              if (Statistics.canEnable) Statistics.stopTimer(failedOpEqNanos, opeqStart)
              val erred = qual1.exists(_.isErroneous) || args.exists(_.isErroneous)
              if (erred) reportError(error) else {
                val convo = convertToAssignment(fun, qual1, name, args)
                silent(op = _.typed1(convo, mode, pt)) match {
                  case SilentResultValue(t) => t
                  case err: SilentTypeError => reportError(
                    SilentTypeError(advice1(convo, error.errors, err), error.warnings)
                  )
                }
              }
            } else {
              if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, appStart)
              val Apply(Select(qual2, _), args2) = tree
              val erred = qual2.exists(_.isErroneous) || args2.exists(_.isErroneous)
              reportError {
                if (erred) error else SilentTypeError(advice2(error.errors), error.warnings)
              }
            }
          case _ =>
            if (Statistics.canEnable) Statistics.stopTimer(failedApplyNanos, appStart)
            reportError(error)
        }
        val silentResult = silent(
          op                    = _.typed(fun, mode.forFunMode, funpt),
          reportAmbiguousErrors = !mode.inExprMode && context.ambiguousErrors,
          newtree               = if (mode.inExprMode) tree else context.tree
        )
        silentResult match {
          case SilentResultValue(fun1) =>
            val fun2 = if (stableApplication) stabilizeFun(fun1, mode, pt) else fun1
            if (Statistics.canEnable) Statistics.incCounter(typedApplyCount)
            val noSecondTry = (
                 isPastTyper
              || context.inSecondTry
              || (fun2.symbol ne null) && fun2.symbol.isConstructor
              || isImplicitMethodType(fun2.tpe)
            )
            val isFirstTry = fun2 match {
              case Select(_, _) => !noSecondTry && mode.inExprMode
              case _            => false
            }
            if (isFirstTry)
              tryTypedApply(fun2, args)
            else
              doTypedApply(tree, fun2, args, mode, pt)
          case err: SilentTypeError => onError(err)
        }
      }

      // convert new Array[T](len) to evidence[ClassTag[T]].newArray(len)
      // convert new Array^N[T](len) for N > 1 to evidence[ClassTag[Array[...Array[T]...]]].newArray(len)
      // where Array HK gets applied (N-1) times
      object ArrayInstantiation {
        def unapply(tree: Apply) = tree match {
          case Apply(Select(New(tpt), name), arg :: Nil) if tpt.tpe != null && tpt.tpe.typeSymbol == ArrayClass =>
            Some(tpt.tpe) collect {
              case erasure.GenericArray(level, componentType) =>
                val tagType = (1 until level).foldLeft(componentType)((res, _) => arrayType(res))

                resolveClassTag(tree.pos, tagType) match {
                  case EmptyTree => MissingClassTagError(tree, tagType)
                  case tag       => atPos(tree.pos)(new ApplyToImplicitArgs(Select(tag, nme.newArray), arg :: Nil))
                }
            }
          case _ => None
        }
      }

      def typedApply(tree: Apply) = tree match {
        case Apply(Block(stats, expr), args) =>
          typed1(atPos(tree.pos)(Block(stats, Apply(expr, args) setPos tree.pos.makeTransparent)), mode, pt)
        case Apply(fun, args) =>
          normalTypedApply(tree, fun, args) match {
            case tree1 @ ArrayInstantiation(tree2) =>
              if (tree2.isErrorTyped) tree2
              //+scalac deviation
              else typed(tree2, mode, pt).rememberNewArrayOf(tree1)
              //-scalac deviation
            case Apply(Select(fun, nme.apply), _) if treeInfo.isSuperConstrCall(fun) =>
              TooManyArgumentListsForConstructor(tree) //SI-5696
            case tree1 =>
              tree1
          }
      }

      def convertToAssignment(fun: Tree, qual: Tree, name: Name, args: List[Tree]): Tree = {
        val prefix = name.toTermName stripSuffix nme.EQL
        def mkAssign(vble: Tree): Tree =
          Assign(
            vble,
            Apply(
              Select(vble.duplicate, prefix) setPos fun.pos.focus, args) setPos tree.pos.makeTransparent
          ) setPos tree.pos

        def mkUpdate(table: Tree, indices: List[Tree]) =
          gen.evalOnceAll(table :: indices, context.owner, context.unit) {
            case tab :: is =>
              def mkCall(name: Name, extraArgs: Tree*) = (
                Apply(
                  Select(tab(), name) setPos table.pos,
                  is.map(i => i()) ++ extraArgs
                ) setPos tree.pos
              )
              mkCall(
                nme.update,
                Apply(Select(mkCall(nme.apply), prefix) setPos fun.pos, args) setPos tree.pos
              )
            case _ => EmptyTree
          }

        val assignment = qual match {
          case Ident(_) =>
            mkAssign(qual)

          case Select(qualqual, vname) =>
            gen.evalOnce(qualqual, context.owner, context.unit) { qq =>
              val qq1 = qq()
              mkAssign(Select(qq1, vname) setPos qual.pos)
            }

          case Apply(fn, indices) =>
            fn match {
              case treeInfo.Applied(Select(table, nme.apply), _, _) => mkUpdate(table, indices)
              case _  => UnexpectedTreeAssignmentConversionError(qual)
            }
        }
        assignment
      }

      def typedAnnotated(atd: Annotated): Tree = {
        val ann = atd.annot
        val arg1 = typed(atd.arg, mode, pt)
        /* mode for typing the annotation itself */
        val annotMode = (mode &~ TYPEmode) | EXPRmode

        def resultingTypeTree(tpe: Type) = {
          // we need symbol-ful originals for reification
          // hence we go the extra mile to hand-craft this guy
          val original = arg1 match {
            case tt @ TypeTree() if tt.original != null => Annotated(ann, tt.original)
            // this clause is needed to correctly compile stuff like "new C @D" or "@(inline @getter)"
            case _ => Annotated(ann, arg1)
          }
          original setType ann.tpe
          val result = TypeTree(tpe) setOriginal original setPos tree.pos.focus
          //+scalac deviation
          result rememberAnnotatedOf treeCopy.Annotated(atd, tpe.annotations.head.original, arg1)
          //-scalac deviation
        }

        if (arg1.isType) {
          // make sure the annotation is only typechecked once
          if (ann.tpe == null) {
            val ainfo = typedAnnotation(ann, annotMode)
            val atype = arg1.tpe.withAnnotation(ainfo)

            if (ainfo.isErroneous)
              // Erroneous annotations were already reported in typedAnnotation
              arg1  // simply drop erroneous annotations
            else {
              ann setType atype
              resultingTypeTree(atype)
            }
          } else {
            // the annotation was typechecked before
            resultingTypeTree(ann.tpe)
          }
        }
        else {
          if (ann.tpe == null) {
            val annotInfo = typedAnnotation(ann, annotMode)
            ann setType arg1.tpe.withAnnotation(annotInfo)
          }
          val atype = ann.tpe
          // For `f(): @inline/noinline` callsites, add the InlineAnnotatedAttachment. TypeApplys
          // are eliminated by erasure, so add it to the underlying function in this case.
          def setInlineAttachment(t: Tree, att: InlineAnnotatedAttachment): Unit = t match {
            case TypeApply(fun, _) => setInlineAttachment(fun, att)
            case _ => t.updateAttachment(att)
          }
          if (atype.hasAnnotation(definitions.ScalaNoInlineClass)) setInlineAttachment(arg1, NoInlineCallsiteAttachment)
          else if (atype.hasAnnotation(definitions.ScalaInlineClass)) setInlineAttachment(arg1, InlineCallsiteAttachment)
          Typed(arg1, resultingTypeTree(atype)) setPos tree.pos setType atype
        }
      }

      // Lookup in the given class using the root mirror.
      def lookupInOwner(owner: Symbol, name: Name): Symbol =
        if (mode.inQualMode) rootMirror.missingHook(owner, name) else NoSymbol

      // Lookup in the given qualifier.  Used in last-ditch efforts by typedIdent and typedSelect.
      def lookupInRoot(name: Name): Symbol  = lookupInOwner(rootMirror.RootClass, name)
      def lookupInEmpty(name: Name): Symbol = rootMirror.EmptyPackageClass.info member name

      def lookupInQualifier(qual: Tree, name: Name): Symbol = (
        if (name == nme.ERROR || qual.tpe.widen.isErroneous)
          NoSymbol
        else lookupInOwner(qual.tpe.typeSymbol, name) orElse {
          NotAMemberError(tree, qual, name)
          NoSymbol
        }
      )

      def inCompanionForJavaStatic(pre: Type, cls: Symbol, name: Name): Symbol =
        if (!(context.unit.isJava && cls.isClass && !cls.isModuleClass)) NoSymbol else {
          val companion = companionSymbolOf(cls, context)
          if (!companion.exists) NoSymbol
          else member(gen.mkAttributedRef(pre, companion), name) // assert(res.isStatic, s"inCompanionJavaStatic($pre, $cls, $name) = $res ${res.debugFlagString}")
        }

      /* Attribute a selection where `tree` is `qual.name`.
       * `qual` is already attributed.
       */
      def typedSelect(tree: Tree, qual: Tree, name: Name): Tree = {
        val t = typedSelectInternal(tree, qual, name)
        // Checking for OverloadedTypes being handed out after overloading
        // resolution has already happened.
        if (isPastTyper) t.tpe match {
          case OverloadedType(pre, alts) =>
            if (alts forall (s => (s.owner == ObjectClass) || (s.owner == AnyClass) || isPrimitiveValueClass(s.owner))) ()
            else if (settings.debug) printCaller(
              s"""|Select received overloaded type during $phase, but typer is over.
                  |If this type reaches the backend, we are likely doomed to crash.
                  |$t has these overloads:
                  |${alts map (s => "  " + s.defStringSeenAs(pre memberType s)) mkString "\n"}
                  |""".stripMargin
            )("")
          case _ =>
        }
        t
      }

      def typedSelectInternal(tree: Tree, qual: Tree, name: Name): Tree = {
        def asDynamicCall = dyna.mkInvoke(context, tree, qual, name) map { t =>
          dyna.wrapErrors(t, (_.typed1(t, mode, pt)))
        }

        val sym = tree.symbol orElse member(qual, name) orElse inCompanionForJavaStatic(qual.tpe.prefix, qual.symbol, name) orElse {
          // symbol not found? --> try to convert implicitly to a type that does have the required
          // member.  Added `| PATTERNmode` to allow enrichment in patterns (so we can add e.g., an
          // xml member to StringContext, which in turn has an unapply[Seq] method)
          if (name != nme.CONSTRUCTOR && mode.inAny(EXPRmode | PATTERNmode)) {
            val qual1 = adaptToMemberWithArgs(tree, qual, name, mode)
            if ((qual1 ne qual) && !qual1.isErrorTyped)
              return typed(treeCopy.Select(tree, qual1, name), mode, pt)
          }
          NoSymbol
        }
        if (phase.erasedTypes && qual.isInstanceOf[Super] && tree.symbol != NoSymbol)
          qual setType tree.symbol.owner.tpe

        if (!reallyExists(sym)) {
          def handleMissing: Tree = {
            def errorTree = missingSelectErrorTree(tree, qual, name)
            def asTypeSelection = (
              if (context.unit.isJava && name.isTypeName) {
                // SI-3120 Java uses the same syntax, A.B, to express selection from the
                // value A and from the type A. We have to try both.
                atPos(tree.pos)(gen.convertToSelectFromType(qual, name)) match {
                  case EmptyTree => None
                  case tree1     => Some(typed1(tree1, mode, pt))
                }
              }
              else None
            )
            debuglog(s"""
              |qual=$qual:${qual.tpe}
              |symbol=${qual.tpe.termSymbol.defString}
              |scope-id=${qual.tpe.termSymbol.info.decls.hashCode}
              |members=${qual.tpe.members mkString ", "}
              |name=$name
              |found=$sym
              |owner=${context.enclClass.owner}
              """.stripMargin)

            // 1) Try converting a term selection on a java class into a type selection.
            // 2) Try expanding according to Dynamic rules.
            // 3) Try looking up the name in the qualifier.
            asTypeSelection orElse asDynamicCall getOrElse (lookupInQualifier(qual, name) match {
              case NoSymbol => setError(errorTree)
              case found    => typed1(tree setSymbol found, mode, pt)
            })
          }
          handleMissing
        }
        else {
          val tree1 = tree match {
            case Select(_, _) => treeCopy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
          }
          val (result, accessibleError) = silent(_.asInstanceOf[ScalahostTyper].makeAccessible(tree1, sym, qual.tpe, qual)) match {
            case SilentTypeError(err: AccessTypeError) =>
              (tree1, Some(err))
            case SilentTypeError(err) =>
              SelectWithUnderlyingError(tree, err)
              return tree
            case SilentResultValue(treeAndPre) =>
              (stabilize(treeAndPre._1, treeAndPre._2, mode, pt), None)
          }

          result match {
            // could checkAccessible (called by makeAccessible) potentially have skipped checking a type application in qual?
            case SelectFromTypeTree(qual@TypeTree(), name) if qual.tpe.typeArgs.nonEmpty => // TODO: somehow the new qual is not checked in refchecks
              treeCopy.SelectFromTypeTree(
                result,
                (TypeTreeWithDeferredRefCheck(){ () => val tp = qual.tpe; val sym = tp.typeSymbolDirect
                  // will execute during refchecks -- TODO: make private checkTypeRef in refchecks public and call that one?
                  checkBounds(qual, tp.prefix, sym.owner, sym.typeParams, tp.typeArgs, "")
                  qual // you only get to see the wrapped tree after running this check :-p
                }) setType qual.tpe setPos qual.pos,
                name)
            case _ if accessibleError.isDefined =>
              // don't adapt constructor, SI-6074
              val qual1 = if (name == nme.CONSTRUCTOR) qual
                          else adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = false, saveErrors = false)
              if (!qual1.isErrorTyped && (qual1 ne qual))
                typed(Select(qual1, name) setPos tree.pos, mode, pt)
              else
                // before failing due to access, try a dynamic call.
                asDynamicCall getOrElse {
                  context.issue(accessibleError.get)
                  setError(tree)
                }
            case _ =>
              result
          }
        }
      }

      // the qualifier type of a supercall constructor is its first parent class
      def typedSelectOrSuperQualifier(qual: Tree) =
        context withinSuperInit typed(qual, PolyQualifierModes)

      def typedSelectOrSuperCall(tree: Select) = tree match {
        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          // the qualifier type of a supercall constructor is its first parent class
          typedSelect(tree, typedSelectOrSuperQualifier(qual), nme.CONSTRUCTOR)
        case Select(qual, name) =>
          if (Statistics.canEnable) Statistics.incCounter(typedSelectCount)
          val qualTyped = checkDead(typedQualifier(qual, mode))
          val qualStableOrError = (
            if (qualTyped.isErrorTyped || !name.isTypeName || treeInfo.admitsTypeSelection(qualTyped))
              qualTyped
            else
              UnstableTreeError(qualTyped)
          )
          val tree1 = typedSelect(tree, qualStableOrError, name)
          def sym = tree1.symbol
          if (tree.isInstanceOf[PostfixSelect])
            checkFeature(tree.pos, PostfixOpsFeature, name.decode)
          if (sym != null && sym.isOnlyRefinementMember && !sym.isMacro)
            checkFeature(tree1.pos, ReflectiveCallsFeature, sym.toString)

          qualStableOrError.symbol match {
            //+scalac deviation
            case s: Symbol if s.isRootPackage => treeCopy.Ident(tree1, name).rememberSelectOf(tree1)
            case _                            => tree1
            //-scalac deviation
          }
      }

      tree match {
        case tree: SingletonTypeTree => typedSingletonTypeTree(tree)
        case tree: Apply => typedApply(tree)
        case tree: Annotated => typedAnnotated(tree)
        case tree: Select => typedSelectOrSuperCall(tree)
        case _ => super.typed1(tree, mode, pt)
      }
    }

    /** Make symbol accessible. This means:
     *  If symbol refers to package object, insert `.package` as second to last selector.
     *  (exception for some symbols in scala package which are dealiased immediately)
     *  Call checkAccessible, which sets tree's attributes.
     *  Also note that checkAccessible looks up sym on pre without checking that pre is well-formed
     *  (illegal type applications in pre will be skipped -- that's why typedSelect wraps the resulting tree in a TreeWithDeferredChecks)
     *  @return modified tree and new prefix type
     */
    private def makeAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): (Tree, Type) =
      if (context.isInPackageObject(sym, pre.typeSymbol)) {
        if (pre.typeSymbol == ScalaPackageClass && sym.isTerm) {
          // short cut some aliases. It seems pattern matching needs this
          // to notice exhaustiveness and to generate good code when
          // List extractors are mixed with :: patterns. See Test5 in lists.scala.
          //
          // TODO SI-6609 Eliminate this special case once the old pattern matcher is removed.
          def dealias(sym: Symbol) = {
            //+scalac deviation
            val tree1 = atPos(tree.pos.makeTransparent) {gen.mkAttributedRef(sym)} setPos tree.pos
            tree1.rememberSelectOf(tree)
            (tree1, sym.owner.thisType)
            //-scalac deviation
          }
          sym.name match {
            case nme.List => return dealias(ListModule)
            case nme.Seq  => return dealias(SeqModule)
            case nme.Nil  => return dealias(NilModule)
            case _ =>
          }
        }
        val qual = typedQualifier { atPos(tree.pos.makeTransparent) {
          tree match {
            case Ident(_) =>
              val packageObject =
                if (!sym.isOverloaded && sym.owner.isModuleClass) sym.owner.sourceModule // historical optimization, perhaps no longer needed
                else pre.typeSymbol.packageObject
              Ident(packageObject)
            case Select(qual, _) => Select(qual, nme.PACKAGEkw)
            case SelectFromTypeTree(qual, _) => Select(qual, nme.PACKAGEkw)
          }
        }}
        val tree1 = atPos(tree.pos) {
          tree match {
            case Ident(name) => Select(qual, name)
            case Select(_, name) => Select(qual, name)
            case SelectFromTypeTree(_, name) => SelectFromTypeTree(qual, name)
          }
        }
        (checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe)
      } else {
        (checkAccessible(tree, sym, pre, site), pre)
      }

    override protected def typedTypeApply(tree: Tree, mode: Mode, fun: Tree, args: List[Tree]): Tree = fun.tpe match {
      case OverloadedType(pre, alts) =>
        inferPolyAlternatives(fun, mapList(args)(treeTpe))

        // SI-8267 `memberType` can introduce existentials *around* a PolyType/MethodType, see AsSeenFromMap#captureThis.
        //         If we had selected a non-overloaded symbol, `memberType` would have been called in `makeAccessible`
        //         and the resulting existential type would have been skolemized in `adapt` *before* we typechecked
        //         the enclosing type-/ value- application.
        //
        //         However, if the selection is overloaded, we defer calling `memberType` until we can select a single
        //         alternative here. It is therefore necessary to skolemize the existential here.
        //
        val fun1 = adaptAfterOverloadResolution(fun, mode.forFunMode | TAPPmode)

        val tparams = fun1.symbol.typeParams //@M TODO: fun.symbol.info.typeParams ? (as in typedAppliedTypeTree)
        val args1 = if (sameLength(args, tparams)) {
          //@M: in case TypeApply we can't check the kind-arities of the type arguments,
          // as we don't know which alternative to choose... here we do
          map2Conserve(args, tparams) {
            //@M! the polytype denotes the expected kind
            (arg, tparam) => typedHigherKindedType(arg, mode, Kind.FromParams(tparam.typeParams))
          }
        } else // @M: there's probably something wrong when args.length != tparams.length... (triggered by bug #320)
         // Martin, I'm using fake trees, because, if you use args or arg.map(typedType),
         // inferPolyAlternatives loops...  -- I have no idea why :-(
         // ...actually this was looping anyway, see bug #278.
          return TypedApplyWrongNumberOfTpeParametersError(fun, fun)

        typedTypeApply(tree, mode, fun1, args1)
      case SingleType(_, _) =>
        typedTypeApply(tree, mode, fun setType fun.tpe.widen, args)
      case PolyType(tparams, restpe) if tparams.nonEmpty =>
        if (sameLength(tparams, args)) {
          val targs = mapList(args)(treeTpe)
          checkBounds(tree, NoPrefix, NoSymbol, tparams, targs, "")
          if (isPredefClassOf(fun.symbol)) {
            //+scalac deviation
            val result = typedClassOf(tree, args.head, noGen = true)
            val original = atPos(tree.pos)(TypeApply(fun, args))
            result.rememberClassOf(original)
            //-scalac deviation
          } else {
            if (!isPastTyper && fun.symbol == Any_isInstanceOf && targs.nonEmpty) {
              val scrutineeType = fun match {
                case Select(qual, _) => qual.tpe
                case _               => AnyTpe
              }
              checkCheckable(tree, targs.head, scrutineeType, inPattern = false)
            }
            val resultpe = restpe.instantiateTypeParams(tparams, targs)
            //@M substitution in instantiateParams needs to be careful!
            //@M example: class Foo[a] { def foo[m[x]]: m[a] = error("") } (new Foo[Int]).foo[List] : List[Int]
            //@M    --> first, m[a] gets changed to m[Int], then m gets substituted for List,
            //          this must preserve m's type argument, so that we end up with List[Int], and not List[a]
            //@M related bug: #1438
            //println("instantiating type params "+restpe+" "+tparams+" "+targs+" = "+resultpe)
            treeCopy.TypeApply(tree, fun, args) setType resultpe
          }
        }
        else {
          TypedApplyWrongNumberOfTpeParametersError(tree, fun)
        }
      case ErrorType =>
        setError(treeCopy.TypeApply(tree, fun, args))
      case _ =>
        fun match {
          // drop the application for an applyDynamic or selectDynamic call since it has been pushed down
          case treeInfo.DynamicApplication(_, _) => fun
          case _ => TypedApplyDoesNotTakeTpeParametersError(tree, fun)
        }
    }

    override protected def typedExistentialTypeTree(tree: ExistentialTypeTree, mode: Mode): Tree = {
      for (wc <- tree.whereClauses)
        if (wc.symbol == NoSymbol) { namer enterSym wc; wc.symbol setFlag EXISTENTIAL }
        else context.scope enter wc.symbol
      val whereClauses1 = typedStats(tree.whereClauses, context.owner)
      for (vd @ ValDef(_, _, _, _) <- whereClauses1)
        if (vd.symbol.tpe.isVolatile)
          AbstractionFromVolatileTypeError(vd)
      val tpt1 = typedType(tree.tpt, mode)
      existentialTransform(whereClauses1 map (_.symbol), tpt1.tpe)((tparams, tp) => {
        val original = tpt1 match {
          case tpt : TypeTree => atPos(tree.pos)(ExistentialTypeTree(tpt.original, tree.whereClauses))
          case _ => {
            debuglog(s"cannot reconstruct the original for $tree, because $tpt1 is not a TypeTree")
            tree
          }
        }
        val result = TypeTree(newExistentialType(tparams, tp)) setOriginal original
        //+scalac deviation
        result.rememberExistentialTypeTreeOf(treeCopy.ExistentialTypeTree(tree, tpt1, whereClauses1.asInstanceOf[List[MemberDef]]))
        //-scalac deviation
      }
      )
    }

    /** Perform the following adaptations of expression, pattern or type `tree` wrt to
     *  given mode `mode` and given prototype `pt`:
     *  (-1) For expressions with annotated types, let AnnotationCheckers decide what to do
     *  (0) Convert expressions with constant types to literals (unless in interactive/scaladoc mode)
     *  (1) Resolve overloading, unless mode contains FUNmode
     *  (2) Apply parameterless functions
     *  (3) Apply polymorphic types to fresh instances of their type parameters and
     *      store these instances in context.undetparams,
     *      unless followed by explicit type application.
     *  (4) Do the following to unapplied methods used as values:
     *  (4.1) If the method has only implicit parameters pass implicit arguments
     *  (4.2) otherwise, if `pt` is a function type and method is not a constructor,
     *        convert to function by eta-expansion,
     *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt`
     *        and it is not a constructor, apply it to ()
     *  otherwise issue an error
     *  (5) Convert constructors in a pattern as follows:
     *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
     *        instance of its primary constructor that is a subtype of the expected type.
     *  (5.2) If constructor refers to an extractor, convert to application of
     *        unapply or unapplySeq method.
     *
     *  (6) Convert all other types to TypeTree nodes.
     *  (7) When in TYPEmode but not FUNmode or HKmode, check that types are fully parameterized
     *      (7.1) In HKmode, higher-kinded types are allowed, but they must have the expected kind-arity
     *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
     *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
     *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
     *  (10) If the expected type is Byte, Short or Char, and the expression
     *      is an integer fitting in the range of that type, convert it to that type.
     *  (11) Widen numeric literals to their expected type, if necessary
     *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is scala.Unit.
     *  (13) When in mode EXPRmode, apply AnnotationChecker conversion if expected type is annotated.
     *  (14) When in mode EXPRmode, do SAM conversion
     *  (15) When in mode EXPRmode, apply a view
     *  If all this fails, error
     */
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
                // SO-10066 Need to patch the enclosing tree in the context to make translation of Dynamic
                //          work during fallback typechecking below.
                val resetContext: Context = {
                  object substResetForOriginal extends Transformer {
                    override def transform(tree: Tree): Tree = {
                      if (tree eq original) resetTree
                      else super.transform(tree)
                    }
                  }
                  context.make(substResetForOriginal.transform(context.tree))
                }
                typerWithLocalContext(resetContext) { typer1 =>
                  val tree1 = typer1.typed(resetTree, mode)
                  // Q: `typed` already calls `pluginsTyped` and `adapt`. the only difference here is that
                  // we pass `EmptyTree` as the `original`. intended? added in 2009 (53d98e7d42) by martin.
                  tree1 setType pluginsTyped(tree1.tpe, typer1, tree1, mode, pt)
                  if (tree1.isEmpty) tree1 else typer1.asInstanceOf[ScalahostTyper].adapt(tree1, mode, pt, EmptyTree)
                }
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

        def cantAdapt =
          if (context.implicitsEnabled) MissingArgsForMethodTpeError(tree, meth)
          else setError(tree)

        // constructors do not eta-expand
        if (meth.isConstructor) cantAdapt
        // (4.2) eta-expand method value when function or sam type is expected
        else if (isFunctionType(pt) || (!mt.params.isEmpty && samOf(pt).exists)) {
          // SI-9536 `!mt.params.isEmpty &&`: for backwards compatiblity with 2.11,
          // we don't adapt a zero-arg method value to a SAM
          // In 2.13, we won't do any eta-expansion for zero-arg method values, but we should deprecate first

          debuglog(s"eta-expanding $tree: ${tree.tpe} to $pt")
          checkParamsConvertible(tree, tree.tpe)

          // SI-7187 eta-expansion of zero-arg method value is deprecated, switch order of (4.3) and (4.2) in 2.13
          def isExplicitEtaExpansion = original match {
            case Typed(_, Function(Nil, EmptyTree)) => true // tree shape for `f _`
            case _ => false
          }
          if (mt.params.isEmpty && !isExplicitEtaExpansion) {
            currentRun.reporting.deprecationWarning(tree.pos, NoSymbol,
              s"Eta-expansion of zero-argument method values is deprecated. Did you intend to write ${Apply(tree, Nil)}?", "2.12.0")
          }

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
        // (4.3) apply to empty argument list -- TODO 2.13: move this one case up to avoid eta-expanding at arity 0
        else if (mt.params.isEmpty) adapt(typed(Apply(tree, Nil) setPos tree.pos), mode, pt, original)
        else cantAdapt
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
        val qual = gen.stabilize(adapted)
        typedPos(tree.pos, mode, pt) {
          Select(qual setPos tree.pos.makeTransparent, nme.apply)
        }
      }
      def adaptConstant(value: Constant): Tree = {
        val sym = tree.symbol
        if (sym != null && sym.isDeprecated)
          context.deprecationWarning(tree.pos, sym)

        val result = treeCopy.Literal(tree, value)
        //+scalac deviation
        result.rememberConstfoldOf(tree)
        //-scalac deviation
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

      def adaptExprNotFunMode(): Tree = {
        def lastTry(err: AbsTypeError = null): Tree = {
          debuglog("error tree = " + tree)
          if (settings.debug && settings.explaintypes) explainTypes(tree.tpe, pt)
          if (err ne null) context.issue(err)
          if (tree.tpe.isErroneous || pt.isErroneous) setError(tree)
          else adaptMismatchedSkolems()
        }

        // TODO: should we even get to fallbackAfterVanillaAdapt for an ill-typed tree?
        if (mode.typingExprNotFun && !tree.tpe.isErroneous) {
          @inline def tpdPos(transformed: Tree) = typedPos(tree.pos, mode, pt)(transformed)
          @inline def tpd(transformed: Tree)    = typed(transformed, mode, pt)

          @inline def warnValueDiscard(): Unit = if (!isPastTyper && settings.warnValueDiscard) {
            def isThisTypeResult = (tree, tree.tpe) match {
              case (Apply(Select(receiver, _), _), SingleType(_, sym)) => sym == receiver.symbol
              case _ => false
            }
            if (!isThisTypeResult) context.warning(tree.pos, "discarded non-Unit value")
          }
          @inline def warnNumericWiden(): Unit =
            if (!isPastTyper && settings.warnNumericWiden) context.warning(tree.pos, "implicit numeric widening")

          // The <: Any requirement inhibits attempts to adapt continuation types to non-continuation types.
          val anyTyped = tree.tpe <:< AnyTpe

          pt.dealias match {
            case TypeRef(_, UnitClass, _) if anyTyped => // (12)
              warnValueDiscard() ; tpdPos(gen.mkUnitBlock(tree))
            case TypeRef(_, numValueCls, _) if anyTyped && isNumericValueClass(numValueCls) && isNumericSubType(tree.tpe, pt) => // (10) (11)
              warnNumericWiden() ; tpdPos(Select(tree, s"to${numValueCls.name}"))
            case dealiased if dealiased.annotations.nonEmpty && canAdaptAnnotations(tree, this, mode, pt) => // (13)
              tpd(adaptAnnotations(tree, this, mode, pt))
            case _ =>
              if (hasUndets) instantiate(tree, mode, pt)
              else {
                // (14) sam conversion
                // TODO: figure out how to avoid partially duplicating typedFunction (samMatchingFunction)
                // Could we infer the SAM type, assign it to the tree and add the attachment,
                // all in one fell swoop at the end of typedFunction?
                val samAttach = inferSamType(tree, pt, mode)

                if (samAttach.samTp ne NoType) tree.setType(samAttach.samTp).updateAttachment(samAttach)
                else {  // (15) implicit view application
                  val coercion =
                    if (context.implicitsEnabled) inferView(tree, tree.tpe, pt)
                    else EmptyTree
                  if (coercion ne EmptyTree) {
                    def msg = s"inferred view from ${tree.tpe} to $pt via $coercion: ${coercion.tpe}"
                    if (settings.logImplicitConv) context.echo(tree.pos, msg)
                    else debuglog(msg)

                    val viewApplied = new ApplyImplicitView(coercion, List(tree)) setPos tree.pos
                    val silentContext = context.makeImplicit(context.ambiguousErrors)
                    val typedView = newTyper(silentContext).typed(viewApplied, mode, pt)

                    silentContext.reporter.firstError match {
                      case None => typedView
                      case Some(err) => lastTry(err)
                    }
                  } else lastTry()
                }
              }
          }
        } else lastTry()
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
        else if (mode.inPatternMode && { inferModulePattern(tree, pt); isPopulated(tree.tpe, approximateAbstracts(pt)) })
          tree
        else {
          val constFolded = constfold(tree, pt)
          if (constFolded.tpe <:< pt) adapt(constFolded, mode, pt, original) // set stage for (0)
          else adaptExprNotFunMode() // (10) -- (15)
        }
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
        case tp if mode.typingExprNotLhs && isExistentialType(tp) &&
             // TODO: this is a private method, unfortunately: !isSyntheticAccessor(context.owner) =>
             !(context.owner.isAccessor && (!context.owner.isLazy || isPastTyper)) =>
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
