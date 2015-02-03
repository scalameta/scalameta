package scala.meta
package internal.hosts.scalac
package macros

import scala.reflect.internal.Flags._
import scala.reflect.internal.Mode
import scala.reflect.internal.util.Collections._
import scala.reflect.internal.util.Statistics
import scala.tools.nsc.typechecker.MacrosStats._
import scala.reflect.runtime.ReflectionUtils
import scala.reflect.macros.runtime.AbortMacroException
import scala.util.control.ControlThrowable
import scala.collection.mutable
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.macros.{Context => ScalametaMacroContext}
import scala.meta.internal.hosts.scalac.{MacroContext => ScalahostMacroContext, PluginBase => ScalahostPlugin, Scalahost}

trait Expansion extends scala.reflect.internal.show.Printers {
  self: ScalahostPlugin =>

  import global._
  import definitions._
  import treeInfo._
  import analyzer.{MacroPlugin => NscMacroPlugin, _}

  def scalahostMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
    val TermQuote = "shadow scala.meta quasiquotes"
    val macroSignatures = expandee.symbol.annotations.filter(_.atp.typeSymbol == MacroImplAnnotation)
    val expanded = macroSignatures match {
      case _ :: AnnotationInfo(_, List(ScalahostSignature(implDdef)), _) :: Nil =>
        object scalahostMacroExpander extends DefMacroExpander(typer, expandee, mode, pt) {
          private def isDelayed(tree: Tree): Boolean = {
            val macros = Class.forName("scala.tools.nsc.typechecker.Macros$class", false, getClass.getClassLoader)
            val isDelayedMethod = macros.getDeclaredMethods().filter(_.getName == "isDelayed").head
            isDelayedMethod.setAccessible(true)
            isDelayedMethod.invoke(null, analyzer, tree).asInstanceOf[Boolean]
          }
          private def appendDelayed(tree: Tree, undets: mutable.Set[Int]): Unit = {
            val macros = Class.forName("scala.tools.nsc.typechecker.Macros", false, getClass.getClassLoader)
            val delayedGetter = macros.getDeclaredMethods().filter(_.getName == "scala$tools$nsc$typechecker$Macros$$delayed").head
            delayedGetter.setAccessible(true)
            val delayed = delayedGetter.invoke(this).asInstanceOf[mutable.WeakHashMap[Tree, scala.collection.mutable.Set[Int]]]
            delayed += tree -> undets
          }
          private def calculateUndetparams(expandee: Tree): mutable.Set[Int] = {
            val macros = Class.forName("scala.tools.nsc.typechecker.Macros$class", false, getClass.getClassLoader)
            val calculatedUndetparamsMethod = macros.getDeclaredMethods().filter(_.getName == "scala$tools$nsc$typechecker$Macros$$calculateUndetparams").head
            calculatedUndetparamsMethod.setAccessible(true)
            calculatedUndetparamsMethod.invoke(null, analyzer, expandee).asInstanceOf[mutable.Set[Int]]
          }
          private def macroArgs(): MacroArgs = {
            val treeInfo.Applied(core, _, _) = expandee
            val prefix = core match { case Select(qual, _) => qual; case _ => EmptyTree }
            val context = expandee.attachments.get[MacroRuntimeAttachment].flatMap(_.macroContext).getOrElse(macroContext(typer, prefix, expandee))
            MacroArgs(context, Nil)
          }
          // NOTE: magic name. essential for detailed and sane stack traces for exceptions in macro expansion logic
          private def macroExpandWithRuntime(rc: ScalareflectMacroContext): Any = {
            import global.{Tree => ScalaTree}
            import scala.meta.{Tree => ScalametaTree, Term => ScalametaTerm}
            import scala.meta.internal.eval.{eval => scalametaEval}
            import scala.meta.ui._
            import org.scalameta.unreachable
            lazy val hc = Scalahost.mkMacroContext[global.type](rc)
            lazy val scalareflectInvocation: ScalaTree = {
              // TODO: implement this
              // val applied @ Applied(core, targs, argss) = dissectApplied(expandee)
              // val implCore = q"${implDdef.symbol}" setType core.tpe
              // val implTapplied = q"$implCore[..$targs]" setType applied.callee.tpe
              // val margss = argss.map(_.map(arg => env.bind(arg))) :+ List(env.bind(hc))
              // val implApplied = q"$implTapplied(...$margss)" setType expandee.tpe
              // val scalaInvocation = q"{ $implDdef; $implApplied }" setType expandee.tpe
              implDdef.rhs
            }
            lazy val scalametaInvocation: ScalametaTree = {
              // TODO: implement this
              // hc.toScalameta(scalareflectInvocation)
              ???
            }
            lazy val scalametaResult: Any = scalametaInvocation match {
              case term: ScalametaTerm => scalametaEval(term)
              case _ => unreachable
            }
            lazy val scalareflectResult: Any = scalametaResult match {
              case scalametaTree: ScalametaTree =>
                // TODO: implement this
                // val scalareflectTree: ScalareflectTree = hc.fromScalameta(tree)
                // attachExpansionString(expandee, scalareflectTree, scalametaTree.show[Code])
                // scalareflectTree
                ???
              case other => other
            }
            // TODO: unhardcode this
            // scalareflectResult
            lazy val hardcodedResult = {
              val Applied(core, targs, List(List(x, y))) = dissectApplied(expandee)
              def fields(tree: Tree) = tree.tpe.members.collect{ case m: TermSymbol if m.isGetter => m }
              val xfields = fields(x).map(f => f -> q"xtemp")
              val yfields = fields(y).map(f => f -> q"ytemp")
              val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
              q"""
                val xtemp = $x
                val ytemp = $y
                new { ..$getters }
              """
            }
            hardcodedResult
          }
          override protected def expand(desugared: Tree): Tree = {
            def showDetailed(tree: Tree) = showRaw(tree, printIds = true, printTypes = true)
            def summary() = s"expander = $this, expandee = ${showDetailed(expandee)}, desugared = ${if (expandee == desugared) () else showDetailed(desugared)}"
            if (macroDebugVerbose) println(s"macroExpand: ${summary()}")
            linkExpandeeAndDesugared(expandee, desugared)
            val start = if (Statistics.canEnable) Statistics.startTimer(macroExpandNanos) else null
            if (Statistics.canEnable) Statistics.incCounter(macroExpandCount)
            try {
              withInfoLevel(nodePrinters.InfoLevel.Quiet) { // verbose printing might cause recursive macro expansions
                if (expandee.symbol.isErroneous || (expandee exists (_.isErroneous))) {
                  val reason = if (expandee.symbol.isErroneous) "not found or incompatible macro implementation" else "erroneous arguments"
                  macroLogVerbose(s"cancelled macro expansion because of $reason: $expandee")
                  onFailure(typer.infer.setError(expandee))
                } else try {
                  val expanded = {
                    val wasDelayed  = isDelayed(expandee)
                    val undetparams = calculateUndetparams(expandee)
                    val nowDelayed  = !typer.context.macrosEnabled || undetparams.nonEmpty
                    (wasDelayed, nowDelayed) match {
                      case (true, true) =>
                        Delay(expandee)
                      case (true, false) =>
                        val expanded = macroExpandAll(typer, expandee)
                        if (expanded exists (_.isErroneous)) Failure(expandee)
                        else Skip(expanded)
                      case (false, true) =>
                        macroLogLite("macro expansion is delayed: %s".format(expandee))
                        appendDelayed(expandee, undetparams)
                        expandee updateAttachment MacroRuntimeAttachment(delayed = true, typerContext = typer.context, macroContext = Some(macroArgs().c))
                        Delay(expandee)
                      case (false, false) =>
                        import typer.TyperErrorGen._
                        macroLogLite("performing macro expansion %s at %s".format(expandee, expandee.pos))
                        val args = macroArgs()
                        try {
                          val numErrors    = reporter.ERROR.count
                          def hasNewErrors = reporter.ERROR.count > numErrors
                          val expanded = { pushMacroContext(args.c); macroExpandWithRuntime(args.c) }
                          if (hasNewErrors) MacroGeneratedTypeError(expandee)
                          def validateResultingTree(expanded: Tree) = {
                            macroLogVerbose("original:")
                            macroLogLite("" + expanded + "\n" + showRaw(expanded))
                            val freeSyms = expanded.freeTerms ++ expanded.freeTypes
                            freeSyms foreach (sym => MacroFreeSymbolError(expandee, sym))
                            // Macros might have spliced arguments with range positions into non-compliant
                            // locations, notably, under a tree without a range position. Or, they might
                            // splice a tree that `resetAttrs` has assigned NoPosition.
                            //
                            // Here, we just convert all positions in the tree to offset positions, and
                            // convert NoPositions to something sensible.
                            //
                            // Given that the IDE now sees the expandee (by using -Ymacro-expand:discard),
                            // this loss of position fidelity shouldn't cause any real problems.
                            //
                            // Alternatively, we could pursue a way to exclude macro expansions from position
                            // invariant checking, or find a way not to touch expansions that happen to validate.
                            //
                            // This would be useful for cases like:
                            //
                            //    macro1 { macro2 { "foo" } }
                            //
                            // to allow `macro1` to see the range position of the "foo".
                            val expandedPos = enclosingMacroPosition.focus
                            def fixPosition(pos: Position) =
                              if (pos == NoPosition) expandedPos else pos.focus
                            expanded.foreach(t => t.pos = fixPosition(t.pos))

                            val result = atPos(enclosingMacroPosition.focus)(expanded)
                            Success(result)
                          }
                          expanded match {
                            case expanded: Expr[_] if expandee.symbol.isTermMacro => validateResultingTree(expanded.tree)
                            case expanded: Tree if expandee.symbol.isTermMacro => validateResultingTree(expanded)
                            case _ => MacroExpansionHasInvalidTypeError(expandee, expanded)
                          }
                        } catch {
                          case ex: Throwable =>
                            popMacroContext()
                            val realex = ReflectionUtils.unwrapThrowable(ex)
                            realex match {
                              case ex: AbortMacroException => MacroGeneratedAbort(expandee, ex)
                              case ex: ControlThrowable => throw ex
                              case ex: TypeError => MacroGeneratedTypeError(expandee, ex)
                              case _ => MacroGeneratedException(expandee, realex)
                            }
                        } finally {
                          expandee.removeAttachment[MacroRuntimeAttachment]
                        }
                      }
                  }
                  expanded match {
                    case Success(expanded) =>
                      // also see http://groups.google.com/group/scala-internals/browse_thread/thread/492560d941b315cc
                      val expanded1 = try onSuccess(duplicateAndKeepPositions(expanded)) finally popMacroContext()
                      if (!hasMacroExpansionAttachment(expanded1)) linkExpandeeAndExpanded(expandee, expanded1)
                      if (settings.Ymacroexpand.value == settings.MacroExpand.Discard) expandee.setType(expanded1.tpe)
                      else expanded1
                    case Fallback(fallback) => onFallback(fallback)
                    case Delayed(delayed) => onDelayed(delayed)
                    case Skipped(skipped) => onSkipped(skipped)
                    case Failure(failure) => onFailure(failure)
                  }
                } catch {
                  case typer.TyperErrorGen.MacroExpansionException => onFailure(expandee)
                }
              }
            } finally {
              if (Statistics.canEnable) Statistics.stopTimer(macroExpandNanos, start)
            }
          }
        }
        scalahostMacroExpander(expandee)
      case _ =>
        new DefMacroExpander(typer, expandee, mode, pt).apply(expandee)
    }
    val hasMeaningfulExpansion = hasMacroExpansionAttachment(expanded) && settings.Ymacroexpand.value != settings.MacroExpand.Discard
    if (hasMeaningfulExpansion) attachExpansionString(expandee, expanded, showCode(expanded))
    Some(expanded)
  }

  private class DefMacroExpander(typer: Typer, expandee: Tree, mode: Mode, outerPt: Type)
  extends analyzer.DefMacroExpander(typer, expandee, mode, outerPt) {
    override def onSuccess(expanded0: Tree) = {
      linkExpandeeAndExpanded(expandee, expanded0)
      val result = super.onSuccess(expanded0)
      attachExpansionString(expandee, expanded0, showCode(expanded0))
      expanded0.removeAttachment[MacroExpansionAttachment] // NOTE: removes MEA from the initial expansion wrapped in a blackbox ascription
      result
    }
  }

  private def linkExpandeeAndExpanded(expandee: Tree, expanded: Tree): Unit = {
    analyzer.linkExpandeeAndExpanded(expandee, expanded)
    syncPropertyBags(List(expandee, expanded), Map("expandeeTree" -> expandee, "expandedTree" -> expanded))
  }

  private def attachExpansionString(expandee: Tree, expanded: Tree, expansionString: String): Unit = {
    syncPropertyBags(List(expandee, expanded), Map("expansionString" -> expansionString))
  }

  private def syncPropertyBags(trees: List[Tree], extra: Map[String, Any]): Unit = {
    val merged = trees.map(_.metadata.toMap).reduce((b1, b2) => b1 ++ b2) ++ extra
    trees.foreach(_.metadata ++= merged)
  }
}
