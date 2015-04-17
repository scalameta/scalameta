package scala.meta
package internal.hosts.scalac
package convert

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._

import scala.meta.internal.{ ast => api}
import scala.meta.tql._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait ConvertPhase {
  self: ScalahostPlugin =>

  object ConvertComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._

    // TODO: ideally we would like to save everything after the very end of typechecking, which is after refchecks
    // but unfortunately by then a lot of semantic stuff is already desugared to death (patmat, superaccessors, some code in refchecks)
    // therefore we run after typer and hope for the best (i.e. that we don't run into nonsense that we don't know how to convert,
    // and also that we don't encounter residual cyclic reference errors which are the reason why certain typechecks are delayed past typer)
    // btw this isn't such a big problem for persistence, but it definitely is for macro interpretation
    // let's hope that the research into runtime macros, which entails moving the typechecker to scala-reflect.jar will allow us to restructure things
    // so that delayed typechecks come right after typer, not intermingled with other logic
    override val runsAfter = List("typer")
    override val runsRightAfter = None
    val phaseName = "convert"
    override def description = "convert compiler trees to scala.meta"
    implicit val c = Scalahost.mkGlobalContext[global.type](global)

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {

      private def merge(semanticTree: api.Source, syntacticTree: api.Source): Source = {
        // TODO: what is the best way to do that then? I can think of two ways:
        //     1. Using TQL, go through Semantic tree, find names, and use TQL again to go through the syntactic
        //      tree to find equivalent name. This is however rather expensive, and does not account for empty
        //      symbols positions.
        //
        //     2. Generate a list of all symbols in the syntactic tree (in topdown, or at least the same way as
        //     TQL will traverse the semantic tree; then traverse the semantic tree and pop symbols one by one
        //     as they come.
        //
        //  => in both cases, there is the problem of the desugaring of semantic informations (e.g. quasiquotes),
        // which leads us to have much more names in source code containing reflection. This should not be
        // the case for "normal" scala source code not involving reflection.

        // TODO: copy the source file into origin as well
        // TODO: this will put origin into name,s what for the test of the tree?

        /* ~~~~ SOLUTION 1 ~~~~ */

        /*def findInSyntactic(name: api.Name): Option[api.Name] = {
          val traverse = tql.collect {
            case nm: api.Name if nm.value == name.value => nm
          }.topDown
          traverse(syntacticTree).flatMap(_._2.headOption)
        }

        val replaceOrigin = tql.transform {
          case nm: api.Term.Name =>
            val ret = findInSyntactic(nm) match {
              case None => nm
              case Some(sm) => nm.copy(origin = sm.origin)
            }
            ret
        }.topDown
        val mergedTree = replaceOrigin(semanticTree).map(_._1).getOrElse(semanticTree)*/

        /* ~~~~ SOLUTION 2 ~~~~ */

        /*val syntacticNames: List[api.Name] = {
          val collect = tql.collect {
            case nm: api.Name => nm
          }.topDown
          collect(syntacticTree).map(_._2).getOrElse(Nil)
        }
        val semanticNames: List[api.Name] = {
          val collect = tql.collect {
            case nm: api.Name => nm
          }.topDown
          collect(semanticTree).map(_._2).getOrElse(Nil)
        }
        println(s"Lengths: ${syntacticNames.length}:${semanticNames.length}")
        if (syntacticNames.length != semanticNames.length)
          semanticNames.zip(syntacticNames) foreach (println(_))

        /* We can do better! */
        println(syntacticNames.forall(s => semanticNames.filter(x => x.value == s.value).length > 0)) // This is always false
        println(semanticNames.forall(s => syntacticNames.filter(x => x.value == s.value).length > 0)) // This is sometimes true
        */

        /* ~~~~ SOLUTION 3 ~~~~ */

        var syntacticCount = 0

        var syntacticNames: List[api.Name] = {
          val collect = tql.collect { case nm: api.Name => syntacticCount += 1;nm }.topDown
          collect(syntacticTree).map(_._2).getOrElse(Nil)
        }
        def findSyntacticEquivalent(nm: api.Name): Option[api.Name] = {
          val pos = syntacticNames.indexWhere(x => ClassTag(x.getClass) == ClassTag(nm.getClass) && x.value == nm.value)
          val elem = syntacticNames.lift(pos)
          syntacticNames = syntacticNames.drop(pos + 1)
          elem
        }

        var found = 0
        var notFound = 0

        val replaceOrigin = tql.transform {
          case nm: api.Name =>
            val ret = findSyntacticEquivalent(nm) match {
              case None => notFound = notFound + 1; nm
              case Some(x) => found = found + 1; nm match { /* Need to ensure case class to use copy constructor */
                case nms: api.Term.Name => nms.copy(origin = x.origin)
                case nms: api.Type.Name => nms.copy(origin = x.origin)
                case nms: api.Ctor.Ref.Name => nms.copy(origin = x.origin)
                case nms: api.Name.Anonymous => nms.copy(origin = x.origin)
                case nms: api.Name.Indeterminate => nms.copy(origin = x.origin)
              }
            }
            ret
        }.topDown

        val mergedTree = replaceOrigin(semanticTree).map(_._1).getOrElse(semanticTree)

        println(s"SyntacticCount: $syntacticCount, found symbols: $found, not found: $notFound")

        // TODO: remove
        /*println("=================================================================================================")
        println(semanticTree.show[Semantics])
        println("-------------------------------------------------------------------------------------------------")
        println(syntacticTree.show[Semantics])
        println("=================================================================================================")
        println(syntacticTree.origin.endLine)
        println(semanticTree.origin)*/
        //semanticTree.copy(origin = syntacticTree.origin)

        /*// TODO: move into test file
        val check = tql.transform {
          case nm: api.Name =>
            print(s"[${nm.origin.start}:${nm.origin.end}]")
            nm
        }.topDown
        println("\nbefore ---------------------------------------------------------------------------------------------")
        check(semanticTree)
        println("\nafter ----------------------------------------------------------------------------------------------")
        check(mergedTree)*/

        mergedTree.asInstanceOf[api.Source]
      }

      override def apply(unit: CompilationUnit) {
        val semanticTree = c.toMtree(unit.body, classOf[Source]).asInstanceOf[api.Source]
        val syntacticTree = unit.source.content.mkString("").parse[Source].asInstanceOf[api.Source]

        unit.body.appendMetadata("scalameta" -> merge(semanticTree, syntacticTree))
      }
    }
  }
}