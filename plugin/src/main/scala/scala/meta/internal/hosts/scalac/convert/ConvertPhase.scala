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
        // TODO: go through names instead, as trees might be different from one to the other. Parsing trees should
        // TODO. generate name
        // TODO: what is the best way to do that then? I can think of two ways:
        // TODO:    1. Using TQL, go through Semantic tree, find names, and use TQL again to go through the syntactic
        // TODO     tree to find equivalent name. This is however rather expensive, and does not account for empty
        // TODO     symbols positions.
        // TODO:    2. Generate a list of all symbols in the syntactic tree (in topdown, or at least the same way as
        // TODO:    TQL will traverse the semantic tree; then traverse the semantic tree and pop symbols one by one
        // TODO:    as they come

        // TODO: copy the source file into origin as well
        // TODO: this will put origin into name,s what for the test of the tree?

        def findInSyntactic(name: api.Name): Option[api.Name] = {
          val traverse = tql.collect {
            case nm: api.Name if nm.value == name.value => nm
          }.topDown
          traverse(syntacticTree).flatMap(_._2.headOption)
        }

        // TODO: remove
        /*println("=================================================================================================")
        println(semanticTree.show[Semantics])
        println("-------------------------------------------------------------------------------------------------")
        println(syntacticTree.show[Semantics])
        println("=================================================================================================")
        println(syntacticTree.origin.endLine)
        println(semanticTree.origin)*/
        //semanticTree.copy(origin = syntacticTree.origin)

        val replaceOrigin = tql.transform {
          case nm: api.Term.Name =>
            val ret = findInSyntactic(nm) match {
              case None => nm
              case Some(sm) => nm.copy(origin = sm.origin)
            }
            ret
            // TODO: add more cases
        }.topDown

        // TODO: move into test file
        val check = tql.transform {
          case nm: api.Name =>
            print(s"[${nm.origin.start}:${nm.origin.end}]")
            nm
        }.topDown
        val mergedTree = replaceOrigin(semanticTree).map(_._1).getOrElse(semanticTree)
        println("before ---------------------------------------------------------------------------------------------")
        check(semanticTree)
        println("after ----------------------------------------------------------------------------------------------")
        check(mergedTree)

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