package scala.meta
package internal.hosts.scalac
package convert

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._

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

      private def merge(semanticTree: Source, syntacticTree: Source): Source = {
        semanticTree
      }

      override def apply(unit: CompilationUnit) {
        // TODO: get token from content, infer position in generated tree below somehow
        // TODO: one easy way would be to parse tree using scalameta (tree lacking semantic info), and move tokens
        // TODO: from this parsed tree to the changed one

        // TODO: go through names instead, as trees might be different from one to the other. Parsing trees should
        // TODO. generate name
        val semanticTree = c.toMtree(unit.body, classOf[Source])
        val syntacticTree = unit.source.content.mkString("").parse[Source]

        unit.body.appendMetadata("scalameta" -> merge(semanticTree, syntacticTree))
      }
    }
  }
}