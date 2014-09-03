import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import org.scalameta.reflection.Metadata

class Plugin(val global: Global) extends NscPlugin with Metadata { self =>
  val name = "attatest"
  val description = "Tests attachments produced by scalahost"
  val components = List[NscPluginComponent](PluginComponent)
  object PluginComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._

    override val runsAfter = List("typer")
    override val runsRightAfter = None
    val phaseName = "attatest"
    override def description = "tests attachments produced by scalahost"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      import analyzer._
      override def apply(unit: CompilationUnit): Unit = {
        unit.body.foreach(tree => {
          if (hasMacroExpansionAttachment(tree)) {
            tree.metadata.toOption match {
              case None =>
                reporter.error(tree.pos, "macro expansion without a property bag")
              case Some(bag) =>
                if (!bag.contains("expandeeTree")) reporter.error(tree.pos, "macro expansion without expandeeTree in the property bag")
                if (!bag.contains("expandedTree")) reporter.error(tree.pos, "macro expansion without expandedTree in the property bag")
                if (!bag.contains("expansionString")) reporter.error(tree.pos, "macro expansion without expansionString in the property bag")
            }
          }
        })
      }
    }
  }
}
