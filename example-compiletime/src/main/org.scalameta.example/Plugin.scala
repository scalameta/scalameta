package org.scalameta.example

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import org.scalameta.reflection._

class Plugin(val global: Global) extends ScalahostPlugin with CompileTime {
  val name = "example"
  val description = """An example of using pre-alpha scala.meta APIs.
  For more information visit https://github.com/scalameta/example"""
  val components = List[NscPluginComponent](ConvertComponent, ExampleComponent)

  object ExampleComponent extends NscPluginComponent {
    val global: Plugin.this.global.type = Plugin.this.global
    import global._

    override val runsAfter = List("typer")
    override val runsRightAfter = Some("convert")
    val phaseName = "example"
    override def description = "an example of using pre-alpha scala.meta apis"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      var alreadyRun = false
      override def apply(unit: CompilationUnit) {
        if (!alreadyRun) {
          example(global.currentRun.units.toList.map(_.body.metadata("scalameta").asInstanceOf[scala.meta.internal.ast.Source]))
          alreadyRun = true
        }
      }
    }
  }
}