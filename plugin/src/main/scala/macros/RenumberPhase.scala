package scala.reflect.internal.hosts
package scalacompiler
package macros

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scalacompiler.{Plugin => PalladiumPlugin}
import scalahost.Scalahost
import scala.reflect.io.AbstractFile

trait RenumberPhase {
  self: PalladiumPlugin =>

  object RenumberComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._
    import Settings._

    override val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "renumber"
    override def description = "renumber positions of macro expansions"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        def fail(msg: String) = if (!reporter.hasErrors) reporter.error(NoPosition, msg)
        (YfictionalLineNumbers.value, YshiftingLineNumbers.value) match {
          case (true, true) => fail("can't have both fictional and shifting line numbers enabled")
          case (true, false) => fictional(unit)
          case (false, true) => fail("shifting line numbers aren't implemented yet")
          case _ => // do nothing
        }
      }
      private def fictional(unit: CompilationUnit): Unit = {
        ???
      }
    }
  }
}