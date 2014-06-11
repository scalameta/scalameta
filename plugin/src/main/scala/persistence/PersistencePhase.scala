package scala.reflect.internal.hosts
package scalacompiler
package persistence

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scalacompiler.{Plugin => PalladiumPlugin}
import scalahost.Scalahost
import scala.reflect.io.AbstractFile

trait PersistencePhase {
  self: PalladiumPlugin =>

  object PersistenceComponent extends NscPluginComponent {
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
    override val runsRightAfter = Some("renumber")
    val phaseName = "persist"
    override def description = "persist palladium abstract syntax trees"
    implicit val h = Scalahost[global.type](global)

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      private def target(unit: CompilationUnit): AbstractFile = {
        // TODO: this method does abstract away the real vs virtual input business
        // but unfortunately it isn't of terrible use for REPL at the moment
        // because the paths we get there are the trivial (memory)/<init> and (memory)/<console>
        val targetDir = settings.outputDirs.outputDirFor(unit.source.file)
        targetDir.fileNamed(unit.source.file.name.replace(".scala", "") + ".ast")
      }

      override def apply(unit: CompilationUnit) {
        // val pbody = h.toPalladium(unit.body)
        // ???
      }
    }
  }
}