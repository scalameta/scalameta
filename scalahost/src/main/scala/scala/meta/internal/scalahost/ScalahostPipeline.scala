package scala.meta.internal
package scalahost

import java.io._
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.compat.Platform.EOL
import scala.meta.semantic.v1.Database

trait ScalahostPipeline { self: ScalahostPlugin =>

  object ScalahostComponent extends PluginComponent {
    val global: ScalahostPipeline.this.global.type = ScalahostPipeline.this.global
    val runsAfter                                  = List("typer")
    override val runsRightAfter                    = Some("typer")
    val phaseName                                  = "scalameta"
    override val description                       = "compute the scala.meta semantic database"
    def newPhase(_prev: Phase)                     = new ScalahostPhase(_prev)

    class ScalahostPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        // TODO: compute and persist the database for every top-level class/module
      }

      override def run(): Unit = {
        super.run()
        val databaseFile = new File(global.settings.d.value + "/semanticdb")
        val prevDatabase =
          if (databaseFile.exists) Database.readFile(databaseFile) else Database(Map())
        val database       = new v1.OnlineMirror(global).database
        val mergedDatabase = prevDatabase.append(database)
        val allowedUris    = global.currentRun.units.map(_.source.file.file.toURI.toString).toSet
        val trimmedDatabase = Database(
          mergedDatabase.symbols.filterKeys(k => allowedUris.contains(k.uri.toString)))
        Database.writeFile(databaseFile, trimmedDatabase)
      }
    }
  }
}
