package scala.meta.internal
package scalahost

import java.io._
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.meta.semantic.v1.Database
import scala.meta.internal.semantic.v1._
import scala.meta.internal.scalahost.v1.online.{Mirror => OnlineMirror}

trait ScalahostPipeline { self: ScalahostPlugin =>

  object ScalahostComponent extends PluginComponent {
    val global: ScalahostPipeline.this.global.type = ScalahostPipeline.this.global
    val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "scalameta"
    override val description = "compute the scala.meta semantic database"
    def newPhase(_prev: Phase) = new ScalahostPhase(_prev)

    class ScalahostPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        // TODO: compute and persist the database for every top-level class/module
      }

      override def run(): Unit = {
        super.run()
        val outputDir =
          global.settings.outputDirs.getSingleOutput.getOrElse(global.settings.d.value)
        val databaseFile = new File(outputDir + File.separator + "semanticdb")
        val prevDatabase =
          if (databaseFile.exists) Database.fromFile(databaseFile).get
          else Database()
        val database = new OnlineMirror(global).database
        val mergedDatabase = prevDatabase.append(database)
        val allowedPaths = global.currentRun.units.map(_.source.toAbsolutePath).toSet
        val trimmedDatabase = Database(
          mergedDatabase.names.filterKeys(k => allowedPaths.contains(k.path)),
          mergedDatabase.messages.filter(msg => allowedPaths.contains(msg.location.path)),
          mergedDatabase.denotations // TODO: filter out obsolete symbols
        )
        trimmedDatabase.writeDatabaseToFile(databaseFile)
      }
    }
  }
}
