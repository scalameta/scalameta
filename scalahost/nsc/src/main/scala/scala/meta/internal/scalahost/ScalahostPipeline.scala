package scala.meta.internal
package scalahost

import java.io._
import scala.collection.mutable
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.{meta => m}
import scala.meta.internal.semantic.DatabaseOps

trait ScalahostPipeline extends DatabaseOps { self: ScalahostPlugin =>

  object ScalahostComponent extends PluginComponent {
    val global: ScalahostPipeline.this.global.type = ScalahostPipeline.this.global
    val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "scalameta"
    override val description = "compute the scala.meta semantic database"
    def newPhase(_prev: Phase) = new ScalahostPhase(_prev)

    class ScalahostPhase(prev: Phase) extends StdPhase(prev) {
      val outputDir = global.settings.outputDirs.getSingleOutput
        .map(_.file)
        .getOrElse(new File(global.settings.d.value))
      val databaseRoot = new File(m.Database.locateInClasspath(outputDir))

      override def apply(unit: g.CompilationUnit): Unit = {
        val attributedSource = unit.toAttributedSource
        val databaseFile = new File(attributedSource.locateInDatabase(databaseRoot.toURI))
        attributedSource.writeToFile(databaseFile)
      }

      override def run(): Unit = {
        val oldDatabaseFiles = mutable.ListBuffer[File]()
        def collectDatabaseFiles(dir: File): Unit = {
          val files = Option(dir.listFiles()).getOrElse(Array[File]())
          files.foreach(f => {
            if (f.isDirectory) collectDatabaseFiles(f)
            else if (f.isFile && f.getName.endsWith(".semanticdb")) oldDatabaseFiles += f
          })
        }
        collectDatabaseFiles(databaseRoot)
        oldDatabaseFiles.foreach(oldDatabaseFile => {
          val oldScalaPath = m.AbsolutePath(
            new File(oldDatabaseFile.getAbsolutePath.stripSuffix(".semanticdb") + ".scala"))
          val oldScalaFile = oldScalaPath
            .toRelative(m.AbsolutePath(databaseRoot))
            .toAbsolute(scala.meta.internal.io.PathIO.workingDirectory)
            .toFile
          if (!oldScalaFile.exists) {
            def cleanupUpwards(file: File): Unit = {
              if (file.isFile) {
                file.delete()
              } else {
                if (file.getAbsolutePath == databaseRoot.getAbsolutePath) return
                if (file.listFiles().isEmpty) file.delete()
              }
              cleanupUpwards(file.getParentFile)
            }
            cleanupUpwards(oldDatabaseFile)
          }
        })
        super.run()
      }
    }
  }
}
