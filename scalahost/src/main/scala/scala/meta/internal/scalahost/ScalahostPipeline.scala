package scala.meta.internal
package scalahost

import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent

trait ScalahostPipeline {
  self: ScalahostPlugin =>

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

        if (Configuration.dumpDatabase) {
          val database = new v1.OnlineMirror(global).database
          val grouped = database.symbols.groupBy(_._1.uri)
          grouped.keys.toList.sorted.foreach(uri => {
            println(uri.stripPrefix("file:"))
            val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
            val content = scala.io.Source.fromFile(new java.io.File(uri.stripPrefix("file:")))(codec).mkString
            grouped(uri).keys.toList.sortBy(_.start).foreach(k => {
              val snippet = content.substring(k.start, k.end)
              println(s"[${k.start}..${k.end}): $snippet => ${grouped(uri)(k).id}")
            })
            println()
          })
        }
      }
    }
  }
}
