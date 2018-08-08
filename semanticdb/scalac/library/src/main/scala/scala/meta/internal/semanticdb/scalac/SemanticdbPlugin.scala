package scala.meta.internal.semanticdb.scalac

import java.io.File
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

class SemanticdbPlugin(val global: Global)
    extends Plugin
    with HijackAnalyzer
    with HijackReporter
    with SemanticdbPipeline {
  val name = SemanticdbPlugin.name
  val description = SemanticdbPlugin.description

  hijackAnalyzer()
  hijackReporter()
  val components = {
    if (isSupportedCompiler) List(SemanticdbTyperComponent, SemanticdbJvmComponent)
    else Nil
  }

  override def init(options: List[String], errFn: String => Unit): Boolean = {
    val originalOptions = options.map(option => "-P:" + name + ":" + option)
    val baseConfig = SemanticdbConfig.default.copy(targetroot = outputDirectory)
    config = SemanticdbConfig.parse(originalOptions, errFn, g.reporter, baseConfig)
    true
  }

  def isAmmonite: Boolean = {
    global.getClass.getName.startsWith("ammonite")
  }

  private def outputDirectory: AbsolutePath = {
    if (isAmmonite) {
      PathIO.workingDirectory.resolve("out/semanticdb-scalac")
    } else {
      AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(_.getAbsolutePath)
          .getOrElse(global.settings.d.value))
    }
  }

}

object SemanticdbPlugin {
  val name = "semanticdb"
  val description = "Scalac 2.x compiler plugin that generates SemanticDB on compile"
}
