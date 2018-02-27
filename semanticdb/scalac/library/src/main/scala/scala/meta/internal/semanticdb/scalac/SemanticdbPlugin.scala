package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.semanticdb._
import scala.meta.io.AbsolutePath
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

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
    config = SemanticdbConfig.parse(originalOptions, errFn)
    true
  }
}

object SemanticdbPlugin {
  val name = "semanticdb"
  val description = "Scalac 2.x compiler plugin that generates SemanticDB on compile"
}
