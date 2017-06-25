package scala.meta.internal
package scalahost

import scala.meta.internal.io.PathIO
import scala.meta.internal.semantic.SemanticdbMode
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalahostPlugin(val global: Global)
    extends Plugin
    with HijackAnalyzer
    with HijackReporter
    with ScalahostPipeline {
  val name = ScalahostPlugin.name
  val description = "scala.meta's connector to the Scala compiler"
  hijackAnalyzer()
  hijackReporter()
  val components = List[PluginComponent](ScalahostComponent)

  override def init(options: List[String], error: (String) => Unit): Boolean = {
    def err(msg: String): Unit = {
      g.reporter.error(g.NoPosition, s"[scalahost] $msg")
    }
    options.foreach {
      case SetSourceroot(path) =>
        val abspath = AbsolutePath(path)
        config.setSourceroot(abspath)
      case SetSemanticdb(SemanticdbMode(mode)) =>
        config.setSemanticdbMode(mode)
      case SetSemanticdb(els) =>
        err(s"Unknown semanticdb $els. Expected one of: ${SemanticdbMode.all.mkString(", ")} ")
      case els =>
        err(s"Ignoring unknown scalahost option $els")
    }
    true
  }
}

object ScalahostPlugin {
  val name = "scalahost"
}
