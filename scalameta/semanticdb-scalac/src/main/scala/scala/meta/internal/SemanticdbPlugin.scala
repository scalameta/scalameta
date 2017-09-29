package scala.meta.internal

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
  val description = "Scala 2.x compiler plugin that generates semanticdb on compile"

  hijackAnalyzer()
  hijackReporter()
  val components = {
    if (isBatchCompiler) List(ComputeSemanticdbComponent, PersistSemanticdbComponent)
    else Nil
  }

  override def init(options: List[String], error: (String) => Unit): Boolean = {
    def err(msg: String): Unit = {
      g.reporter.error(g.NoPosition, s"[semanticdb] $msg")
    }
    options.foreach {
      case SetSourceroot(path) =>
        val abspath = AbsolutePath(path)
        config.setSourceroot(abspath)
      case SetMode(SemanticdbMode(mode)) =>
        config.setMode(mode)
      case SetMode(els) =>
        err(s"Unknown mode $els. Expected one of: ${SemanticdbMode.all.mkString(", ")} ")
      case SetFailures(FailureMode(severity)) =>
        config.setFailures(severity)
      case SetDenotations(DenotationMode(denotations)) =>
        config.setDenotations(denotations)
      case SetProfiling(ProfilingMode(profiling)) =>
        config.setProfiling(profiling)
      case els =>
        err(s"Ignoring unknown option $els")
    }
    true
  }
}

object SemanticdbPlugin {
  val name = "semanticdb"
}
