package scala.meta
package internal.hosts.scalac
package compat

import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.tools.nsc.{Global => NscGlobal}
import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}

trait ParadiseCompat {
  self: ScalahostPlugin =>

  import global._
  import analyzer.{AnalyzerPlugin => NscAnalyzerPlugin, MacroPlugin => NscMacroPlugin}

  def ifNecessaryReenableMacroParadise(oldAnalyzer: NscAnalyzer): Unit = {
    val methAnalyzerPlugins = oldAnalyzer.getClass.getDeclaredMethods.find(_.getName.endsWith("$analyzerPlugins")).head
    val analyzerPlugins = methAnalyzerPlugins.invoke(oldAnalyzer).asInstanceOf[List[NscAnalyzerPlugin]]
    val paradiseAnalyzerPlugin = analyzerPlugins.find(_.getClass.getName == "org.scalamacros.paradise.typechecker.AnalyzerPlugins$AnalyzerPlugin$")
    paradiseAnalyzerPlugin match {
      case Some(paradiseAnalyzerPlugin) =>
        // NOTE: this situation occurs when macro paradise was before scalahost in compiler options
        // this means it has already registered its analyzer and macro plugins in the analyzer that we've just hijacked
        // now we need to recreate those subplugins in the new analyzer
        // the easiest way of doing that is to reinstantiate the paradise plugin
        // its constructor will create and register new subplugins in the new analyzer
        val paradiseClassLoader = paradiseAnalyzerPlugin.getClass.getClassLoader
        val classParadisePlugin = Class.forName("org.scalamacros.paradise.Plugin", true, paradiseClassLoader)
        val ctorParadisePlugin = classParadisePlugin.getConstructors().find(_.getParameterTypes.toList == List(classOf[NscGlobal])).get
        ctorParadisePlugin.newInstance(global)
      case _ =>
        // NOTE: no need to do anything
        // macro paradise is either not in compiler options or will be loaded later
        // in both cases, natural order of events will ensure correct behavior
    }
  }

  def isNecessaryToDisableOurMacroPlugin(): Boolean = {
    // NOTE: if both scalahost and macro paradise are active, and macro paradise was registered after scalahost
    // then pluginsEnterStats will produce incorrect results
    // that's a bug in scalac, so we need to work around until the fix is published (https://github.com/scala/scala/pull/4353)
    val methMacroPlugins = analyzer.getClass.getDeclaredMethods.find(_.getName.endsWith("$macroPlugins")).head
    val macroPlugins = methMacroPlugins.invoke(analyzer).asInstanceOf[List[NscMacroPlugin]]
    if (macroPlugins.length > 1) {
      val stackTrace = Thread.currentThread().getStackTrace()
      val trimmed1 = stackTrace.dropWhile(f => f.getClassName != "scala.tools.nsc.typechecker.AnalyzerPlugins$class" && f.getMethodName != "invoke")
      val trimmed2 = trimmed1.dropWhile(f => f.getClassName == "scala.tools.nsc.typechecker.AnalyzerPlugins$class" && f.getMethodName == "invoke")
      val designation = trimmed2.headOption.map(_.getMethodName).getOrElse("")
      val needsWorkaround = designation == "pluginsEnterStats"
      needsWorkaround
    } else {
      false
    }
  }
}
