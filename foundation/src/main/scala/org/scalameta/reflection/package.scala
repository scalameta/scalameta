package org.scalameta

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter

package object reflection {
  implicit class RichReflectionToolbox(tb: ToolBox[ru.type]) {
    def global: Global = {
      val m_withCompilerApi = tb.getClass.getDeclaredMethods.filter(_.getName == "withCompilerApi").head
      val withCompilerApi = m_withCompilerApi.invoke(tb)
      val m_api = withCompilerApi.getClass.getDeclaredMethods.filter(_.getName == "api").head
      m_api.setAccessible(true)
      val api = m_api.invoke(withCompilerApi)
      val m_compiler = api.getClass.getDeclaredMethods.filter(_.getName == "compiler").head
      m_compiler.invoke(api).asInstanceOf[scala.tools.nsc.Global]
    }
  }
  def mkGlobal(options: String): Global = {
    var compilerOptions = options
    if (!compilerOptions.contains("-Xplugin-require:scalahost")) {
      val scalahostJar = {
        try getClass.getProtectionDomain().getCodeSource().getLocation().getFile()
        catch { case ex: Throwable => throw new scala.Exception("failed to auto-load the scalahost plugin", ex) }
      }
      val scalahostOptions = " -Xplugin:" + scalahostJar + " -Xplugin-require:scalahost"
      compilerOptions += scalahostOptions
    }
    val customGlobal = {
      val args = CommandLineParser.tokenize(compilerOptions)
      val emptySettings = new Settings(error => sys.error("compilation has failed: " + error))
      val reporter = new StoreReporter()
      val command = new CompilerCommand(args, emptySettings)
      val settings = command.settings
      val global = new Global(settings, reporter)
      val run = new global.Run
      global.phase = run.parserPhase
      global.globalPhase = run.parserPhase
      global
    }
    customGlobal
  }
}
