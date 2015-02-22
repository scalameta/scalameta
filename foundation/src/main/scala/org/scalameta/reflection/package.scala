package org.scalameta

import org.scalameta.invariants._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter

package object reflection {
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
