package scala.meta
package internal.hosts.scalac
package contexts

import java.io.File
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.taxonomic.{Context => TaxonomicContext}

object Compiler {
  def apply()(implicit taxonomy: TaxonomicContext): Global = {
    apply("")
  }

  def apply(options: String)(implicit taxonomy: TaxonomicContext): Global = {
    def fail(reason: String) = throw new InfrastructureException("can't initialize a semantic proxy: " + reason)
    val args = CommandLineParser.tokenize(options)
    val emptySettings = new Settings(error => fail("invalid compiler options: $error"))
    val reporter = new StoreReporter()
    val command = new CompilerCommand(args, emptySettings)
    val settings = command.settings
    if (settings.classpath.isDefault) settings.classpath.value = ""
    if (settings.usejavacp.isDefault) settings.usejavacp.value = false
    if (settings.nobootcp.isDefault) settings.nobootcp.value = true
    // settings.Ylogcp.value = true
    val global = new Global(settings, reporter)
    val run = new global.Run
    global.phase = run.picklerPhase // NOTE: need to set to something post-typer
    global.globalPhase = run.picklerPhase
    global
  }
}