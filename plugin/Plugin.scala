package scala.meta
package internal.hosts.scalacompiler

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import typechecker.HijackAnalyzer
import macros.{MacroPlugin => PalladiumMacroPlugin}
import macros.RenumberPhase
import persistence.PersistencePhase

class Plugin(val global: Global) extends NscPlugin
                                    with HijackAnalyzer
                                    with PalladiumMacroPlugin
                                    with RenumberPhase
                                    with PersistencePhase
                                    with PluginSettings
                                    with Metadata {
  val name = "scalahost"
  val description = """Hosts Project Palladium in scalac.
  For more information visit https://github.com/scalareflect/scalahost"""
  val components = List[NscPluginComponent](RenumberComponent, PersistenceComponent)
  global.analyzer.addMacroPlugin(palladiumMacroPlugin)
  val hijackedAnalyzer = hijackAnalyzer()
  if (global.analyzer ne hijackedAnalyzer) sys.error("failed to hijack analyzer")
}
