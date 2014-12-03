package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import typechecker.HijackAnalyzer
import macros.{MacroPlugin => PalladiumMacroPlugin}
import macros.RenumberPhase
import persistence.PersistencePhase
import org.scalameta.reflection._

trait PluginBase extends NscPlugin
                    with HijackAnalyzer
                    with PalladiumMacroPlugin
                    with RenumberPhase
                    with PersistencePhase
                    with PluginSettings
                    with GlobalToolkit {
  val hijackedAnalyzer = hijackAnalyzer()
  if (global.analyzer ne hijackedAnalyzer) sys.error("failed to hijack analyzer")
  global.analyzer.addMacroPlugin(palladiumMacroPlugin)
}

class Plugin(val global: Global) extends PluginBase {
  val name = "scalahost"
  val description = """Hosts Project Palladium in scalac.
  For more information visit https://github.com/scalameta/scalahost"""
  val components = List[NscPluginComponent](RenumberComponent, PersistenceComponent)
}