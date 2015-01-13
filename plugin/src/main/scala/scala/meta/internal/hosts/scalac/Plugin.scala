package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import typechecker.HijackAnalyzer
import macros.{MacroPlugin => ScalahostMacroPlugin}
import convert.ConvertPhase
import org.scalameta.reflection._

trait PluginBase extends NscPlugin
                    with HijackAnalyzer
                    with ScalahostMacroPlugin
                    with ConvertPhase
                    with PluginSettings
                    with GlobalToolkit {
  val hijackedAnalyzer = hijackAnalyzer()
  if (global.analyzer ne hijackedAnalyzer) sys.error("failed to hijack analyzer")
  global.analyzer.addMacroPlugin(scalahostMacroPlugin)
}

class Plugin(val global: Global) extends PluginBase {
  val name = "scalahost"
  val description = """Hosts scala.meta in scalac.
  For more information visit https://github.com/scalameta/scalahost"""
  val components = List[NscPluginComponent](ConvertComponent)
}