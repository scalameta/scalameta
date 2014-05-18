package scala.reflect.hosts
package scalacompiler

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.reflect.hosts.scalacompiler.parser.HijackSyntaxAnalyzer
import scala.reflect.hosts.scalacompiler.macros.HijackAnalyzer
import scala.reflect.hosts.scalacompiler.macros.{MacroPlugin => PalladiumMacroPlugin}

class Plugin(val global: Global) extends NscPlugin with HijackSyntaxAnalyzer with HijackAnalyzer with PalladiumMacroPlugin {
  val name = "scalahost"
  val description = """Hosts Project Palladium macros in scalac.
  For more information visit https://github.com/scalareflect/scalahost"""
  val components = List[NscPluginComponent]()
  hijackSyntaxAnalyzer()
  hijackAnalyzer()
  global.analyzer.addMacroPlugin(palladiumMacroPlugin)
}
