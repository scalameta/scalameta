package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalac.reflect._

class Plugin(val global: Global) extends NscPlugin
                                    with ConvertPhase
                                    with HijackBackend
                                    with HijackAnalyzer
                                    with ParadiseCompat
                                    with PluginSettings
                                    with ReflectToolkit {
  val name = "scalahost"
  val description = """Hosts scala.meta in scalac.
  For more information visit https://github.com/scalameta/scalahost"""
  val components = List[NscPluginComponent](ConvertComponent)

  val (newAnalyzer, oldAnalyzer) = hijackAnalyzer()
  if (global.analyzer ne newAnalyzer) sys.error("failed to hijack analyzer")
  ifNecessaryReenableMacroParadise(oldAnalyzer)
  val (newBackend, oldBackend) = hijackBackend()
  // TODO: looks like it doesn't get hijacked cleanly...
  // if (global.genBCode ne newBackend) sys.error("failed to hijack backend")
}