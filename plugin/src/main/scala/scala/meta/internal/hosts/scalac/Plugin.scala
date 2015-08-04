package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalac.reflect._

trait PluginBase extends NscPlugin
                    with ConvertPhase
                    with HijackBackend
                    with PluginSettings
                    with GlobalToolkit {
  val (newBackend, oldBackend) = hijackBackend()
  // TODO: looks like it doesn't get hijacked cleanly...
  // if (global.genBCode ne newBackend) sys.error("failed to hijack backend")
}

class Plugin(val global: Global) extends PluginBase {
  val name = "scalahost"
  val description = """Hosts scala.meta in scalac.
  For more information visit https://github.com/scalameta/scalahost"""
  val components = List[NscPluginComponent](ConvertComponent)
}