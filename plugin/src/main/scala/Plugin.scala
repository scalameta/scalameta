package scala.tools.nsc.scalahost

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}

class Plugin(val global: Global) extends NscPlugin { self =>
  import global._
  import definitions._
  import analyzer.{AnalyzerPlugin => NscAnalyzerPlugin, MacroPlugin => NscMacroPlugin, _}
  import scala.reflect.internal.Mode
  import scala.reflect.internal.Flags._

  val name = "scalahost"
  val description = """Hosts Project Palladium macros in scalac.
  For more information visit https://github.com/scalareflect/scalahost"""
  val components = List[NscPluginComponent]()
  analyzer.addMacroPlugin(MacroPlugin)

  object MacroPlugin extends NscMacroPlugin {
    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      println(s"[scalahost] macroexpanding $expandee")
      val expander = new DefMacroExpander(typer, expandee, mode, pt)
      Some(expander(expandee))
    }
  }
}
