package scala.meta
package internal.hosts.scalac
package macros

import scala.reflect.internal.Mode
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}

trait MacroPlugin extends Typechecking with Expansion {
  self: ScalahostPlugin =>

  import global._
  import analyzer.{MacroPlugin => NscMacroPlugin, _}

  object scalahostMacroPlugin extends NscMacroPlugin {
    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = scalahostTypedMacroBody(typer, ddef)
    override def pluginsIsBlackbox(macroDef: Symbol): Option[Boolean] = scalahostIsBlackbox(macroDef)
    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = scalahostMacroExpand(typer, expandee, mode, pt)
  }
}
