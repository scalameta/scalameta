package scala.meta
package internal.hosts.scalacompiler
package macros

import scala.reflect.internal.Mode
import scala.meta.internal.hosts.scalacompiler.{PluginBase => PalladiumPlugin}

trait MacroPlugin extends Typechecking with Expansion {
  self: PalladiumPlugin =>

  import global._
  import analyzer.{MacroPlugin => NscMacroPlugin, _}

  object palladiumMacroPlugin extends NscMacroPlugin {
    override def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = palladiumTypedMacroBody(typer, ddef)
    override def pluginsIsBlackbox(macroDef: Symbol): Option[Boolean] = palladiumIsBlackbox(macroDef)
    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = palladiumMacroExpand(typer, expandee, mode, pt)
  }
}
