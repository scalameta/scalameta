package scala.reflect.internal.hosts
package scalacompiler
package scalahost

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.macros.contexts.{Context => ScalaMacroContext}
import scala.reflect.semantic.{HostContext => PalladiumHostContext}
import scala.reflect.semantic.{MacroContext => PalladiumMacroContext}
import scalahost.{HostContext => OurHostContext, MacroContext => OurMacroContext}

object Scalahost {
  def apply[G <: ScalaGlobal](g: G): PalladiumHostContext with OurHostContext[G] = new OurHostContext[G](g)
  def apply[G <: ScalaGlobal](c: ScalaMacroContext): PalladiumMacroContext with OurMacroContext[G] = new OurMacroContext[G](c)
}