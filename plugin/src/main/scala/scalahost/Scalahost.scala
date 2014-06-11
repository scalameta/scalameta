package scala.reflect.internal.hosts
package scalacompiler
package scalahost

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.macros.contexts.{Context => ScalaMacroContext}
import scala.reflect.semantic.{Host => PalladiumHost}
import scala.reflect.semantic.{MacroHost => PalladiumMacroHost}
import scalahost.{Host => OurHost, MacroHost => OurMacroHost}

object Scalahost {
  def apply[G <: ScalaGlobal](g: G): PalladiumHost with OurHost[G] = new OurHost[G](g)
  def apply[G <: ScalaGlobal](c: ScalaMacroContext): PalladiumMacroHost with OurMacroHost[G] = new OurMacroHost[G](c)
}