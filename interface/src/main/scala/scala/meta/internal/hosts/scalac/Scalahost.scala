package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.macros.contexts.{Context => ScalaMacroContext}
import scala.meta.semantic.{Host => PalladiumHost}
import scala.meta.semantic.{MacroHost => PalladiumMacroHost}
import scala.meta.internal.hosts.scalac.{Host => OurHost, MacroHost => OurMacroHost}

object Scalahost {
  def apply[G <: ScalaGlobal](g: G): PalladiumHost with OurHost[G] = new OurHost[G](g)
  def apply[G <: ScalaGlobal](c: ScalaMacroContext): PalladiumMacroHost with OurMacroHost[G] = new OurMacroHost[G](c)
}