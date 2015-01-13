package scala.meta
package internal.hosts
package scalac

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.{SemanticContext => ScalahostSemanticContext}
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.macros.{Context => ScalametaMacroContext}
import scala.meta.internal.hosts.scalac.{MacroContext => ScalahostMacroContext}

object Scalahost {
  def mkSemanticContext[G <: ScalaGlobal](g: G): ScalametaSemanticContext with ScalahostSemanticContext[G] = new ScalahostSemanticContext[G](g)
  def mkMacroContext[G <: ScalaGlobal](rc: ScalareflectMacroContext): ScalametaMacroContext with ScalahostMacroContext[G] = new ScalahostMacroContext[G](rc)
}