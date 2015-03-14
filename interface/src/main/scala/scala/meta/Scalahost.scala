package scala.meta

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostSemanticContext}
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.macros.{Context => ScalametaMacroContext}
import scala.meta.internal.hosts.scalac.contexts.{MacroContext => ScalahostMacroContext}
import scala.meta.internal.hosts.scalac.contexts.{StandaloneContext => ScalametaStandaloneContext}

object Scalahost {
  def mkGlobalContext[G <: ScalaGlobal](g: G): ScalametaSemanticContext with ScalahostSemanticContext[G] =
    new ScalahostSemanticContext[G](g)
  def mkMacroContext[G <: ScalaGlobal](rc: ScalareflectMacroContext): ScalametaMacroContext with ScalahostMacroContext[G] =
    new ScalahostMacroContext[G](rc)
  def mkStandaloneContext(options: String = ""): ScalametaStandaloneContext =
    new ScalametaStandaloneContext(options)
}
