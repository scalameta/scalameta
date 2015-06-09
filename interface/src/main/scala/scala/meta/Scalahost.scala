package scala.meta

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostSemanticContext}
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.semantic.{Context => ScalametaMacroContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostMacroContext}
import scala.meta.internal.hosts.scalac.contexts.{StandaloneContext => ScalametaStandaloneContext}
import scala.meta.internal.hosts.scalac.contexts.{StandaloneContext => ScalahostStandaloneContext}
import scala.meta.projects.{Context => ScalametaProjectContext}
import scala.meta.internal.hosts.scalac.contexts.{ProjectContext => ScalahostProjectContext}

object Scalahost {
  def mkGlobalContext[G <: ScalaGlobal](g: G): ScalametaSemanticContext with ScalahostSemanticContext[G] =
    new ScalahostSemanticContext[G](g)
  def mkMacroContext[G <: ScalaGlobal](rc: ScalareflectMacroContext): ScalametaMacroContext with ScalahostMacroContext[G] =
    new ScalahostMacroContext[G](rc.universe.asInstanceOf[G])
  def mkStandaloneContext(options: String = ""): ScalametaStandaloneContext =
    new ScalahostStandaloneContext(options)
  def mkProjectContext(sourcepath: String, classpath: String): ScalametaProjectContext with ScalametaSemanticContext with ScalahostProjectContext =
    new ScalahostProjectContext(sourcepath, classpath)
}
