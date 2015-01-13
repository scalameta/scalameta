package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.macros.{Context => ScalametaMacroContext}
import scala.meta.internal.hosts.scalac.{SemanticContext => ScalahostSemanticContext}

class MacroContext[G <: ScalaGlobal](val scalareflectMacroContext: ScalareflectMacroContext)
extends ScalahostSemanticContext[G](scalareflectMacroContext.universe.asInstanceOf[G]) with ScalametaMacroContext {
  def warning(msg: String): Unit = ???
  def error(msg: String): Unit = ???
  def abort(msg: String): Nothing = ???
  def resources: Map[String, Array[Byte]] = ???
}