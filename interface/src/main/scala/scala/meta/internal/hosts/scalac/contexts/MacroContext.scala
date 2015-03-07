package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostSemanticContext}
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.macros.{Context => ScalametaMacroContext}
import scala.tools.nsc.{Global => ScalaGlobal}

@context(translateExceptions = false)
class MacroContext[G <: ScalaGlobal](val scalareflectMacroContext: ScalareflectMacroContext)
extends ScalahostSemanticContext[G](scalareflectMacroContext.universe.asInstanceOf[G]) with ScalametaMacroContext {
  private[meta] def warning(msg: String): Unit = ???
  private[meta] def error(msg: String): Unit = ???
  private[meta] def abort(msg: String): Nothing = ???
  private[meta] def resources: Map[String, Array[Byte]] = ???
}
