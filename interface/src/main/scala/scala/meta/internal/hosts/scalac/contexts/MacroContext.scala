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
class MacroContext[G <: ScalaGlobal](val rc: ScalareflectMacroContext)
extends ScalahostSemanticContext[G](rc.universe.asInstanceOf[G]) with ScalametaMacroContext {
  private[meta] def warning(msg: String): Unit = rc.warning(rc.enclosingPosition, msg)
  private[meta] def error(msg: String): Unit = rc.error(rc.enclosingPosition, msg)
  private[meta] def abort(msg: String): Nothing = rc.abort(rc.enclosingPosition, msg)
  private[meta] def resources: Map[String, Array[Byte]] = ???
}
