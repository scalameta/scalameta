package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable

@data class ParseException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with ScalametaException {
  override def toString = super.toString
}

@data class MergeException(culprits: Seq[Tree], message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalametaException {
  override def toString = super.toString
}

@data class SemanticException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with ScalametaException {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
  override def toString = super.toString
}

@data class ModuleException(module: Module, message: String, cause: Option[Throwable] = None)
extends Exception(s"failed to load $module because $message", cause.orNull) with ScalametaException {
  override def toString = super.toString
}

@data class AbortException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with ScalametaException {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
  override def toString = super.toString
}
