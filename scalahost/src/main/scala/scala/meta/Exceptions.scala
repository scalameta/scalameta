package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable

trait ScalahostException extends ScalametaException

trait ScalahostError extends ScalametaError

@data class InfrastructureException(message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with ScalahostException {
  def this(message: String) = this(message, None)
  def this(message: String, cause: Throwable) = this(message, Some(cause))
  override def toString = super.toString
}
