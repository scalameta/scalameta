package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable

trait ScalahostException extends ScalametaException

trait ScalahostError extends ScalametaError

@data class InfrastructureException(message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalahostException {
  override def toString = super.toString
}
