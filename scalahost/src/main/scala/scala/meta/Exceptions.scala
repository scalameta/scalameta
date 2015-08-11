package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable

trait ScalahostException extends ScalametaException

@data class InfrastructureException(message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalahostException {
  override def toString = super.toString
}

@data class ConvertException(culprit: Any, message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalahostException {
  override def toString = super.toString
}
