package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait ScalahostException extends ScalametaException

@leaf class ConvertException(culprit: Any, message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalahostException {
  override def toString = super.toString
}
