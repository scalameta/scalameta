package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait MetaException extends Exception

@leaf class TokenizeException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with MetaException

@leaf class ParseException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with MetaException

@leaf class SemanticException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with MetaException {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
}

@leaf class AbortException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with MetaException {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
}
