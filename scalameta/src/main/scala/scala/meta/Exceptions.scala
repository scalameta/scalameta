package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait MetaException extends Exception

@leaf class TokenizeException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with MetaException

@leaf class ParseException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with MetaException

@leaf class SemanticException(message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with MetaException
