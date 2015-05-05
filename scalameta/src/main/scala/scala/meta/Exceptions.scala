package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait MetaException extends Exception
@leaf class TokenizeException(input: Input.Real, offset: Int, message: String) extends Exception(message + " at " + offset) with MetaException
@leaf class ParseException(input: Input, token: Token, message: String) extends Exception(message + " at " + token) with MetaException
@leaf class SemanticException(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull) with MetaException