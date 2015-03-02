package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait MetaException extends Exception
@leaf class TokenizeException(input: Input, offset: Int, msg: String) extends Exception(msg + " at " + offset) with MetaException
@leaf class ParseException(input: Input, token: Token, msg: String) extends Exception(msg + " at " + token) with MetaException
@leaf class SemanticException(msg: String) extends Exception(msg) with MetaException