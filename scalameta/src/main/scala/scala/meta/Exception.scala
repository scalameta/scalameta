package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait Exception extends scala.Exception
@leaf class TokenizeException(input: Input, offset: Int, msg: String) extends scala.Exception(msg + " at " + offset) with Exception
@leaf class ParseException(input: Input, token: Token, msg: String) extends scala.Exception(msg + " at " + token) with Exception
@leaf class SemanticException(msg: String) extends scala.Exception(msg) with Exception