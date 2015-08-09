package scala.meta

import org.scalameta.adt._
import org.scalameta.unreachable
import scala.meta.syntactic._

trait ScalametaException extends Exception

final case class TokenizeException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with ScalametaException
