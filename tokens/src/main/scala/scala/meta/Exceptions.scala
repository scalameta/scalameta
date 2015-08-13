package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable
import scala.meta.syntactic._

trait ScalametaException extends Exception

@data class DialectException(name: String, message: String)
extends Exception(message) with ScalametaException {
  override def toString = super.toString
}

@data class TokenizeException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with ScalametaException {
  override def toString = super.toString
}
