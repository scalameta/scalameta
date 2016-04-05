package scala.meta
package tokenizers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.tokens._
import scala.meta.inputs._

@root trait Tokenized {
  def get: Tokens = this match {
    case Tokenized.Success(tokens) => tokens
    case Tokenized.Error(_, _, details) => throw details
  }
}

object Tokenized {
  @leaf class Success(tokens: Tokens) extends Tokenized
  @leaf class Error(pos: Position, message: String, details: TokenizeException) extends Tokenized
}

@data class TokenizeException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") {
  override def toString = super.toString
}
