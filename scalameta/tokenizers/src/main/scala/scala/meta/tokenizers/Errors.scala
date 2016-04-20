package scala.meta
package tokenizers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.tokens._
import scala.meta.inputs._
import scala.meta.internal.inputs._
import scala.compat.Platform.EOL

@root trait Tokenized {
  def get: Tokens = this match {
    case Tokenized.Success(tokens) => tokens
    case Tokenized.Error(_, _, details) => throw details
  }
  def orElse(alt: => Tokenized): Tokenized = this match {
    case Tokenized.Success(_) => this
    case _ => alt
  }
  def getOrElse(alt: => Tokens): Tokens = this match {
    case Tokenized.Success(tokens) => tokens
    case _ => alt
  }
}

object Tokenized {
  @leaf class Success(tokens: Tokens) extends Tokenized
  @leaf class Error(pos: Position, message: String, details: TokenizeException) extends Tokenized {
    override def toString = details.fullMessage
  }
}

@data class TokenizeException(pos: Position, shortMessage: String)
extends Exception(pos.formatMessage("error", shortMessage)) {
  def fullMessage = getMessage
  override def toString = super.toString
}
