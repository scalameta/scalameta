package scala.meta
package tokenizers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.tokens._
import scala.meta.inputs._
import scala.meta.internal.inputs._

@root trait Tokenized {

  def fold[A](fe: Tokenized.Error => A, ft: Tokens => A): A = this match {
    case Tokenized.Success(t) => ft(t)
    case e: Tokenized.Error => fe(e)
  }

  def get: Tokens = fold(e => throw e.details, identity)
  def orElse(alt: => Tokenized): Tokenized = fold(_ => alt, _ => this)
  def getOrElse(alt: => Tokens): Tokens = fold(_ => alt, identity)

  def toOption: Option[Tokens] = fold(_ => None, t => Some(t))
  def toEither: Either[Tokenized.Error, Tokens] = fold(e => Left(e), t => Right(t))

}

object Tokenized {
  @leaf class Success(tokens: Tokens) extends Tokenized {
    override def toString = tokens.toString
  }
  @leaf class Error(pos: Position, message: String, details: Exception) extends Tokenized {
    override def toString = details.toString
  }
}

@data class TokenizeException(pos: Position, shortMessage: String)
extends Exception(pos.formatMessage("error", shortMessage)) {
  def fullMessage = getMessage
  override def toString = fullMessage
}
