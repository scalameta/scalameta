package scala.meta
package tokenizers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.tokens._

@root
trait Tokenized {

  def fold[A](fe: Tokenized.Error => A, ft: Tokens => A): A = this match {
    case x: Tokenized.Success => ft(x.tokens)
    case e: Tokenized.Error => fe(e)
  }

  def get: Tokens = fold(e => throw e.details, identity)
  def orElse(alt: => Tokenized): Tokenized = fold(_ => alt, _ => this)
  def getOrElse(alt: => Tokens): Tokens = fold(_ => alt, identity)

  def toOption: Option[Tokens] = fold(_ => None, t => Some(t))
  def toEither: Either[Tokenized.Error, Tokens] = fold(e => Left(e), t => Right(t))

}

object Tokenized {
  @leaf
  class Success(tokens: Tokens) extends Tokenized {
    override def toString = tokens.toString
  }
  @leaf
  class Error(pos: Position, message: String, details: Exception) extends Tokenized {
    override def toString = details.toString
  }
}

@data
class TokenizeException(pos: Position, shortMessage: String)
    extends InputException(pos, shortMessage) {
  override def toString = getMessage
}
