package scala.meta
package tokenizers

import org.scalameta.unreachable
import scala.meta.convert._
import scala.meta.tokens._
import scala.meta.inputs._

private[meta] trait Api {
  implicit class XtensionTokens(tokens: Seq[Token]) {
    def toTokens: Tokens = Tokens(tokens: _*)
  }

  implicit class XtensionTokenizeInputLike[T](inputLike: T) {
    def tokenize(implicit convert: Convert[T, Input], tokenize: Tokenize, dialect: Dialect): Tokenized = {
      val input = convert(inputLike)
      input match {
        case content: Content => tokenize(content)(dialect)
        case tokens: Tokens => Tokenized.Success(tokens)
        case _ => unreachable
      }
    }
  }
}

private[meta] trait Aliases {
  type Tokenized = scala.meta.tokenizers.Tokenized
  val Tokenized = scala.meta.tokenizers.Tokenized

  type TokenizeException = scala.meta.tokenizers.TokenizeException
  val TokenizeException = scala.meta.tokenizers.TokenizeException
}
