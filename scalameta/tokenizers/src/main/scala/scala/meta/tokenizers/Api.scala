package scala.meta
package tokenizers

import org.scalameta.unreachable
import org.scalameta.convert._
import scala.meta.tokens._
import scala.meta.inputs._

private[meta] trait Api {
  implicit class XtensionTokens(tokens: Seq[Token]) {
    def toTokens: Tokens = Tokens(tokens: _*)
  }

  implicit class XtensionTokenizeInputLike[T](inputLike: T) {
    def tokens(implicit convert: Convert[T, Input], tokenize: Tokenize, dialect: Dialect): Tokens = {
      val input = convert(inputLike)
      input match {
        case content: Content => tokenize(content)(dialect)
        case tokens: Tokens => tokens
        case _ => unreachable
      }
    }
  }
}

private[meta] trait Aliases {
  type TokenizeException = scala.meta.tokenizers.TokenizeException
  val TokenizeException = scala.meta.tokenizers.TokenizeException
}
