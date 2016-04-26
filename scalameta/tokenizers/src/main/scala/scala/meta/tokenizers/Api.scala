package scala.meta
package tokenizers

import org.scalameta.unreachable
import scala.meta.convert._
import scala.meta.tokens._
import scala.meta.inputs._

class InputWithDialect(input: Input, dialect: Dialect) {
  def tokenize(implicit tokenize: Tokenize): Tokenized = {
    input match {
      case content: Content => tokenize.apply(content)(dialect)
      case tokens: Tokens => Tokenized.Success(tokens)
      case _ => unreachable
    }
  }
}

private[meta] trait Api {
  implicit class XtensionTokenizeInputLike[T](inputLike: T) {
    def tokenize(implicit convert: Convert[T, Input], tokenize: Tokenize, dialect: Dialect): Tokenized = {
      val input = convert(inputLike)
      new InputWithDialect(input, dialect).tokenize
    }
  }

  implicit class XtensionDialectTokenizeInputLike(dialect: Dialect) {
    def apply[T](inputLike: T)(implicit convert: Convert[T, Input]): InputWithDialect = {
      val input = convert(inputLike)
      new InputWithDialect(input, dialect)
    }
  }
}

private[meta] trait Aliases {
  type Tokenized = scala.meta.tokenizers.Tokenized
  val Tokenized = scala.meta.tokenizers.Tokenized

  type TokenizeException = scala.meta.tokenizers.TokenizeException
  val TokenizeException = scala.meta.tokenizers.TokenizeException
}
