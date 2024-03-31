package scala.meta
package tokenizers

import scala.meta.common._
import scala.meta.inputs._

private[meta] trait Api {
  implicit class XtensionTokenizeInputLike[T](inputLike: T) {
    def tokenize(implicit
        convert: Convert[T, Input],
        tokenize: Tokenize,
        dialect: Dialect
    ): Tokenized = (dialect, convert(inputLike)).tokenize
  }
  implicit class XtensionTokenizersDialectApply(dialect: Dialect) {
    def apply[T](inputLike: T)(implicit convert: Convert[T, Input]): (Dialect, Input) =
      (dialect, convert(inputLike))
  }
  implicit class XtensionTokenizeDialectInput(dialectInput: (Dialect, Input)) {
    def tokenize(implicit tokenize: Tokenize): Tokenized = {
      val (dialect, input) = dialectInput
      tokenize.apply(input, dialect)
    }
  }
  implicit class XtensionTokenizeInputDialect(inputDialect: (Input, Dialect)) {
    def tokenize(implicit tokenize: Tokenize): Tokenized = {
      val (input, dialect) = inputDialect
      (dialect, input).tokenize
    }
  }
}

private[meta] trait Aliases {
  type Tokenized = scala.meta.tokenizers.Tokenized
  object Tokenized {
    type Success = scala.meta.tokenizers.Tokenized.Success
    val Success = scala.meta.tokenizers.Tokenized.Success

    type Error = scala.meta.tokenizers.Tokenized.Error
    val Error = scala.meta.tokenizers.Tokenized.Error
  }

  type TokenizeException = scala.meta.tokenizers.TokenizeException
  lazy val TokenizeException = scala.meta.tokenizers.TokenizeException
}
