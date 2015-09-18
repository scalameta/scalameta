package scala.meta
package tokenizers
package common

import org.scalameta.convert._
import scala.meta.inputs._
import scala.meta.tokens._

private[meta] trait Api {
  implicit class XtensionTokens(tokens: Seq[Token]) {
    def toTokens: Tokens = Tokens(tokens: _*)
  }

  implicit class XtensionTokenizeContentLike[T](contentLike: T) {
    def tokens(implicit convert: Convert[T, Content], dialect: Dialect, tokenize: Tokenize): Tokens = {
      tokenize(convert(contentLike))
    }
  }
}

private[meta] trait Aliases {
  type TokenizeException = scala.meta.tokenizers.common.TokenizeException
  val TokenizeException = scala.meta.tokenizers.common.TokenizeException
}
