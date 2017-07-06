package scala.meta
package internal
package prettyprinters

import scala.meta.tokens.Token
import scala.meta.dialects.Scala211

object TokenToString {
  def apply(token: Token) = {
    val prettyprinter = TokenSyntax[Token](token.dialect)
    prettyprinter(token).toString
  }
}
