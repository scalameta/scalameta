package scala.meta
package internal
package prettyprinters

import scala.meta.tokens.Token

object TokenToString {
  def apply(token: Token) = {
    val prettyprinter = TokenSyntax[Token](token.dialect)
    prettyprinter(token).toString
  }
}
