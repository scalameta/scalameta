package scala.meta
package internal
package prettyprinters

import scala.meta.tokens.Token

private[meta] object TokenToString {
  def apply(token: Token) = {
    val prettyprinter = TokenSyntax[Token]
    prettyprinter(token).toString
  }
}
