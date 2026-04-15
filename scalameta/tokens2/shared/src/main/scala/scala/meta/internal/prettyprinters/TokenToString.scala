package scala.meta
package internal
package prettyprinters

import scala.meta.tokens.Token

object TokenToString {
  def apply(token: Token): String = TokenSyntax.show(token).toString
}
