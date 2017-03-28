package scala.meta
package internal
package prettyprinters

import scala.meta.tokens._

object TokensToString {
  def apply(tokens: Tokens) = {
    tokens.mkString("")
  }
}
