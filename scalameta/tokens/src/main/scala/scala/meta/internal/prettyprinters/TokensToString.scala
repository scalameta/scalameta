package scala.meta
package internal
package prettyprinters

import scala.meta.tokens.Tokens

private[meta] object TokensToString {
  def apply(tokens: Tokens) = {
    val prettyprinter = TokensStructure[Tokens]
    prettyprinter(tokens).toString
  }
}
