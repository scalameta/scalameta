package scala.meta
package internal
package prettyprinters

import scala.meta.tokens._

object TokensToString {
  def apply(tokens: Tokens) = {
    val sb = new StringBuilder
    tokens.foreach(t => sb.append(t.text))
    sb.result()
  }
}
