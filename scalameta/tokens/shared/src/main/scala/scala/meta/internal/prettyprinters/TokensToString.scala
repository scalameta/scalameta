package scala.meta
package internal
package prettyprinters

import scala.meta.inputs.Input
import scala.meta.tokens._

object TokensToString {
  def apply(tokens: Tokens) = {
    val sb = new StringBuilder
    tokens.foreach(t => sb.append(t.text))
    sb.result()
  }

  /**
   * Serialize tokens for quasiquotes, ensuring each token would subsequently be parsed as a single
   * token, so that Origin.ParsedSpliced positions remain valid.
   *
   * This includes converting an Unquote into an Ident (to avoid `\${foo}` getting parsed as
   * multiple tokens); also, what was an escaped `$$` in the original input should be output as a
   * single `$` (but we get it here for free since that `$$` was parsed as `Ident($)`).
   */
  def quasi(tokens: Tokens, input: Input): String = {
    val sb = new java.lang.StringBuilder
    val chars = input.chars
    tokens.foreach {
      case t: Token.Unquote => // output any identifier; let's use original as a human hint
        sb.append('`')
        var idx = t.start
        val maxend = idx + 50
        while (idx < t.end && {
            if (idx > maxend) {
              sb.append("...")
              false
            } else {
              val ch = chars(idx)
              if (Character.isWhitespace(ch)) sb.append(' ') // no newlines
              else if (ch != '`') sb.append(ch) // no backquotes
              true
            }
          }) idx += 1
        sb.append('`')
      case t: Token.Ellipsis => sb.append("/* ").append(chars, t.start, t.len).append(" */")
      case t => sb.append(chars, t.start, t.len)
    }
    sb.toString
  }
}
