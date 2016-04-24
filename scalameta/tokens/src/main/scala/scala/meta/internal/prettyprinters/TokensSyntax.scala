package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens._

object TokensSyntax {
  def apply[T <: Tokens]: Syntax[T] = {
    Syntax { xs => r(xs.toList.map(TokenSyntax[Token].apply), "") }
  }
}