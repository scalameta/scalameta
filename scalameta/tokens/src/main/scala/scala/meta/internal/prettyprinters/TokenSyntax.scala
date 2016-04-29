package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens._

object TokenSyntax {
  def apply[T <: Token]: Syntax[T] = {
    Syntax { x => s(new String(x.input.chars, x.start, x.end - x.start)) }
  }
}