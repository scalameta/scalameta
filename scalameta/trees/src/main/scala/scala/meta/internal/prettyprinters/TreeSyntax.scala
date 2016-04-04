package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }

object TreeSyntax {
  def apply[T <: Tree](implicit dialect: Dialect): Syntax[T] = {
    // TODO: Take the dialect into account (#220)
    Syntax { (x: Tree) => s(x.tokens.map(_.show[Syntax]).mkString) }
  }
}