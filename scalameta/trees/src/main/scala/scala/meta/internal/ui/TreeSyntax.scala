package scala.meta
package internal
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.ui.Syntax

object TreeSyntax {
  def apply[T <: Tree](implicit dialect: Dialect): Syntax[T] = {
    // TODO: Take the dialect into account (#220)
    Syntax { (x: Tree) => s(x.tokens.map(_.show[Syntax]).mkString) }
  }
}