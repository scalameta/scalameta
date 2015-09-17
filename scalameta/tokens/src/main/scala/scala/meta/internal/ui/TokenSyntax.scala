package scala.meta
package internal
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.ui.Syntax
import scala.meta.syntactic.Token

object TokenSyntax {
  def apply[T <: Token](implicit dialect: Dialect): Syntax[T] = {
    // TODO: Take the dialect into account (#220)
    Syntax { x => s(x.code) }
  }
}