package scala.meta
package internal
package ui

object TokenSyntax {
  def apply[T <: Tree](implicit dialect: Dialect): Syntax[T] = {
    // TODO: Take the dialect into account (#220)
    Syntax { x => s(x.code) }
  }
}