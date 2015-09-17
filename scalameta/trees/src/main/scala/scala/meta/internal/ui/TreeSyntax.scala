package scala.meta
package internal
package ui

object TreeSyntax {
  def apply[T <: Tree](implicit dialect: Dialect): Syntax[T] = {
    // TODO: Take the dialect into account (#220)
    Syntax { (x: Tree) => s(x.tokens.map(_.show[Syntax]).mkString) }
  }
}