package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokenSyntax {
  private[meta] def show[T <: Token](token: T): Show.Result = Show.asis(token.text)
  def apply[T <: Token]: Syntax[T] = Syntax(show)
}
