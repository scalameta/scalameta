package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokenSyntax {
  def apply[T <: Token](dialect: Dialect): Syntax[T] = Syntax(x => Show.asis(x.text))
}
