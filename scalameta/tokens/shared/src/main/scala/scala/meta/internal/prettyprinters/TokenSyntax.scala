package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{sequence => s}
import scala.meta.tokens._

object TokenSyntax {
  def apply[T <: Token](dialect: Dialect): Syntax[T] = Syntax(x => s(x.text))
}
