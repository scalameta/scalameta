package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokenSyntax {
  import Show.{sequence => s}

  def apply[T <: Token](dialect: Dialect): Syntax[T] = Syntax(x => s(x.text))
}
