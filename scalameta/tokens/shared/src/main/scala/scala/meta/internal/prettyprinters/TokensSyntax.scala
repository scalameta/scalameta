package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokensSyntax {
  import Show.{repeat => r}

  def apply[T <: Tokens](dialect: Dialect): Syntax[T] =
    Syntax(xs => r(xs.toList.map(x => TokenSyntax.apply[Token](dialect).apply(x)), ""))
}
