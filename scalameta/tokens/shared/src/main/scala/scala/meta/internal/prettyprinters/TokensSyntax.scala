package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{repeat => r }
import scala.meta.tokens._

object TokensSyntax {
  def apply[T <: Tokens](dialect: Dialect): Syntax[T] = {
    Syntax { xs => r(xs.toList.map(x => TokenSyntax.apply[Token](dialect).apply(x)), "") }
  }
}
