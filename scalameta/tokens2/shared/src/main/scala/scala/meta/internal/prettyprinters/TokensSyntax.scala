package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokensSyntax {
  def apply[T <: Tokens]: Syntax[T] = Syntax(xs => Show.repeat(xs.map(TokenSyntax.show), ""))
}
