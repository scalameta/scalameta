package scala.meta
package internal
package prettyprinters

import org.scalameta.show.Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokenSyntax {
  def apply[T <: Token](implicit dialect: Dialect): Syntax[T] = {
    // TODO: Take the dialect into account (#220)
    Syntax { x => s(x.code) }
  }
}