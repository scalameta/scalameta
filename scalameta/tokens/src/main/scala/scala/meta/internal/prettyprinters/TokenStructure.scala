package scala.meta
package internal
package prettyprinters

import org.scalameta.show.Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.prettyprinters._
import scala.meta.tokens._

object TokenStructure {
  def apply[T <: Token]: Structure[T] = {
    Structure(x => {
      val prefix = (x: Token) match {
        case x: Token.BOF => "BOF"
        case x: Token.EOF => "EOF"
        case x: Token.Dynamic => x.code
        case x: Token.Static => x.name
      }
      s(prefix, " (", x.start.toString, "..", x.end.toString, ")")
    })
  }
}