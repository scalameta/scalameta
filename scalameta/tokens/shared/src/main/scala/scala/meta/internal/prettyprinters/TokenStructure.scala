package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{sequence => s, wrap => w}
import scala.meta.tokens._

object TokenStructure {
  def apply[T <: Token]: Structure[T] = {
    Structure[Token] { x =>
      implicit val dialect = x.dialect
      val syntax = x match {
        case _: Token.Tab => "\\t"
        case _: Token.CR => "\\r"
        case _: Token.LF => "\\n"
        case _: Token.FF => "\\f"
        case _: Token.LFLF => "\\n\\n"
        case x => x.text
      }
      val label = {
        val name = x.getClass.getName
        val idx = name.lastIndexOf(".Token$")
        if (idx < 0) x.name else name.substring(idx + 7).replace("$", ".")
      }
      s(label, w("(", syntax, ")"), " [", x.start.toString, "..", x.end.toString, ")")
    }
  }
}
