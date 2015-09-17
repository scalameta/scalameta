package scala.meta
package internal
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }

object TokenStructure {
  def apply[T <: Tree]: Structure[T] = {
    Structure(x => {
      val prefix = (x: Token) match {
        case x: BOF => "BOF"
        case x: EOF => "EOF"
        case x: Dynamic => x.code
        case x: Static => x.name
      }
      s(prefix, " (", x.start.toString, "..", x.end.toString, ")")
    })
  }
}