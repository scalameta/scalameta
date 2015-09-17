package scala.meta
package internal
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.ui.Structure
import scala.meta.syntactic.Token
import scala.meta.syntactic.Token._

object TokenStructure {
  def apply[T <: Token]: Structure[T] = {
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