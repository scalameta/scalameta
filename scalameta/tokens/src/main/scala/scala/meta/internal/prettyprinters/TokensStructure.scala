package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens._

object TokensStructure {
  def apply[T <: Tokens]: Structure[T] = {
    Structure(xs => {
      // val prefix = xs.productPrefix
      val prefix = "Tokens"
      s(prefix, "(", r(xs.toList.map(TokenStructure[Token].apply), ", "), ")")
    })
  }
}