package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r }
import scala.meta.tokens._

object TokensStructure {
  def apply[T <: Tokens]: Structure[T] = {
    Structure(xs => {
      // val prefix = xs.productPrefix
      val prefix = "Tokens"
      val contents = r(xs.toList.map(_.structure), ", ")
      s(prefix, "(", contents, ")")
    })
  }
}
