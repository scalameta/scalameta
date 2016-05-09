package scala.meta
package internal
package prettyprinters

import org.scalameta.collections._
import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens._

object TokensStructure {
  def apply[T <: Tokens](implicit options: Options): Structure[T] = {
    Structure(xs => {
      // val prefix = xs.productPrefix
      val prefix = "Tokens"
      val contents = if (options.isLazy && xs.isLazy) s("...") else r(xs.toList.map(_.structure), ", ")
      s(prefix, "(", contents, ")")
    })
  }
}