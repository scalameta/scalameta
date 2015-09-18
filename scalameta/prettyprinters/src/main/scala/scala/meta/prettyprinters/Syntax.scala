package scala.meta
package prettyprinters

import org.scalameta.show._
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Syntax] for ${T} (if you're prettyprinting a tree, be sure to import a dialect, e.g. scala.meta.dialects.Scala211)")
trait Syntax[T] extends Show[T]
object Syntax {
  def apply[T](f: T => Show.Result): Syntax[T] = new Syntax[T] { def apply(input: T) = f(input) }
}
