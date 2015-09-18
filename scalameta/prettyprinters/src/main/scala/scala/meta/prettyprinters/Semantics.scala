package scala.meta
package prettyprinters

import org.scalameta.show._
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Semantics] for ${T} (be sure to have an implicit scala.meta.semantic.Context in scope)")
trait Semantics[T] extends Show[T]
object Semantics {
  def apply[T](f: T => Show.Result): Semantics[T] = new Semantics[T] { def apply(input: T) = f(input) }
}
