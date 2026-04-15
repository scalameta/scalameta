package scala.meta.prettyprinters

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Reprint] for ${T}")
trait Reprint[-T] extends Show[T]
object Reprint {
  def apply[T](f: T => Show.Result): Reprint[T] = new Reprint[T] {
    def apply(input: T) = f(input)
  }
}
