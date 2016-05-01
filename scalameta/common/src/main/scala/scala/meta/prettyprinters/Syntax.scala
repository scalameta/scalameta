package scala.meta
package prettyprinters

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Syntax] for ${T}")
trait Syntax[T] extends Show[T]
object Syntax {
  def apply[T](f: T => Show.Result): Syntax[T] = new Syntax[T] { def apply(input: T) = f(input) }

  sealed trait Options { def isLazy: Boolean }
  private[meta] trait LowPriorityOptions {
    case object Lazy extends Options { def isLazy = true }
  }
  object Options extends LowPriorityOptions {
    implicit case object Eager extends Options { def isLazy = false }
  }
}
