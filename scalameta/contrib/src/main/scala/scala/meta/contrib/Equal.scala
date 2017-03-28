package scala.meta.contrib

import scala.meta.prettyprinters.Show
import scala.language.higherKinds

/** Type class used to determine equality.
  *
  * Inspired by cats.Eq and scalaz.Equal.
  */
trait Equal[F[x] <: Show[x]] {
  def isEqual[A](a: A, b: A): Boolean
}
