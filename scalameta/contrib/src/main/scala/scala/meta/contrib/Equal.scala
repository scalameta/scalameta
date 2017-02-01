package scala.meta.contrib

/** Type class used to determine equality.
  *
  * For examples, see [[Structurally]] or [[Syntactically]].
  *
  * Inspired by cats.Eq and scalaz.Equal.
  */
trait Equal[A] {
  def equal(a: A, b: A): Boolean
}
