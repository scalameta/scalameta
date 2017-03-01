package scala.meta.contrib.equality

/** Type class used to determine equality.
  *
  * For examples, see [[Structurally]] or [[Syntactically]].
  *
  * Inspired by cats.Eq and scalaz.Equal.
  */
trait Equal[A] {
  @deprecated("Use isEqual instead", "1.7.0")
  def equal(a: A, b: A): Boolean = isEqual(a, b)
  def isEqual(a: A, b: A): Boolean
}
