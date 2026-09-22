package scala.meta
package trees

/**
 * Builds one tree, once.
 *
 * A companion's `newBuilder` takes the fields without a default and the implicit dialect, and
 * starts from a fresh tree whose origin is that dialect. A tree's `toBuilder` starts from a copy of
 * that tree: an untouched field is loaded from the original on first use, the copy has no parent,
 * and its origin keeps the dialect and the input of the original and drops the position. A setter
 * checks its value and returns the builder, and `origin` replaces the origin. `result()` returns
 * the tree, with its children reparented.
 *
 * A field that a later version replaced keeps a setter under its old name, deprecated: it computes
 * the new field from the value given and the tree's other old fields.
 *
 * A builder is single-use. `result()` returns the instance it built, so a setter called after it
 * writes into that tree, and a second `result()` returns the same tree again.
 */
trait TreeBuilder[T <: Tree] extends Any {
  def origin(value: Origin): TreeBuilder[T]
  def begComment(value: Option[Tree.Comments]): TreeBuilder[T]
  def endComment(value: Option[Tree.Comments]): TreeBuilder[T]
  def result(): T
}
