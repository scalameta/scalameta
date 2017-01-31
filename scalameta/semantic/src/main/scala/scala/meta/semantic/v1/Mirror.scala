package scala.meta
package semantic
package v1

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the long-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Mirror {
  def dialect: Dialect
  def sources: Seq[Source]
  def database: Database
  def symbol(ref: Ref): Completed[Symbol]
}
