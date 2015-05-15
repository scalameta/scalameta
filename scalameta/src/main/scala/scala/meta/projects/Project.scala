package scala.meta
package projects

import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Project {
  def sources: Seq[Source]
  def resources: Seq[Resource]
  // TODO: later on, `def dependencies: Seq[Dependency]` etc
}