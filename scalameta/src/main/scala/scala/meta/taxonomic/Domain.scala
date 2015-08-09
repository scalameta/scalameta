package scala.meta
package taxonomic

import scala.{Seq => _}
import scala.collection.immutable.Seq

final case class Domain(modules: Seq[Module])
object Domain {
  def apply(modules: Module*): Domain = Domain(modules: _*)
}
