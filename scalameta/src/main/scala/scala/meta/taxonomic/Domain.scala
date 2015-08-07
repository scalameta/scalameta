package scala.meta
package taxonomic

import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Domain extends Module {
  def modules: Seq[Module]
}