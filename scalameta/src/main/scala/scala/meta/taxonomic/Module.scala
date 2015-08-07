package scala.meta
package taxonomic

import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Module {
  def uri: URI
  def sources: Seq[Source]
  def resources: Seq[Resource]
  def dependencies: Seq[Module]
}
