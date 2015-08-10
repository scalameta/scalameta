package scala.meta
package taxonomic

import org.scalameta.annotations._
import scala.annotation._
import scala.{Seq => _}
import scala.collection.immutable.Seq

@opaque
@implicitNotFound("this method requires an implicit scala.meta.taxonomic.Context")
trait Context {
  def sources(module: Module): Seq[Source]
  def resources(module: Module): Seq[Resource]
  def deps(module: Module): Seq[Module]
}
