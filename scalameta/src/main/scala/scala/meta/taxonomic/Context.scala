package scala.meta
package taxonomic

import org.scalameta.annotations._
import scala.annotation._
import scala.{Seq => _}
import scala.collection.immutable.Seq

@opaque
@implicitNotFound("this method requires an implicit scala.meta.taxonomic.Context")
trait Context {
  def domain: Domain

  def sources(module: Module): Seq[Source]
  def resources(module: Module): Seq[Resource]
  def dependencies(module: Module): Seq[Module]

  def append(worksheet: Worksheet, source: Source): Unit
}
