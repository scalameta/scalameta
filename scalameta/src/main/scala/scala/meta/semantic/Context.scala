package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.hygiene.Symbol
import org.scalameta.annotations._
import scala.annotation._

@opaque(exclude = "dialect")
@implicitNotFound("this method requires an implicit scala.meta.semantic.Context")
trait Context {
  def dialect: Dialect

  def tpe(term: Term): Type
  def tpe(member: Member): Type
  def defns(ref: Ref): Seq[Member]
  def members(tpe: Type): Seq[Member]

  def isSubType(tpe1: Type, tpe2: Type): Boolean
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type

  def parents(member: Member): Seq[Member]
  def children(member: Member): Seq[Member]
}