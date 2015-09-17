package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.annotations._
import scala.annotation._

@opaque(exclude = "dialect|domain")
@implicitNotFound("this method requires an implicit scala.meta.semantic.Context")
trait Context {
  def dialect: Dialect
  def domain: Domain

  def typecheck(tree: Tree): Tree

  def defns(ref: Ref): Seq[Member]
  def members(tpe: Type): Seq[Member]
  def supermembers(member: Member): Seq[Member]
  def submembers(member: Member): Seq[Member]

  def isSubtype(tpe1: Type, tpe2: Type): Boolean
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type
  def supertypes(tpe: Type): Seq[Type]
  def widen(tpe: Type): Type
  def dealias(tpe: Type): Type
}