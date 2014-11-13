package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Host {
  def attrs(tree: Tree): Seq[Attr]

  def owner(tree: Tree): Scope
  def members(scope: Scope): Seq[Tree]

  def isSubType(tpe1: Type, tpe2: Type): Boolean
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type

  def parents(member: Member): Seq[Member]
  def children(member: Member): Seq[Member]
}
