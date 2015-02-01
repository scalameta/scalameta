package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.hygiene.Symbol

trait Context {
  def dialect: Dialect

  def attrs(tree: Tree): Seq[Attr]

  def root: Scope
  def owner(tree: Tree): Scope
  def members(scope: Scope): Seq[Tree]
  def symbol(tree: Tree): Symbol
  def tree(symbol: Symbol): Tree

  def isSubType(tpe1: Type, tpe2: Type): Boolean
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type

  def parents(member: Member): Seq[Member]
  def children(member: Member): Seq[Member]
}