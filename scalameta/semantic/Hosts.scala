package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Host {
  def attrs(tree: Tree): Seq[Attr]

  def owner(tree: Tree): Scope
  def members(scope: Scope): Seq[Tree]

  def <:<(tpe1: Type, tpe2: Type): Boolean
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type
  def erasure(tpe: Type): Type

  def parents(member: Member): Seq[Member]
  def children(member: Member): Seq[Member]
}

trait MacroHost extends Host {
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def resources: Seq[String]
  def resource(url: String): Array[Byte]
}
