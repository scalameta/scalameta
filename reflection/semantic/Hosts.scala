package scala.reflect
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

trait Host {
  def defns(ref: Ref): Seq[Tree]
  def attrs(tree: Tree): Seq[Attr]

  def owner(tree: Tree): Scope
  def members(scope: Scope): Seq[Tree]
  def members(scope: Scope, name: Name): Seq[Tree]

  def <:<(tpe1: Type, tpe2: Type): Boolean
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type
  def superclasses(member: Member.Template): Seq[Member.Template]
  def subclasses(member: Member.Template): Seq[Member.Template]
  def overridden(member: Member): Seq[Member]
  def overriding(member: Member): Seq[Member]

  def erasure(tpe: Type): Type
}

trait MacroHost extends Host {
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def resources: Seq[String]
  def resource(url: String): Array[Byte]
}
