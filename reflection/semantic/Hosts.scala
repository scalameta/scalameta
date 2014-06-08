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
  def overrides(member: Member): Seq[Member]
  def overriddenby(member: Member): Seq[Member]

  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type
  def supertypes(tpe: Type): Seq[Type]
  def subclasses(tpe: Type): Seq[Member.Template]
  def dealias(tpe: Type): Type
  def erasure(tpe: Type): Type

  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def resources: Seq[String]
  def resource(url: String): Array[Byte]
}
