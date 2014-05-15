package scala.reflect
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

trait HostContext {
  def syntaxProfile: SyntaxProfile
  def semanticProfile: SemanticProfile

  def root: Scope.TopLevel
  // NOTE: def stats(scope: Scope): Seq[Tree] is implicit in signatures of Template and Pkg
  def members(scope: Scope): Seq[Member]
  def members(scope: Scope, name: Name): Seq[Member]
  def ctors(scope: Scope): Seq[Ctor]

  def defn(term: Term.Ref): Seq[Member.Term]
  def defn(tpe: Type.Ref): Member
  def overrides(member: Member.Term): Seq[Member.Term]
  def overrides(member: Member.Type): Seq[Member.Type]

  def <:<(tpe1: Type, tpe2: Type): Boolean
  def weak_<:<(tpe1: Type, tpe2: Type): Boolean
  def supertypes(tpe: Type): Seq[Type]
  def linearization(tpes: Seq[Type]): Seq[Type]
  def subclasses(tpe: Type): Seq[Member.Template]
  def self(tpe: Type): Aux.Self
  def lub(tpes: Seq[Type]): Type
  def glb(tpes: Seq[Type]): Type
  def widen(tpe: Type): Type
  def dealias(tpe: Type): Type
  def erasure(tpe: Type): Type

  def attrs(tree: Tree): Seq[Attribute]
}

trait MacroContext extends HostContext {
  def application: Tree
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def resources: Seq[String]
  def resourceAsBytes(url: String): Array[Byte]
}
