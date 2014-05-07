package scala.reflect
package core

import scala.{Seq => _}
import scala.collection.immutable.Seq

trait HostContext {
  def languageProfile: LanguageProfile

  def root: Pkg.Root
  // NOTE: def stats(scope: Scope): Seq[Tree] is implicit in signatures of Template and Pkg
  def members(scope: Scope): Seq[Member]
  def ctors(scope: Scope): Seq[Ctor]

  def defn(term: Term.Ref): Overload[Member.Term]
  def defn(ref: Type.Ref): Member
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
  def macroApplication: Tree
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def listResources: Seq[String]
  def readResource(url: String): Array[Byte]
}
