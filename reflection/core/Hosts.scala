package scala.reflect
package core

trait HostContext {
  def languageProfile: LanguageProfile

  def root: Pkg.Root
  // NOTE: def stats(scope: Scope): Seq[Tree] is implicit in signatures of Template and Pkg
  def members(scope: Scope): Seq[Member]
  def ctors(scope: Scope): Seq[Ctor]

  def defn(term: Term.Ref): Member.Overloaded[Member.Term]
  def defn(ref: Type.Ref): Member
  def overrides(member: Member.Term): List[Member.Term]
  def overrides(member: Member.Type): List[Member.Type]

  def <:<(tpe1: Type, tpe2: Type): Boolean
  def weak_<:<(tpe1: Type, tpe2: Type): Boolean
  def supertypes(tpe: Type): List[Type]
  def linearization(tpes: List[Type]): List[Type]
  def subclasses(tpe: Type): List[Member.Template]
  def self(tpe: Type): Aux.Self
  def lub(tpes: Type*): Type
  def glb(tpes: Type*): Type
  def widen(tpe: Type): Type
  def dealias(tpe: Type): Type
  def erasure(tpe: Type): Type

  // TODO: if we keep typecheck, then we don't really need all those `tpe` methods
  def resolve[T <: Member](overloaded: Member.Overloaded[T], tpes: Type*): T
  def typecheck(tree: Tree): List[Attribute]
}

trait MacroContext extends HostContext {
  def macroApplication: Tree
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def listResources: List[String]
  def readResource(url: String, codec: scala.io.Codec): String
}
