package scala.meta

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.{Seq => _}
import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.reflect.{ClassTag, classTag}

package object semantic {
  // ===========================
  // PART 1: ATTRIBUTES
  // ===========================

  @root trait Attr
  object Attr {
    // TODO: design the attr hierarchy of semantic facts that can be figured out about trees
    // TODO: examples: a type a tree, a definition/definitions the tree refers to, maybe a desugaring, etc
    // TODO: see https://github.com/JetBrains/intellij-scala/blob/master/src/org/jetbrains/plugins/scala/lang/resolve/ScalaResolveResult.scala#L24
  }

  implicit class SemanticTreeOps(val tree: Tree) extends AnyVal {
    @hosted def attrs: Seq[Attr] = ???
    @hosted def owner: Scope = ???
  }

  sealed trait HasTpe[+T, U]
  object HasTpe {
    implicit object Term extends HasTpe[meta.Term, meta.Type]
    implicit object Method extends HasTpe[meta.Member.Method, meta.Type]
    implicit object Field extends HasTpe[meta.Member.Field, meta.Type]
    implicit object Ctor extends HasTpe[meta.Ctor, meta.Type]
    implicit object TemplateParam extends HasTpe[meta.Template.Param, meta.Type.Arg]
    implicit object Template extends HasTpe[meta.Template, meta.Type]
  }

  implicit class SemanticTypeableOps[T <: Tree, U <: Tree](val tree: T)(implicit ev: HasTpe[T, U]) {
    @hosted def tpe: U = ???
  }

  sealed trait HasDefn[+T, U]
  object HasDefn {
    implicit object Ref extends HasDefn[meta.Ref, meta.Member]
    implicit object TermRef extends HasDefn[meta.Term.Ref, meta.Member.Term]
    implicit object TypeRef extends HasDefn[meta.Type.Ref, meta.Member] // Type.Ref can refer to both types (regular types) and terms (singleton types)
  }

  implicit class SemanticResolvableOps[T <: Tree, U <: Tree](val tree: T)(implicit ev: HasDefn[T, U]) {
    @hosted def defns: Seq[U] = ???
    @hosted def defn: U = ???
  }

  implicit class SemanticCtorRefOps(val tree: Ctor.Ref) extends AnyVal {
    @hosted def ctor: Ctor = ???
  }

  // ===========================
  // PART 2: TYPES
  // ===========================

  implicit class SemanticTypeOps(val tree: Type) extends AnyVal {
    @hosted def <:<(other: Type): Boolean = ???
    @hosted def weak_<:<(other: Type): Boolean = ???
    @hosted def widen: Type = ???
    @hosted def dealias: Type = ???
    @hosted def erasure: Type = ???
    @hosted def companion: Type.Ref = ???
  }

  @hosted def lub(tpes: Seq[Type]): Type = ???
  @hosted def glb(tpes: Seq[Type]): Type = ???

  // ===========================
  // PART 3: MEMBERS
  // ===========================

  implicit class SemanticMemberOps(val tree: Member) extends AnyVal {
    @hosted def ref: Ref = ???
    @hosted def overridden: Seq[Member] = ???
    @hosted def overriding: Seq[Member] = ???
    @hosted def mods: Seq[Mod] = ???
    @hosted def annots: Seq[Ctor.Ref] = ???
    @hosted def isVal: Boolean = ???
    @hosted def isVar: Boolean = ???
    @hosted def isDef: Boolean = ???
    @hosted def isMacro: Boolean = ???
    @hosted def isAbstractType: Boolean = ???
    @hosted def isAliasType: Boolean = ???
    @hosted def isClass: Boolean = ???
    @hosted def isTrait: Boolean = ???
    @hosted def isObject: Boolean = ???
    @hosted def isPkg: Boolean = ???
    @hosted def isPkgObject: Boolean = ???
    @hosted def isPrivate: Boolean = ???
    @hosted def isProtected: Boolean = ???
    @hosted def isPublic: Boolean = ???
    @hosted def accessBoundary: Member = ???
    @hosted def isImplicit: Boolean = ???
    @hosted def isFinal: Boolean = ???
    @hosted def isSealed: Boolean = ???
    @hosted def isOverride: Boolean = ???
    @hosted def isCase: Boolean = ???
    @hosted def isAbstract: Boolean = ???
    @hosted def isCovariant: Boolean = ???
    @hosted def isContravariant: Boolean = ???
    @hosted def isLazy: Boolean = ???
    @hosted def isAbstractOverride: Boolean = ???
    @hosted def isByNameParam: Boolean = ???
    @hosted def isVarargParam: Boolean = ???
    @hosted def isValParam: Boolean = ???
    @hosted def isVarParam: Boolean = ???
  }

  implicit class SemanticTermMemberOps(val tree: Member.Term) extends AnyVal {
    @hosted def ref: Term.Ref = ???
    @hosted def parents: Seq[Member.Term] = ???
    @hosted def children: Seq[Member.Term] = ???
  }

  implicit class SemanticTypeMemberOps(val tree: Member.Type) extends AnyVal {
    @hosted def ref: Type.Ref = ???
    @hosted def parents: Seq[Member.Type] = ???
    @hosted def children: Seq[Member.Type] = ???
    @hosted def tparams: Seq[Type.Param] = ???
  }

  implicit class SemanticTemplateMemberOps(val tree: Member.Template) extends AnyVal {
    @hosted def parents: Seq[Member.Template with Member.Type] = ???
    @hosted def children: Seq[Member.Template] = ???
    @hosted def self: Term.Param = ???
    @hosted def companion: Member.Template = ???
    @hosted def tparams: Seq[Type.Param] = ???
    @hosted def paramss: Seq[Seq[Term.Param]] = ???
    @hosted def template: Template = ???
  }

  implicit class SemanticClassMemberOps(val tree: Member.Class) extends AnyVal {
    @hosted def companion: Member.Object = ???
  }

  implicit class SemanticTraitMemberOps(val tree: Member.Trait) extends AnyVal {
    @hosted def companion: Member.Object = ???
  }

  implicit class SemanticMethodMemberOps(val tree: Member.Method) extends AnyVal {
    @hosted def tparams: Seq[Type.Param] = ???
    @hosted def paramss: Seq[Seq[Term.Param]] = ???
  }

  implicit class SemanticObjectMemberOps(val tree: Member.Object) extends AnyVal {
    @hosted def companion: Member.Template with Member.Type = ???
  }

  implicit class SemanticTemplateParameterOps(val tree: Template.Param) extends AnyVal {
    @hosted def mods: Seq[Mod] = ???
    @hosted def name: Option[meta.Term.Name] = ???
    @hosted def default: Option[meta.Term] = ???
  }

  implicit class SemanticTermParameterOps(val tree: Term.Param) extends AnyVal {
    @hosted def mods: Seq[Mod] = ???
    @hosted def name: Option[meta.Term.Name] = ???
    @hosted def default: Option[meta.Term] = ???
  }

  implicit class SemanticTypeParameterOps(val tree: Type.Param) extends AnyVal {
    @hosted def mods: Seq[Mod] = ???
    @hosted def name: Option[meta.Type.Name] = ???
    @hosted def tparams: Seq[meta.Type.Param] = ???
    @hosted def contextBounds: Seq[meta.Type] = ???
    @hosted def viewBounds: Seq[meta.Type] = ???
    @hosted def lo: meta.Type = ???
    @hosted def hi: meta.Type = ???
  }

  // ===========================
  // PART 4: SCOPES
  // ===========================

  implicit class SemanticScopeOps(val tree: Scope) extends AnyVal {
    @hosted def members: Seq[Member] = ???
    @hosted def members(name: Name): Seq[Member] = ???
    @hosted def packages: Seq[Member.Pkg] = ???
    @hosted def packages(name: Name): Member.Pkg = ???
    @hosted def packages(name: String): Member.Pkg = ???
    @hosted def packages(name: scala.Symbol): Member.Pkg = ???
    @hosted def pkgobject: Member.Object = ???
    @hosted def parents: Seq[Member.Template with Member.Type] = ???
    @hosted def children: Seq[Member.Template] = ???
    @hosted def self: Term.Param = ???
    @hosted def ctor: Ctor = ???
    @hosted def ctors: Seq[Ctor] = ???
    @hosted def classes: Seq[Member.Class] = ???
    @hosted def classes(name: Name): Member.Class = ???
    @hosted def classes(name: String): Member.Class = ???
    @hosted def classes(name: scala.Symbol): Member.Class = ???
    @hosted def traits: Seq[Member.Trait] = ???
    @hosted def traits(name: Name): Member.Trait = ???
    @hosted def traits(name: String): Member.Trait = ???
    @hosted def traits(name: scala.Symbol): Member.Trait = ???
    @hosted def objects: Seq[Member.Object] = ???
    @hosted def objects(name: Name): Member.Object = ???
    @hosted def objects(name: String): Member.Object = ???
    @hosted def objects(name: scala.Symbol): Member.Object = ???
    @hosted def vars: Seq[Term.Name] = ???
    @hosted def vars(name: Name): Term.Name = ???
    @hosted def vars(name: String): Term.Name = ???
    @hosted def vars(name: scala.Symbol): Term.Name = ???
    @hosted def vals: Seq[Term.Name] = ???
    @hosted def vals(name: Name): Term.Name = ???
    @hosted def vals(name: String): Term.Name = ???
    @hosted def vals(name: scala.Symbol): Term.Name = ???
    @hosted def defs: Seq[Member.Method] = ???
    @hosted def defs(name: Name): Member.Method = ???
    @hosted def defs(name: String): Member.Method = ???
    @hosted def defs(name: scala.Symbol): Member.Method = ???
    @hosted def macros: Seq[Member.Method] = ???
    @hosted def macros(name: Name): Member.Method = ???
    @hosted def macros(name: String): Member.Method = ???
    @hosted def macros(name: scala.Symbol): Member.Method = ???
    @hosted def types: Seq[Member.Type] = ???
    @hosted def types(name: Name): Member.Type = ???
    @hosted def types(name: String): Member.Type = ???
    @hosted def types(name: scala.Symbol): Member.Type = ???
    @hosted def params: Seq[Template.Param] = ???
    @hosted def params(name: Name): Template.Param = ???
    @hosted def params(name: String): Template.Param = ???
    @hosted def params(name: scala.Symbol): Template.Param = ???
    @hosted def tparams: Seq[Type.Param] = ???
    @hosted def tparams(name: Name): Type.Param = ???
    @hosted def tparams(name: String): Type.Param = ???
    @hosted def tparams(name: scala.Symbol): Type.Param = ???
  }

  // ===========================
  // PART 5: BINDINGS
  // ===========================

  @root trait Mark
  def mark(): Mark = ???

  implicit class SemanticNameOps(val tree: Name) extends AnyVal {
    @hosted def isBinder: Boolean = ???
    @hosted def isBindee: Boolean = ???
  }
}
