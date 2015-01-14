package scala.meta

import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.reflect.{ClassTag, classTag}
import scala.meta.ui.{Exception => SemanticException}

package object semantic {
  // ===========================
  // PART 1: ATTRIBUTES
  // ===========================

  @root trait Attr
  object Attr {
    // TODO: design the attr hierarchy of semantic facts that can be figured out about trees
    // TODO: examples: a type of a tree, a definition/definitions the tree refers to, maybe a desugaring, etc
    // TODO: see https://github.com/JetBrains/intellij-scala/blob/master/src/org/jetbrains/plugins/scala/lang/resolve/ScalaResolveResult.scala#L24
    @leaf class Type(tpe: scala.meta.Type) extends Attr
  }

  implicit class SemanticTreeOps(val tree: Tree) extends AnyVal {
    @hosted private[meta] def attr[T: ClassTag]: T = {
      val relevant = tree.attrs.filter(_ match { case _: T => true; case _ => false })
      require(relevant.length < 2)
      relevant match {
        case Seq(tpe) => tpe.asInstanceOf[T]
        case Seq() => throw new SemanticException(s"failed to figure out ${classTag[T].runtimeClass.getName.toLowerCase} of ${tree.summary}")
        case _ => unreachable
      }
    }
    @hosted def attrs: Seq[Attr] = askHost
    @hosted def owner: Scope = ???
  }

  sealed trait HasTpe[+T, U]
  object HasTpe {
    implicit object Term extends HasTpe[meta.Term, meta.Type]
    implicit object Member extends HasTpe[meta.Member, meta.Type]
    implicit object Ctor extends HasTpe[meta.Ctor, meta.Type]
    implicit object TemplateParam extends HasTpe[meta.Templ.Param, meta.Type.Arg]
    implicit object Templ extends HasTpe[meta.Templ, meta.Type]
  }

  implicit class SemanticTypeableOps[T <: Tree, U <: Tree : ClassTag](val tree: T)(implicit ev: HasTpe[T, U]) {
    @hosted def tpe: U = {
      val tpe = tree.attr[Attr.Type].tpe
      require(tpe != null && classTag[U].runtimeClass.isAssignableFrom(tpe.getClass))
      tpe.asInstanceOf[U]
    }
  }

  sealed trait HasDefn[+T, U]
  object HasDefn {
    implicit object Ref extends HasDefn[meta.Ref, meta.Member]
    implicit object TermRef extends HasDefn[meta.Term.Ref, meta.Member.Term]
    implicit object TypeRef extends HasDefn[meta.Type.Ref, meta.Member] // Type.Ref can refer to both types (regular types) and terms (singleton types)
    implicit object Selector extends HasDefn[meta.Selector, meta.Member]
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
    @hosted def companion: Type.Ref = ???
    @hosted def parents: Seq[Type] = ???
  }

  @hosted def lub(tpes: Seq[Type]): Type = ???
  @hosted def glb(tpes: Seq[Type]): Type = ???

  // ===========================
  // PART 3: MEMBERS
  // ===========================

  implicit class SemanticMemberOps(val tree: Member) extends AnyVal {
    @hosted def ref: Ref = ???
    @hosted def parents: Seq[Member] = ???
    @hosted def children: Seq[Member] = ???
    @hosted def companion: Member = ???
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
    @hosted def companion: Member.Type = ???
  }

  implicit class SemanticTypeMemberOps(val tree: Member.Type) extends AnyVal {
    @hosted def ref: Type.Ref = ???
    @hosted def parents: Seq[Member.Type] = ???
    @hosted def children: Seq[Member.Type] = ???
    @hosted def companion: Member.Term = ???
  }

  implicit class SemanticTemplateParameterOps(val tree: Templ.Param) extends AnyVal {
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
    @hosted def packages: Seq[Member.Term] = ???
    @hosted def packages(name: Name): Member.Term = ???
    @hosted def packages(name: String): Member.Term = ???
    @hosted def packages(name: scala.Symbol): Member.Term = ???
    @hosted def ctor: Ctor = ???
    @hosted def ctors: Seq[Ctor] = ???
    @hosted def classes: Seq[Member.Type] = ???
    @hosted def classes(name: Name): Member.Type = ???
    @hosted def classes(name: String): Member.Type = ???
    @hosted def classes(name: scala.Symbol): Member.Type = ???
    @hosted def traits: Seq[Member.Type] = ???
    @hosted def traits(name: Name): Member.Type = ???
    @hosted def traits(name: String): Member.Type = ???
    @hosted def traits(name: scala.Symbol): Member.Type = ???
    @hosted def objects: Seq[Member.Term] = ???
    @hosted def objects(name: Name): Member.Term = ???
    @hosted def objects(name: String): Member.Term = ???
    @hosted def objects(name: scala.Symbol): Member.Term = ???
    @hosted def vars: Seq[Term.Name] = ???
    @hosted def vars(name: Name): Term.Name = ???
    @hosted def vars(name: String): Term.Name = ???
    @hosted def vars(name: scala.Symbol): Term.Name = ???
    @hosted def vals: Seq[Term.Name] = ???
    @hosted def vals(name: Name): Term.Name = ???
    @hosted def vals(name: String): Term.Name = ???
    @hosted def vals(name: scala.Symbol): Term.Name = ???
    @hosted def defs: Seq[Member.Term] = ???
    @hosted def defs(name: Name): Member.Term = ???
    @hosted def defs(name: String): Member.Term = ???
    @hosted def defs(name: scala.Symbol): Member.Term = ???
    @hosted def types: Seq[Member.Type] = ???
    @hosted def types(name: Name): Member.Type = ???
    @hosted def types(name: String): Member.Type = ???
    @hosted def types(name: scala.Symbol): Member.Type = ???
    @hosted def params: Seq[Templ.Param] = ???
    @hosted def paramss: Seq[Seq[Templ.Param]] = ???
    @hosted def params(name: Name): Templ.Param = ???
    @hosted def params(name: String): Templ.Param = ???
    @hosted def params(name: scala.Symbol): Templ.Param = ???
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
    @hosted def isReference: Boolean = ???
  }
}
