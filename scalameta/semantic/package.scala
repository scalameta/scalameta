package scala.meta

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.{Seq => _}
import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.reflect.{ClassTag, classTag}

package object semantic {
  @root trait Mark
  def mark(): Mark = ???

  @root trait Attr
  object Attr {
    // TODO: design the attr hierarchy of semantic facts that can be figured out about trees
    // TODO: examples: a type a tree, a definition/definitions the tree refers to, the scope that a tree belongs to, maybe a desugaring, etc
    // TODO: see https://github.com/JetBrains/intellij-scala/blob/master/src/org/jetbrains/plugins/scala/lang/resolve/ScalaResolveResult.scala#L24
  }

  implicit class RichTree(val tree: Tree) extends AnyVal {
    @hosted def attrs: Seq[Attr] = delegate
  }

  implicit class SemanticTermOps(val tree: Term) extends AnyVal {
    @hosted def tpe: Type = ???
  }

  implicit class SemanticTypeOps(val tree: Type) extends AnyVal {
    @hosted def <:<(other: Type): Boolean = delegate
    @hosted def weak_<:<(other: Type): Boolean = ???
    @hosted def widen: Type = ???
    @hosted def dealias: Type = ???
    @hosted def erasure: Type = delegate
    @hosted def companion: Type.Ref = ???
  }

  @hosted def lub(tpes: Seq[Type]): Type = delegate
  @hosted def glb(tpes: Seq[Type]): Type = delegate

  implicit class SemanticRefOps(val tree: Ref) extends AnyVal {
    @hosted def defns: Seq[Member] = ???
    @hosted def defn: Member = ???
  }

  implicit class SemanticTypeRefOps(val tree: Type.Ref) extends AnyVal {
    // NOTE: we can't refine the return type of Ref.defns and Ref.defn
    // because a Type.Ref can refer to both types (regular types) and terms (singleton types)
  }

  implicit class SemanticTermRefOps(val tree: Term.Ref) extends AnyVal {
    @hosted def defns: Seq[Member.Term] = ???
    @hosted def defn: Member.Term = ???
  }

  implicit class SemanticTermMembersOps[A <: Member.Term](val tree: Seq[A]) extends AnyVal {
    def resolve(tpes: Seq[meta.Type]): A = ???
  }

  implicit class SemanticMemberOps(val tree: Member) extends AnyVal {
    def ref: Ref = ???
    @hosted def parents: Seq[Member] = ???
    @hosted def children: Seq[Member] = ???
    def annots: Seq[Mod.Annot] = tree.mods.collect{ case annot: Mod.Annot => annot }
    def isVal: Boolean = ???
    def isVar: Boolean = ???
    def isDef: Boolean = ???
    def isMacro: Boolean = ???
    def isAbstractType: Boolean = ???
    def isAliasType: Boolean = ???
    def isClass: Boolean = ???
    def isTrait: Boolean = ???
    def isObject: Boolean = ???
    def isPkg: Boolean = ???
    def isPkgObject: Boolean = ???
    def isPrivate: Boolean = ???
    def isProtected: Boolean = ???
    def isPublic: Boolean = ???
    @hosted def accessBoundary: Member = ???
    def isImplicit: Boolean = ???
    def isFinal: Boolean = ???
    def isSealed: Boolean = ???
    @hosted def isOverride: Boolean = ???
    def isCase: Boolean = ???
    def isAbstract: Boolean = ???
    def isCovariant: Boolean = ???
    def isContravariant: Boolean = ???
    def isLazy: Boolean = ???
    def isAbstractOverride: Boolean = ???
    def isByNameParam: Boolean = ???
    def isVarargParam: Boolean = ???
    def isValParam: Boolean = ???
    def isVarParam: Boolean = ???
  }

  implicit class SemanticTermMemberOps(val tree: Member.Term) extends AnyVal {
    def ref: Term.Ref = ???
    @hosted def overridden: Seq[Member.Term] = ???
    @hosted def overriding: Seq[Member.Term] = ???
  }

  implicit class SemanticTypeMemberOps(val tree: Member.Type) extends AnyVal {
    def ref: Type.Ref = new SemanticMemberOps(tree).ref.asInstanceOf[Type.Ref]
    @hosted def overridden: Seq[Member.Type] = ???
    @hosted def overriding: Seq[Member.Type] = ???
  }

  implicit class SemanticDefMemberOps(val tree: Member.Def) extends AnyVal {
    @hosted def tpe: meta.Type = ???
  }

  implicit class SemanticTemplateMemberOps(val tree: Has.Template) extends AnyVal {
    @hosted def parents: Seq[Member.Type with Has.Template] = ???
    @hosted def children: Seq[Has.Template] = ???
    @hosted def self: Aux.Self = ???
    @hosted def companion: Has.Template = ???
  }

  implicit class SemanticDeclValOps(val tree: Decl.Val) extends AnyVal {
    @hosted def tpe: meta.Type = ???
  }

  implicit class SemanticDeclVarOps(val tree: Decl.Var) extends AnyVal {
    @hosted def tpe: meta.Type = ???
  }

  implicit class SemanticDefnValOps(val tree: Defn.Val) extends AnyVal {
    @hosted def tpe: meta.Type = ???
  }

  implicit class SemanticDefnVarOps(val tree: Defn.Var) extends AnyVal {
    @hosted def tpe: meta.Type = ???
  }

  implicit class SemanticDefnClassOps(val tree: Defn.Class) extends AnyVal {
    @hosted def companion: Object = ???
  }

  implicit class SemanticDefnTraitOps(val tree: Defn.Trait) extends AnyVal {
    @hosted def companion: Object = ???
  }

  implicit class SemanticDefnObjectOps(val tree: Defn.Object) extends AnyVal {
    @hosted def companion: Has.Template with Member.Type = ???
  }

  implicit class SemanticPkgObjectOps(val tree: Defn.Object) extends AnyVal {
    @hosted def companion: Has.Template with Member.Type = ???
  }

  implicit class SemanticCtorOps(val tree: Ctor) extends AnyVal {
    @hosted def tpe: meta.Type = ???
  }

  implicit class SemanticParentOps(val tree: Param) extends AnyVal {
    @hosted def ctor: Ctor = ???
  }

  implicit class SemanticSelfOps(val tree: Aux.Self) extends AnyVal {
    def ref: Term.This = ???
    @hosted def tpe: Type = ???
  }

  implicit class SemanticParamOps(val tree: Param) extends AnyVal {
    @hosted def tpe: Param.Type = ???
  }

  implicit class SemanticTemplateOps(val tree: Aux.Template) extends AnyVal {
    @hosted def tpe: Type = ???
  }

  implicit class SemanticTreeOps(val tree: Tree) extends AnyVal {
    @hosted def owner: Scope = ???
  }

  implicit class SemanticScopeOps(val tree: Scope) extends AnyVal {
    @hosted def members: Seq[Member] = ???
    @hosted def members(name: Name): Seq[Member] = ???
  }

  implicit class SemanticTopLevelScopeOps(val tree: Scope.TopLevel) extends AnyVal {
    @hosted def packages: Seq[Pkg] = ???
    @hosted def packages(name: Name): Pkg = ???
    @hosted def packages(name: String): Pkg = ???
    @hosted def packages(name: scala.Symbol): Pkg = ???
    @hosted def pkgobject: Defn.Object = ???
  }

  implicit class SemanticTemplateScopeOps(val tree: Scope.Template) extends AnyVal {
    @hosted def parents: Seq[Member.Type with Has.Template] = ???
    @hosted def children: Seq[Has.Template] = ???
    @hosted def self: Aux.Self = ???
    @hosted def ctor: Ctor.Primary = ???
    @hosted def ctors: Seq[Ctor] = ???
  }

  implicit class SemanticBlockScopeOps(val tree: Scope.Block) extends AnyVal {
    @hosted def classes: Seq[Defn.Class] = ???
    @hosted def classes(name: Name): Defn.Class = ???
    @hosted def classes(name: String): Defn.Class = ???
    @hosted def classes(name: scala.Symbol): Defn.Class = ???
    @hosted def traits: Seq[Defn.Trait] = ???
    @hosted def traits(name: Name): Defn.Trait = ???
    @hosted def traits(name: String): Defn.Trait = ???
    @hosted def traits(name: scala.Symbol): Defn.Trait = ???
    @hosted def objects: Seq[Defn.Object] = ???
    @hosted def objects(name: Name): Defn.Object = ???
    @hosted def objects(name: String): Defn.Object = ???
    @hosted def objects(name: scala.Symbol): Defn.Object = ???
    @hosted def vars: Seq[Term.Name] = ???
    @hosted def vars(name: Name): Term.Name = ???
    @hosted def vars(name: String): Term.Name = ???
    @hosted def vars(name: scala.Symbol): Term.Name = ???
  }

  implicit class SemanticRefineScopeOps(val tree: Scope.Refine) extends AnyVal {
    @hosted def defs: Seq[Member.Def] = ???
    @hosted def defs(name: Name): Member.Def = ???
    @hosted def defs(name: String): Member.Def = ???
    @hosted def defs(name: scala.Symbol): Member.Def = ???
    @hosted def macros: Seq[Defn.Macro] = ???
    @hosted def macros(name: Name): Defn.Macro = ???
    @hosted def macros(name: String): Defn.Macro = ???
    @hosted def macros(name: scala.Symbol): Defn.Macro = ???
  }

  implicit class SemanticExistentialScopeOps(val tree: Scope.Existential) extends AnyVal {
    @hosted def vals: Seq[Term.Name] = ???
    @hosted def vals(name: Name): Term.Name = ???
    @hosted def vals(name: String): Term.Name = ???
    @hosted def vals(name: scala.Symbol): Term.Name = ???
    @hosted def types: Seq[Member.Type] = ???
    @hosted def types(name: Name): Member.Type = ???
    @hosted def types(name: String): Member.Type = ???
    @hosted def types(name: scala.Symbol): Member.Type = ???
  }

  implicit class SemanticParamsScopeOps(val tree: Scope.Params) extends AnyVal {
    @hosted def params: Seq[Param.Named] = ???
    @hosted def params(name: Name): Param.Named = ???
    @hosted def params(name: String): Param.Named = ???
    @hosted def params(name: scala.Symbol): Param.Named = ???
    @hosted def tparams: Seq[TypeParam.Named] = ???
    @hosted def tparams(name: Name): TypeParam.Named = ???
    @hosted def tparams(name: String): TypeParam.Named = ???
    @hosted def tparams(name: scala.Symbol): TypeParam.Named = ???
  }
}
