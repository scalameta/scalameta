package scala.reflect
package semantic

import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic.errors.wrapHosted

trait MemberOps {
  implicit class SemanticMemberOps(tree: Member) {
    // TODO: expose type parameter instantiation facilities, e.g. `def foo[T]: T = ...` => `def foo: Int = ...`
    def ref: Ref = tree match {
      case self: Aux.Self => self.name.getOrElse(Term.This(None))
      case named: Has.Name => named.name
    }
    @hosted def overrides: Seq[Member] = tree match {
      case mte: Member.Term => wrapHosted(_.overrides(mte))
      case mty: Member.Type => wrapHosted(_.overrides(mty))
    }
    def annots: Seq[Mod.Annot] = tree.mods.collect{ case annot: Mod.Annot => annot }
    def doc: Option[Mod.Doc] = tree.mods.collect{ case doc: Mod.Doc => doc }.headOption
    def isVal: Boolean = tree.isInstanceOf[Term.Name] && (tree.parent.map(parent => parent.isInstanceOf[Decl.Val] || parent.isInstanceOf[Defn.Val]).getOrElse(false))
    def isVar: Boolean = tree.isInstanceOf[Term.Name] && (tree.parent.map(parent => parent.isInstanceOf[Decl.Var] || parent.isInstanceOf[Defn.Var]).getOrElse(false))
    def isDef: Boolean = tree.isInstanceOf[Member.Def]
    def isType: Boolean = tree.isInstanceOf[Member.AbstractOrAliasType]
    def isClass: Boolean = tree.isInstanceOf[Defn.Class]
    def isTrait: Boolean = tree.isInstanceOf[Defn.Trait]
    def isObject: Boolean = tree.isInstanceOf[Defn.Object]
    def isPkg: Boolean = tree.isInstanceOf[Pkg]
    def isPkgObject: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Package])
    def isJava: Boolean = ??? // TODO: need special trees for Java artifacts
    def isPrivate: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Private])
    def isProtected: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Protected])
    def isPublic: Boolean = !tree.isPrivate && !tree.isProtected
    def isImplicit: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Implicit])
    def isFinal: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Final])
    def isSealed: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Sealed])
    @hosted def isOverride: Boolean = tree.overrides.map(_.nonEmpty)
    def isCase: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Case])
    def isAbstract: Boolean = (tree.mods.exists(_.isInstanceOf[Mod.Abstract]) || tree.isInstanceOf[Decl]) && !isAbstractOverride
    def isCovariant: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Covariant])
    def isContravariant: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Contravariant])
    def isLazy: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Lazy])
    def isAbstractOverride: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Abstract]) && tree.mods.exists(_.isInstanceOf[Mod.Override])
    def isMacro: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Macro])
    def isByNameParam: Boolean = ???
    def isVarargParam: Boolean = ???
    def isValParam: Boolean = tree.mods.exists(_.isInstanceOf[Mod.ValParam])
    def isVarParam: Boolean = tree.mods.exists(_.isInstanceOf[Mod.VarParam])
  }

  implicit class SemanticTermMemberOps(tree: Member.Term) {
    def ref: Term.Ref = new SemanticMemberOps(tree).ref.asInstanceOf[Term.Ref]
    @hosted def overrides: Seq[Member.Term] = new SemanticMemberOps(tree).overrides.map(_.asInstanceOf[Seq[Member.Term]])
  }

  implicit class SemanticTypeMemberOps(tree: Member.Type) {
    def ref: Type.Ref = new SemanticMemberOps(tree).ref.asInstanceOf[Type.Ref]
    @hosted def overrides: Seq[Member.Type] = new SemanticMemberOps(tree).overrides.map(_.asInstanceOf[Seq[Member.Type]])
  }

  implicit class SemanticDefMemberOps(tree: Member.Def) {
    @hosted def tpe: core.Type = tree match {
      case x: Decl.Def => succeed(x.decltpe)
      case x: Decl.Procedure => ??? // TODO: t"Unit"
      case x: Defn.Def => x.body.tpe
      case x: Defn.Procedure => ??? // TODO: t"Unit"
    }
  }

  implicit class SemanticTemplateMemberOps(tree: Member.Template) {
    @hosted def superclasses: Seq[Member.Template] = tree.ref.toTypeRef.superclasses
    @hosted def supertypes: Seq[core.Type] = tree.ref.toTypeRef.supertypes
    @hosted def subclasses: Seq[Member.Template] = tree.ref.toTypeRef.subclasses
    @hosted def self: Aux.Self = succeed(tree.templ.self)
    @hosted def companion: Member.Template = tree match {
      case _: Defn.Class => findCompanion{ case x: Defn.Object => x }
      case _: Defn.Trait => findCompanion{ case x: Defn.Object => x }
      case _: Defn.Object => findCompanion{ case x: Defn.Class => x; case x: Defn.Trait => x }
    }
    @hosted private[semantic] def findCompanion[T <: Member.Template](f: PartialFunction[Member, T]): T = {
      val companionName = {
        if (tree.name.isInstanceOf[core.Term.Name]) core.Type.Name(tree.name.value)(isBackquoted = false) else
        core.Term.Name(tree.name.value)(isBackquoted = false)
      }
      val candidates = tree.owner.members(companionName)
      candidates.flatMap{candidates =>
        val relevant = candidates.collect(f).headOption
        relevant.map(result => succeed(result)).getOrElse(fail(ReflectionException(s"companion not found")))
      }
    }
  }

  implicit class SemanticDeclValOps(tree: Decl.Val) {
    @hosted def tpe: core.Type = succeed(tree.decltpe)
  }

  implicit class SemanticDeclVarOps(tree: Decl.Var) {
    @hosted def tpe: core.Type = succeed(tree.decltpe)
  }

  implicit class SemanticDefnValOps(tree: Defn.Val) {
    @hosted def tpe: core.Type = tree.rhs.tpe
  }

  implicit class SemanticDefnVarOps(tree: Defn.Var) {
    @hosted def tpe: core.Type = tree.rhs.map(_.tpe).getOrElse(succeed(tree.decltpe.get))
  }

  implicit class SemanticDefnClassOps(tree: Defn.Class) {
    @hosted def companion: Object = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Object])
  }

  implicit class SemanticDefnTraitOps(tree: Defn.Trait) {
    @hosted def companion: Object = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Object])
  }

  implicit class SemanticDefnObjectOps(tree: Defn.Object) {
    @hosted def companion: Member.Template with Member.Type = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Member.Template with Member.Type])
  }

  implicit class SemanticPkgObjectOps(tree: Defn.Object) {
    @hosted def companion: Member.Template with Member.Type = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Member.Template with Member.Type])
  }

  implicit class SemanticCtorOps(tree: Ctor) {
    @hosted def tpe: core.Type = tree.internalTpe
  }

  implicit class SemanticParentOps(tree: Aux.Param) {
    @hosted def ctor: Ctor = tree.attrs.flatMap(_.collect{ case defn: Attribute.Defn => defn } match {
      case Attribute.Defn(defn: Ctor) :: Nil => succeed(defn)
      case _ => fail(ReflectionException("typecheck has failed"))
    })
  }

  implicit class SemanticSelfOps(tree: Aux.Self) {
    def ref: Term.This = new SemanticMemberOps(tree).ref.asInstanceOf[Term.This]
    @hosted def tpe: Type = tree.internalTpe
  }

  implicit class SemanticParamOps(tree: Aux.Param) {
    @hosted def tpe: Aux.ParamType = tree.internalParamTpe
  }
}
