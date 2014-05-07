package scala.reflect
package semantic

import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic.errors.wrapHosted

trait ScopeOps {
  private[semantic] implicit class SemanticIterableOps[T](val members: Seq[T]) {
    @hosted def findUnique: T = members match {
      case Seq(unique) => succeed(unique)
      case Seq() => fail(ReflectionException("no members found"))
      case _ => fail(ReflectionException("multiple members found"))
    }
  }

  implicit class SemanticScopeOps(tree: Scope) {
    @hosted def members: Seq[Member] = delegate
    @hosted def members(name: Name): Overload[Member] = wrapHosted(_.members(tree)).map(Overload.apply)
    @hosted private[semantic] def allMembers[T: ClassTag]: Seq[T] = {
      members.map(_.collect { case x: T => x })
    }
    @hosted private[semantic] def uniqueMember[T: ClassTag](s_name: String): T = {
      val isTerm = classOf[Member.Term].isAssignableFrom(classTag[T].runtimeClass)
      val name = if (isTerm) Term.Name(s_name)(SourceContext.None) else Type.Name(s_name)(SourceContext.None)
      members(name).map(_.alts).map(_.collect { case x: T => x }).flatMap(_.findUnique)
    }
  }

  implicit class SemanticTopLevelScopeOps(tree: Scope.TopLevel) {
    @hosted def packages: Seq[Pkg.Named] = tree.allMembers[Pkg.Named]
    @hosted def packages(name: Name): Pkg.Named = tree.uniqueMember[Pkg.Named](name.toString)
    @hosted def packages(name: String): Pkg.Named = tree.uniqueMember[Pkg.Named](name.toString)
    @hosted def packages(name: scala.Symbol): Pkg.Named = tree.uniqueMember[Pkg.Named](name.toString)
    @hosted def pkgobject: Pkg.Object = tree.allMembers[Pkg.Object].flatMap(_.findUnique)
  }

  implicit class SemanticTemplateScopeOps(tree: Scope.Template) {
    // TODO: directSuperclasses and others
    @hosted def superclasses: Seq[Member.Template] = tree match {
      case x: Aux.Template => x.tpe.flatMap(_.superclasses)
      case x: Member.Template => x.templ.superclasses
      case x: Type => x.supertypes.flatMap(tpes => supertypesToMembers(tpes))
    }
    @hosted def supertypes: Seq[Type] = tree match {
      case x: Aux.Template => x.tpe.flatMap(_.supertypes)
      case x: Member.Template => x.templ.supertypes
      case x: Type => wrapHosted(_.supertypes(x))
    }
    @hosted def self: Aux.Self = tree match {
      case x: Aux.Template => succeed(x.self)
      case x: Member.Template => succeed(x.templ.self)
      case x: Type => wrapHosted(_.self(x))
    }
    @hosted def subclasses: Seq[Member.Template] = tree match {
      case x: Aux.Template => x.tpe.flatMap(_.superclasses)
      case x: Member.Template => x.templ.subclasses
      case x: Type => wrapHosted(_.subclasses(x))
    }
    @hosted def ctor: Ctor.Primary = ctors.flatMap(_.collect { case prim: Ctor.Primary => prim }.findUnique)
    @hosted def ctors: Seq[Ctor] = delegate
  }

  implicit class SemanticBlockScopeOps(tree: Scope.Block) {
    @hosted def classes: Seq[Defn.Class] = tree.allMembers[Defn.Class]
    @hosted def classes(name: Name): Defn.Class = tree.uniqueMember[Defn.Class](name.toString)
    @hosted def classes(name: String): Defn.Class = tree.uniqueMember[Defn.Class](name.toString)
    @hosted def classes(name: scala.Symbol): Defn.Class = tree.uniqueMember[Defn.Class](name.toString)
    @hosted def traits: Seq[Defn.Trait] = tree.allMembers[Defn.Trait]
    @hosted def traits(name: Name): Defn.Trait = tree.uniqueMember[Defn.Trait](name.toString)
    @hosted def traits(name: String): Defn.Trait = tree.uniqueMember[Defn.Trait](name.toString)
    @hosted def traits(name: scala.Symbol): Defn.Trait = tree.uniqueMember[Defn.Trait](name.toString)
    @hosted def objects: Seq[Defn.Object] = tree.allMembers[Defn.Object]
    @hosted def objects(name: Name): Defn.Object = tree.uniqueMember[Defn.Object](name.toString)
    @hosted def objects(name: String): Defn.Object = tree.uniqueMember[Defn.Object](name.toString)
    @hosted def objects(name: scala.Symbol): Defn.Object = tree.uniqueMember[Defn.Object](name.toString)
    @hosted def vars: Seq[Term.Name] = tree.allMembers[Term.Name]
    @hosted def vars(name: Name): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def vars(name: String): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def vars(name: scala.Symbol): Term.Name = tree.uniqueMember[Term.Name](name.toString)
  }

  implicit class SemanticRefineScopeOps(tree: Scope.Refine) {
    @hosted def defs: Seq[Member.Def] = tree.allMembers[Member.Def]
    @hosted def defs(name: Name): Member.Def = tree.uniqueMember[Member.Def](name.toString)
    @hosted def defs(name: String): Member.Def = tree.uniqueMember[Member.Def](name.toString)
    @hosted def defs(name: scala.Symbol): Member.Def = tree.uniqueMember[Member.Def](name.toString)
  }

  implicit class SemanticExistentialScopeOps(tree: Scope.Existential) {
    @hosted def vals: Seq[Term.Name] = tree.allMembers[Term.Name]
    @hosted def vals(name: Name): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def vals(name: String): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def vals(name: scala.Symbol): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def types: Seq[Member.AbstractOrAliasType] = tree.allMembers[Member.AbstractOrAliasType]
    @hosted def types(name: Name): Member.AbstractOrAliasType = tree.uniqueMember[Member.AbstractOrAliasType](name.toString)
    @hosted def types(name: String): Member.AbstractOrAliasType = tree.uniqueMember[Member.AbstractOrAliasType](name.toString)
    @hosted def types(name: scala.Symbol): Member.AbstractOrAliasType = tree.uniqueMember[Member.AbstractOrAliasType](name.toString)
  }

  implicit class SemanticParamsScopeOps(tree: Scope.Params) {
    @hosted def params: Seq[Aux.Param.Named] = tree.allMembers[Aux.Param.Named]
    @hosted def params(name: Name): Aux.Param.Named = tree.uniqueMember[Aux.Param.Named](name.toString)
    @hosted def params(name: String): Aux.Param.Named = tree.uniqueMember[Aux.Param.Named](name.toString)
    @hosted def params(name: scala.Symbol): Aux.Param.Named = tree.uniqueMember[Aux.Param.Named](name.toString)
    @hosted def tparams: Seq[Aux.TypeParam.Named] = tree.allMembers[Aux.TypeParam.Named]
    @hosted def tparams(name: Name): Aux.TypeParam.Named = tree.uniqueMember[Aux.TypeParam.Named](name.toString)
    @hosted def tparams(name: String): Aux.TypeParam.Named = tree.uniqueMember[Aux.TypeParam.Named](name.toString)
    @hosted def tparams(name: scala.Symbol): Aux.TypeParam.Named = tree.uniqueMember[Aux.TypeParam.Named](name.toString)
  }

  implicit class SemanticTemplateOps(tree: Aux.Template) {
    @hosted def tpe: Type = tree.internalTpe
  }
}
