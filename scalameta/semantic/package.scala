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
    @leaf class Defn(defn: Tree) extends Attr
    @leaf class Type(tpe: Param.Type) extends Attr
    @leaf class InferredTargs(targs: Seq[Type]) extends Attr
    @leaf class InferredVargs(vargs: Seq[Term]) extends Attr
    @leaf class MacroExpansion(tree: Tree) extends Attr
    // TODO: design additional attrs for other aspects of typechecking
    // TODO: see https://github.com/JetBrains/intellij-scala/blob/master/src/org/jetbrains/plugins/scala/lang/resolve/ScalaResolveResult.scala#L24
  }

  implicit class RichTree(val tree: Tree) extends AnyVal {
    @hosted def attrs: Seq[Attr] = delegate
    @hosted private[semantic] def internalTpe: Type = attrs.flatMap(_.collect{ case tpe: Attr.Type => tpe } match {
      case Attr.Type(tpe: Type) :: Nil => succeed(tpe)
      case _ => fail("typecheck has failed")
    })
    @hosted private[semantic] def internalParamTpe: Param.Type = attrs.flatMap(_.collect{ case tpe: Attr.Type => tpe } match {
      case Attr.Type(paramTpe) :: Nil => succeed(paramTpe)
      case _ => fail("typecheck has failed")
    })
  }

  implicit class SemanticTermOps(val tree: Term) extends AnyVal {
    @hosted def tpe: Type = tree.internalTpe
  }

  implicit class SemanticTypeOps(val tree: Type) extends AnyVal {
    @hosted def <:<(other: Type): Boolean = delegate
    @hosted def weak_<:<(other: Type): Boolean = ??? // TODO: express this as something like (tree <:< other) || (numeric "subtyping")
    @hosted def widen: Type = tree match {
      case Type.Singleton(ref: Term) => ref.tpe.flatMap(_.widen)
      case _ => succeed(tree)
    }
    @hosted def dealias: Type = ??? // TODO: this can be expressed as an isAliasType check + a hygienic-equality-based substitution
    @hosted def erasure: Type = delegate
    @hosted def companion: Type.Ref = tree match {
      case ref: Type.Ref => ref.defns.flatMap {
        case Seq(t: Has.Template) => t.companion
        case _ => fail("companion not found")
      }.map(_.ref.toTypeRef)
      case _ => fail("companion not found")
    }
  }

  @hosted private[semantic] def supertypesToMembers(tpes: Seq[Type]): Seq[Has.Template] = {
    def extractTemplate(ref: Type.Ref) = {
      for {
        defns <- ref.defns
        result <- defns match {
          case Seq(t: Has.Template) => succeed(t)
          case d => fail(s"unexpected ref $ref to $d returned from supertypes")
        }
      } yield result
    }
    succeed(tpes) mmap {
      case ref: Type.Ref => extractTemplate(ref)
      case Type.Apply(ref: Type.Ref, _) => extractTemplate(ref)
      case tpe => fail(s"unexpected type $tpe returned from supertypes")
    }
  }

  @hosted def lub(tpes: Seq[Type]): Type = delegate
  @hosted def glb(tpes: Seq[Type]): Type = delegate

  implicit class SemanticRefOps(val tree: Ref) extends AnyVal {
    private[semantic] def toTypeRef: Type.Ref = ??? // TODO: t"$tree"
    @hosted def defns: Seq[Member] = wrapHosted(_.defns(tree).collect{ case m: Member => m })
    @hosted def defn: Member = defns.flatMap(_.findUnique)
  }

  implicit class SemanticTypeRefOps(val tree: Type.Ref) extends AnyVal {
    // NOTE: we can't refine the return type of Ref.defns and Ref.defn
    // because a Type.Ref can refer to both types (regular types) and terms (singleton types)
  }

  implicit class SemanticTermRefOps(val tree: Term.Ref) extends AnyVal {
    @hosted def defns: Seq[Member.Term] = (tree: Ref).defns.flatMap(defns => {
      if (defns.exists(!_.isInstanceOf[Member.Term])) fail(s"unexpected $defns for ref $tree")
      else succeed(defns.asInstanceOf[Seq[Member.Term]])
    })
    @hosted def defn: Member.Term = defns.flatMap(_.findUnique)
  }

  implicit class SemanticMembers[A <: Member.Term](val tree: Seq[A]) extends AnyVal {
    def resolve(tpes: Seq[meta.Type]): A = ???
  }

  implicit class SemanticMemberOps(val tree: Member) extends AnyVal {
    def ref: Ref = tree match {
      case self: Aux.Self => self.name.getOrElse(Term.This(None))
      case named: Has.Name => named.name
    }
    @hosted def overridden: Seq[Member] = delegate
    @hosted def overriding: Seq[Member] = delegate
    def annots: Seq[Mod.Annot] = tree.mods.collect{ case annot: Mod.Annot => annot }
    def doc: Option[Mod.Doc] = tree.mods.collect{ case doc: Mod.Doc => doc }.headOption
    def isVal: Boolean = tree.isInstanceOf[Term.Name] && (tree.parent.map(parent => parent.isInstanceOf[Decl.Val] || parent.isInstanceOf[Defn.Val]).getOrElse(false))
    def isVar: Boolean = tree.isInstanceOf[Term.Name] && (tree.parent.map(parent => parent.isInstanceOf[Decl.Var] || parent.isInstanceOf[Defn.Var]).getOrElse(false))
    def isDef: Boolean = tree.isInstanceOf[Member.Def]
    def isMacro: Boolean = tree.isInstanceOf[Defn.Macro]
    def isType: Boolean = tree.isInstanceOf[Member.Type]
    def isAbstractType: Boolean = tree.isInstanceOf[Decl.Type]
    def isAliasType: Boolean = tree.isInstanceOf[Defn.Type]
    def isClass: Boolean = tree.isInstanceOf[Defn.Class]
    def isTrait: Boolean = tree.isInstanceOf[Defn.Trait]
    def isObject: Boolean = tree.isInstanceOf[Defn.Object]
    def isPkg: Boolean = tree.isInstanceOf[Pkg]
    def isPkgObject: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Package])
    def isJava: Boolean = ???
    def isPrivate: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Private])
    def isProtected: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Protected])
    def isPublic: Boolean = !tree.isPrivate && !tree.isProtected
    def isImplicit: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Implicit])
    def isFinal: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Final]) || tree.isObject
    def isSealed: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Sealed])
    @hosted def isOverride: Boolean = tree.overridden.map(_.nonEmpty)
    def isCase: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Case])
    def isAbstract: Boolean = (tree.mods.exists(_.isInstanceOf[Mod.Abstract]) || tree.isInstanceOf[Decl]) && !isAbstractOverride
    def isCovariant: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Covariant])
    def isContravariant: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Contravariant])
    def isLazy: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Lazy])
    def isAbstractOverride: Boolean = tree.mods.exists(_.isInstanceOf[Mod.Abstract]) && tree.mods.exists(_.isInstanceOf[Mod.Override])
    def isByNameParam: Boolean = ???
    def isVarargParam: Boolean = ???
    def isValParam: Boolean = tree.mods.exists(_.isInstanceOf[Mod.ValParam])
    def isVarParam: Boolean = tree.mods.exists(_.isInstanceOf[Mod.VarParam])
  }

  implicit class SemanticTermMemberOps(val tree: Member.Term) extends AnyVal {
    def ref: Term.Ref = new SemanticMemberOps(tree).ref.asInstanceOf[Term.Ref]
    @hosted def overridden: Seq[Member.Term] = new SemanticMemberOps(tree).overridden.map(_.asInstanceOf[Seq[Member.Term]])
    @hosted def overriding: Seq[Member.Term] = new SemanticMemberOps(tree).overriding.map(_.asInstanceOf[Seq[Member.Term]])
  }

  implicit class SemanticTypeMemberOps(val tree: Member.Type) extends AnyVal {
    def ref: Type.Ref = new SemanticMemberOps(tree).ref.asInstanceOf[Type.Ref]
    @hosted def overridden: Seq[Member.Type] = new SemanticMemberOps(tree).overridden.map(_.asInstanceOf[Seq[Member.Type]])
    @hosted def overriding: Seq[Member.Type] = new SemanticMemberOps(tree).overriding.map(_.asInstanceOf[Seq[Member.Type]])
  }

  implicit class SemanticDefMemberOps(val tree: Member.Def) extends AnyVal {
    @hosted def tpe: meta.Type = tree match {
      case x: Decl.Def => succeed(x.decltpe)
      case x: Decl.Procedure => ??? // TODO: t"Unit"
      case x: Defn.Def => x.body.tpe
      case x: Defn.Procedure => ??? // TODO: t"Unit"
    }
  }

  implicit class SemanticTemplateMemberOps(val tree: Has.Template) extends AnyVal {
    @hosted def parents: Seq[Member.Type with Has.Template] = tree.ref.toTypeRef.parents
    @hosted def children: Seq[Has.Template] = tree.ref.toTypeRef.children
    @hosted def self: Aux.Self = succeed(tree.templ.self)
    @hosted def companion: Has.Template = tree match {
      case _: Defn.Class => findCompanion{ case x: Defn.Object => x }
      case _: Defn.Trait => findCompanion{ case x: Defn.Object => x }
      case _: Defn.Object => findCompanion{ case x: Defn.Class => x; case x: Defn.Trait => x }
    }
    @hosted private[semantic] def findCompanion[T <: Has.Template](f: PartialFunction[Member, T]): T = {
      val companionName = {
        if (tree.name.isInstanceOf[meta.Term.Name]) meta.Type.Name(tree.name.value, isBackquoted = false) else
        meta.Term.Name(tree.name.value, isBackquoted = false)
      }
      val candidates = tree.owner.flatMap(_.members(companionName))
      candidates.flatMap{candidates =>
        val relevant = candidates.collect(f).headOption
        relevant.map(result => succeed(result)).getOrElse(fail("companion not found"))
      }
    }
  }

  implicit class SemanticDeclValOps(val tree: Decl.Val) extends AnyVal {
    @hosted def tpe: meta.Type = succeed(tree.decltpe)
  }

  implicit class SemanticDeclVarOps(val tree: Decl.Var) extends AnyVal {
    @hosted def tpe: meta.Type = succeed(tree.decltpe)
  }

  implicit class SemanticDefnValOps(val tree: Defn.Val) extends AnyVal {
    @hosted def tpe: meta.Type = tree.rhs.tpe
  }

  implicit class SemanticDefnVarOps(val tree: Defn.Var) extends AnyVal {
    @hosted def tpe: meta.Type = tree.rhs.map(_.tpe).getOrElse(succeed(tree.decltpe.get))
  }

  implicit class SemanticDefnClassOps(val tree: Defn.Class) extends AnyVal {
    @hosted def companion: Object = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Object])
  }

  implicit class SemanticDefnTraitOps(val tree: Defn.Trait) extends AnyVal {
    @hosted def companion: Object = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Object])
  }

  implicit class SemanticDefnObjectOps(val tree: Defn.Object) extends AnyVal {
    @hosted def companion: Has.Template with Member.Type = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Has.Template with Member.Type])
  }

  implicit class SemanticPkgObjectOps(val tree: Defn.Object) extends AnyVal {
    @hosted def companion: Has.Template with Member.Type = new SemanticTemplateMemberOps(tree).companion.map(_.asInstanceOf[Has.Template with Member.Type])
  }

  implicit class SemanticCtorOps(val tree: Ctor) extends AnyVal {
    @hosted def tpe: meta.Type = tree.internalTpe
  }

  implicit class SemanticParentOps(val tree: Param) extends AnyVal {
    @hosted def ctor: Ctor = tree.attrs.flatMap(_.collect{ case defn: Attr.Defn => defn } match {
      case Attr.Defn(defn: Ctor) :: Nil => succeed(defn)
      case _ => fail("typecheck has failed")
    })
  }

  implicit class SemanticSelfOps(val tree: Aux.Self) extends AnyVal {
    def ref: Term.This = new SemanticMemberOps(tree).ref.asInstanceOf[Term.This]
    @hosted def tpe: Type = tree.internalTpe
  }

  implicit class SemanticParamOps(val tree: Param) extends AnyVal {
    @hosted def tpe: Param.Type = tree.internalParamTpe
  }

  implicit class SemanticTemplateOps(val tree: Aux.Template) extends AnyVal {
    @hosted def tpe: Type = tree.internalTpe
  }

  private[semantic] implicit class SemanticIterableOps[T](val members: Seq[T]) extends AnyVal {
    @hosted def findUnique: T = members match {
      case Seq(unique) => succeed(unique)
      case Seq() => fail("no members found")
      case _ => fail("multiple members found")
    }
  }

  implicit class SemanticTreeOps(val tree: Tree) extends AnyVal {
    @hosted def owner: Scope = wrapHosted(_.owner(tree))
  }

  implicit class SemanticScopeOps(val tree: Scope) extends AnyVal {
    @hosted def members: Seq[Member] = wrapHosted(_.members(tree).collect{ case m: Member => m })
    @hosted def members(name: Name): Seq[Member] = wrapHosted(_.members(tree).collect{ case m: Member => m })
    @hosted private[semantic] def allMembers[T: ClassTag]: Seq[T] = {
      members.map(_.collect { case x: T => x })
    }
    @hosted private[semantic] def uniqueMember[T: ClassTag](s_name: String): T = {
      val isTerm = classOf[Member.Term].isAssignableFrom(classTag[T].runtimeClass)
      val name = if (isTerm) Term.Name(s_name, isBackquoted = false) else Type.Name(s_name, isBackquoted = false)
      members(name).map(_.collect { case x: T => x }).flatMap(_.findUnique)
    }
  }

  implicit class SemanticTopLevelScopeOps(val tree: Scope.TopLevel) extends AnyVal {
    @hosted def packages: Seq[Pkg] = tree.allMembers[Pkg]
    @hosted def packages(name: Name): Pkg = tree.uniqueMember[Pkg](name.toString)
    @hosted def packages(name: String): Pkg = tree.uniqueMember[Pkg](name.toString)
    @hosted def packages(name: scala.Symbol): Pkg = tree.uniqueMember[Pkg](name.toString)
    @hosted def pkgobject: Defn.Object = tree.allMembers[Defn.Object].map(_.filter(_.isPkgObject)).flatMap(_.findUnique)
  }

  implicit class SemanticTemplateScopeOps(val tree: Scope.Template) extends AnyVal {
    @hosted def parents: Seq[Member.Type with Has.Template] = ???
    @hosted def children: Seq[Has.Template] = ???
    @hosted def self: Aux.Self = tree match {
      case x: Aux.Template => succeed(x.self)
      case x: Has.Template => succeed(x.templ.self)
      case x: Type => ??? // TODO: compute this by intersecting x and x.defn.self
    }
    @hosted def ctor: Ctor.Primary = ctors.flatMap(_.collect { case prim: Ctor.Primary => prim }.findUnique)
    @hosted def ctors: Seq[Ctor] = wrapHosted(_.members(tree).collect{ case c: Ctor => c })
  }

  implicit class SemanticBlockScopeOps(val tree: Scope.Block) extends AnyVal {
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

  implicit class SemanticRefineScopeOps(val tree: Scope.Refine) extends AnyVal {
    @hosted def defs: Seq[Member.Def] = tree.allMembers[Member.Def]
    @hosted def defs(name: Name): Member.Def = tree.uniqueMember[Member.Def](name.toString)
    @hosted def defs(name: String): Member.Def = tree.uniqueMember[Member.Def](name.toString)
    @hosted def defs(name: scala.Symbol): Member.Def = tree.uniqueMember[Member.Def](name.toString)
    @hosted def macros: Seq[Defn.Macro] = tree.allMembers[Defn.Macro]
    @hosted def macros(name: Name): Defn.Macro = tree.uniqueMember[Defn.Macro](name.toString)
    @hosted def macros(name: String): Defn.Macro = tree.uniqueMember[Defn.Macro](name.toString)
    @hosted def macros(name: scala.Symbol): Defn.Macro = tree.uniqueMember[Defn.Macro](name.toString)
  }

  implicit class SemanticExistentialScopeOps(val tree: Scope.Existential) extends AnyVal {
    @hosted def vals: Seq[Term.Name] = tree.allMembers[Term.Name]
    @hosted def vals(name: Name): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def vals(name: String): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def vals(name: scala.Symbol): Term.Name = tree.uniqueMember[Term.Name](name.toString)
    @hosted def types: Seq[Member.Type] = tree.allMembers[Member.Type]
    @hosted def types(name: Name): Member.Type = tree.uniqueMember[Member.Type](name.toString)
    @hosted def types(name: String): Member.Type = tree.uniqueMember[Member.Type](name.toString)
    @hosted def types(name: scala.Symbol): Member.Type = tree.uniqueMember[Member.Type](name.toString)
  }

  implicit class SemanticParamsScopeOps(val tree: Scope.Params) extends AnyVal {
    @hosted def params: Seq[Param.Named] = tree.allMembers[Param.Named]
    @hosted def params(name: Name): Param.Named = tree.uniqueMember[Param.Named](name.toString)
    @hosted def params(name: String): Param.Named = tree.uniqueMember[Param.Named](name.toString)
    @hosted def params(name: scala.Symbol): Param.Named = tree.uniqueMember[Param.Named](name.toString)
    @hosted def tparams: Seq[TypeParam.Named] = tree.allMembers[TypeParam.Named]
    @hosted def tparams(name: Name): TypeParam.Named = tree.uniqueMember[TypeParam.Named](name.toString)
    @hosted def tparams(name: String): TypeParam.Named = tree.uniqueMember[TypeParam.Named](name.toString)
    @hosted def tparams(name: scala.Symbol): TypeParam.Named = tree.uniqueMember[TypeParam.Named](name.toString)
  }
}
