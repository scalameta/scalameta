package scala.meta

import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.annotation.compileTimeOnly
import scala.collection.immutable.Seq
import scala.reflect.{ClassTag, classTag}
import scala.meta.ui.{Exception => SemanticException, _}
import scala.meta.semantic.{Context => SemanticContext}
import scala.meta.internal.{ast => impl} // necessary only to implement APIs, not to define them
import scala.reflect.runtime.{universe => ru} // necessary only for a very hacky approximation of hygiene

package object semantic {
  // ===========================
  // PART 1: ATTRIBUTES
  // ===========================

  @root trait Attr
  object Attr {
    // TODO: design the attr hierarchy of semantic facts that can be figured out about trees
    // TODO: examples: a type of a tree, a definition/definitions the tree refers to, maybe a desugaring, etc
    // TODO: see https://github.com/JetBrains/intellij-scala/blob/master/src/org/jetbrains/plugins/scala/lang/resolve/ScalaResolveResult.scala#L24
    // TODO: but keep in mind that some of the facts (denotations) are now stored in tree fields (see Hygiene.scala for more information)
    @leaf class Type(tpe: scala.meta.Type) extends Attr
    @leaf class Defns(defns: Seq[scala.meta.Member] @nonEmpty) extends Attr
  }

  implicit class SemanticTreeOps(val tree: Tree) extends AnyVal {
    @hosted private[meta] def internalAttr[T: ClassTag]: T = {
      val relevant = tree.attrs.collect{ case x: T => x }
      require(relevant.length < 2)
      relevant match {
        case Seq(tpe) => tpe
        case Seq() => throw new SemanticException(s"failed to figure out ${classTag[T].runtimeClass.getName.toLowerCase} of ${tree.show[Summary]}")
        case _ => unreachable
      }
    }
    @hosted def attrs: Seq[Attr] = implicitly[SemanticContext].attrs(tree)
    @hosted def owner: Scope = implicitly[SemanticContext].owner(tree)
  }

  sealed trait HasTpe[T <: Tree, U <: Type.Arg]
  object HasTpe {
    implicit def Term[T <: meta.Term]: HasTpe[T, meta.Type] = new HasTpe[T, meta.Type] {}
    implicit def Member[T <: meta.Member]: HasTpe[T, meta.Type] = new HasTpe[T, meta.Type] {}
    implicit def TermParam[T <: meta.Term.Param]: HasTpe[T, meta.Type.Arg] = new HasTpe[T, meta.Type.Arg] {}
    implicit def Template[T <: meta.Template]: HasTpe[T, meta.Type] = new HasTpe[T, meta.Type] {}
  }

  implicit class SemanticTypeableOps[T <: Tree, U <: Type : ClassTag](val tree: T)(implicit ev: HasTpe[T, U]) {
    @hosted def tpe: U = tree.internalAttr[Attr.Type].tpe.require[U]
  }

  sealed trait HasDefn[T <: Tree, U <: Member]
  object HasDefn {
    implicit def Ref[T <: meta.Ref]: HasDefn[T, meta.Member] = new HasDefn[T, meta.Member] {}
    implicit def TermRef[T <: meta.Term.Ref]: HasDefn[T, meta.Member.Term] = new HasDefn[T, meta.Member.Term] {}
    implicit def TypeRef[T <: meta.Type.Ref]: HasDefn[T, meta.Member] = new HasDefn[T, meta.Member] {} // Type.Ref can refer to both types (regular types) and terms (singleton types)
    implicit def Importee[T <: meta.Importee]: HasDefn[T, meta.Member] = new HasDefn[T, meta.Member] {}
  }

  implicit class SemanticDefnableOps[T <: Tree, U <: meta.Member](val tree: T)(implicit ev: HasDefn[T, U]) {
    @hosted def defns: Seq[U] = tree.internalAttr[Attr.Defns].defns.require[Seq[U]]
    @hosted def defn: U = {
      defns match {
        case Seq(single) => single
        case Seq(_, _*) => throw new SemanticException(s"multiple definitions found for ${tree.show[Summary]}")
        case Seq() => unreachable
      }
    }
  }

  sealed trait HasPrefix[T <: Tree, U <: Type]
  object HasPrefix {
    implicit def Ref[T <: meta.Ref]: HasPrefix[T, meta.Type] = new HasPrefix[T, meta.Type] {}
  }

  implicit class SemanticPrefixableOps[T <: Tree, U <: meta.Type](val tree: T)(implicit ev: HasPrefix[T, U]) {
    @hosted def prefix: Option[U] = ???
    @hosted def in(prefix: U): T = in(Some(prefix))
    @hosted def in(prefix: Option[U]): T = ???
  }

  // ===========================
  // PART 2: TYPES
  // ===========================

  implicit class SemanticTypeOps(val tree: Type) extends AnyVal {
    @hosted def <:<(other: Type): Boolean = implicitly[SemanticContext].isSubType(tree, other)
    @hosted def weak_<:<(other: Type): Boolean = ???
    @hosted def widen: Type = ???
    @hosted def dealias: Type = ???
    @hosted def companion: Type.Ref = ???
    @hosted def parents: Seq[Type] = ???
  }

  @hosted def lub(tpes: Seq[Type]): Type = implicitly[SemanticContext].lub(tpes)
  @hosted def glb(tpes: Seq[Type]): Type = implicitly[SemanticContext].glb(tpes)

  // ===========================
  // PART 3: MEMBERS
  // ===========================

  implicit class SemanticMemberOps(val tree: Member) extends AnyVal {
    @hosted def prefix: Type = ???
    @hosted def in(prefix: Type): Member = ???
    @hosted def ref: Ref = {
      tree.require[impl.Member] match {
        case tree: impl.Term.Name => tree
        case tree: impl.Decl.Def => tree.name
        case tree: impl.Decl.Type => tree.name
        case tree: impl.Defn.Def => tree.name
        case tree: impl.Defn.Macro => tree.name
        case tree: impl.Defn.Type => tree.name
        case tree: impl.Defn.Class => tree.name
        case tree: impl.Defn.Trait => tree.name
        case tree: impl.Defn.Object => tree.name
        case       impl.Pkg(name: impl.Term.Name, _) => name
        case       impl.Pkg(impl.Term.Select(_, name: impl.Term.Name), _) => name
        case tree: impl.Pkg.Object => tree.name
        case tree: impl.Term.Param if tree.parent.map(_.isInstanceOf[impl.Template]).getOrElse(false) => impl.Term.This(???)
        case tree: impl.Term.Param if tree.name.isDefined => tree.name.get
        case tree: impl.Term.Param => throw new SemanticException(s"can't reference an anonymous parameter ${tree.show[Summary]}")
        case tree: impl.Type.Param if tree.name.isDefined => tree.name.get
        case tree: impl.Type.Param => throw new SemanticException(s"can't reference an anonymous parameter ${tree.show[Summary]}")
        case tree: impl.Ctor.Primary => tree.name
        case tree: impl.Ctor.Secondary => tree.name
      }
    }
    @hosted def parents: Seq[Member] = implicitly[SemanticContext].parents(tree)
    @hosted def children: Seq[Member] = implicitly[SemanticContext].children(tree)
    @hosted def companion: Member = {
      val candidates = {
        if (tree.isClass || tree.isTrait) tree.owner.members.filter(m => m.isObject && m.ref.toString == tree.ref.toString)
        else if (tree.isObject) tree.owner.members.filter(m => (m.isClass || m.isTrait) && m.ref.toString == tree.ref.toString)
        else throw new SemanticException(s"can't have companions for ${tree.show[Summary]}")
      }
      require(candidates.length < 2)
      candidates match {
        case Seq(companion) => companion
        case Seq() => throw new SemanticException(s"no companions for ${tree.show[Summary]}")
        case _ => unreachable
      }
    }
    @hosted def mods: Seq[Mod] = {
      tree.require[impl.Member] match {
        case tree: impl.Term.Name => firstNonPatParent(tree).collect{case member: Member => member}.map(_.mods).getOrElse(Nil)
        case tree: impl.Decl.Def => tree.mods
        case tree: impl.Decl.Type => tree.mods
        case tree: impl.Defn.Def => tree.mods
        case tree: impl.Defn.Macro => tree.mods
        case tree: impl.Defn.Type => tree.mods
        case tree: impl.Defn.Class => tree.mods
        case tree: impl.Defn.Trait => tree.mods
        case tree: impl.Defn.Object => tree.mods
        case tree: impl.Pkg => Nil
        case tree: impl.Pkg.Object => tree.mods
        case tree: impl.Term.Param => tree.mods
        case tree: impl.Type.Param => tree.mods
        case tree: impl.Ctor.Primary => tree.mods
        case tree: impl.Ctor.Secondary => tree.mods
      }
    }
    @hosted def annots: Seq[Term] = tree.mods.collect{ case impl.Mod.Annot(ref) => ref }
    @hosted private def firstNonPatParent(pat: Pat): Option[Tree] = pat.parent.collect{case pat: Pat => pat}.flatMap(firstNonPatParent).orElse(pat.parent)
    @hosted def isVal: Boolean = Some(tree).collect{case name: Term.Name => name}.flatMap(firstNonPatParent).map(s => s.isInstanceOf[impl.Decl.Val] || s.isInstanceOf[impl.Defn.Val]).getOrElse(false)
    @hosted def isVar: Boolean = Some(tree).collect{case name: Term.Name => name}.flatMap(firstNonPatParent).map(s => s.isInstanceOf[impl.Decl.Var] || s.isInstanceOf[impl.Defn.Var]).getOrElse(false)
    @hosted def isDef: Boolean = tree.isInstanceOf[impl.Decl.Def] || tree.isInstanceOf[impl.Defn.Def]
    @hosted def isCtor: Boolean = tree.isInstanceOf[impl.Ctor.Primary] || tree.isInstanceOf[impl.Ctor.Secondary]
    @hosted def isPrimaryCtor: Boolean = tree.isInstanceOf[impl.Ctor.Primary]
    @hosted def isMacro: Boolean = tree.isInstanceOf[impl.Defn.Macro]
    @hosted def isAbstractType: Boolean = tree.isInstanceOf[impl.Decl.Type]
    @hosted def isAliasType: Boolean = tree.isInstanceOf[impl.Defn.Type]
    @hosted def isClass: Boolean = tree.isInstanceOf[impl.Defn.Class]
    @hosted def isTrait: Boolean = tree.isInstanceOf[impl.Defn.Trait]
    @hosted def isObject: Boolean = tree.isInstanceOf[impl.Defn.Object]
    @hosted def isPackage: Boolean = tree.isInstanceOf[impl.Pkg]
    @hosted def isPackageObject: Boolean = tree.isInstanceOf[impl.Pkg.Object]
    @hosted def isPrivate: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Private]) || tree.mods.exists(_.isInstanceOf[impl.Mod.PrivateThis]) || tree.mods.exists(_.isInstanceOf[impl.Mod.PrivateWithin])
    @hosted def isProtected: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Protected]) || tree.mods.exists(_.isInstanceOf[impl.Mod.ProtectedThis]) || tree.mods.exists(_.isInstanceOf[impl.Mod.ProtectedWithin])
    @hosted def isPublic: Boolean = !tree.isPrivate && !tree.isProtected
    @hosted def accessBoundary: Member = ???
    @hosted def isImplicit: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Implicit])
    @hosted def isFinal: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Final]) || tree.isObject
    @hosted def isSealed: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Sealed])
    @hosted def isOverride: Boolean = {
      def isSyntacticOverride = !isAbstract && tree.mods.exists(_.isInstanceOf[impl.Mod.Override])
      def isSemanticOverride = {
        def isEligible = isVal || isVar || isDef || isMacro || isAbstractType || isAliasType
        def overridesSomething = parents.nonEmpty
        isEligible && overridesSomething
      }
      isSyntacticOverride || isSemanticOverride
    }
    @hosted def isCase: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Case])
    @hosted def isAbstract: Boolean = (!isAbstractOverride && tree.mods.exists(_.isInstanceOf[impl.Mod.Abstract])) || tree.isInstanceOf[impl.Decl]
    @hosted def isCovariant: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Covariant])
    @hosted def isContravariant: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Contravariant])
    @hosted def isLazy: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Lazy])
    @hosted def isAbstractOverride: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Abstract]) && tree.mods.exists(_.isInstanceOf[impl.Mod.Override])
    @hosted def isParam: Boolean = tree.isInstanceOf[impl.Term.Param]
    @hosted def isTypeParam: Boolean = tree.isInstanceOf[impl.Type.Param]
    @hosted def isByNameParam: Boolean = ???
    @hosted def isVarargParam: Boolean = ???
    @hosted def isValParam: Boolean = ???
    @hosted def isVarParam: Boolean = ???
  }

  implicit class SemanticTermMemberOps(val tree: Member.Term) extends AnyVal {
    @hosted def in(prefix: Type): Member.Term = ???
    @hosted def ref: Term.Ref = new SemanticMemberOps(tree).ref.require[Term.Ref]
    @hosted def parents: Seq[Member.Term] = new SemanticMemberOps(tree).parents.require[Seq[Member.Term]]
    @hosted def children: Seq[Member.Term] = new SemanticMemberOps(tree).children.require[Seq[Member.Term]]
    @hosted def companion: Member.Type = new SemanticMemberOps(tree).companion.require[Member.Type]
  }

  implicit class SemanticTypeMemberOps(val tree: Member.Type) extends AnyVal {
    @hosted def in(prefix: Type): Member.Type = ???
    @hosted def ref: Type.Ref = new SemanticMemberOps(tree).ref.require[Type.Ref]
    @hosted def parents: Seq[Member.Type] = new SemanticMemberOps(tree).parents.require[Seq[Member.Type]]
    @hosted def children: Seq[Member.Type] = new SemanticMemberOps(tree).parents.require[Seq[Member.Type]]
    @hosted def companion: Member.Term = new SemanticMemberOps(tree).companion.require[Member.Term]
  }

  implicit class SemanticTermParameterOps(val tree: Term.Param) extends AnyVal {
    @hosted def default: Option[meta.Term] = tree.require[impl.Term.Param].default
  }

  implicit class SemanticTypeParameterOps(val tree: Type.Param) extends AnyVal {
    @hosted def contextBounds: Seq[meta.Type] = tree.require[impl.Type.Param].contextBounds
    @hosted def viewBounds: Seq[meta.Type] = tree.require[impl.Type.Param].viewBounds
    @hosted def lo: meta.Type = tree.require[impl.Type.Param].lo
    @hosted def hi: meta.Type = tree.require[impl.Type.Param].hi
  }

  // ===========================
  // PART 4: SCOPES
  // ===========================

  implicit class SemanticScopeOps(val tree: Scope) extends AnyVal {
    @hosted private[meta] def internalAll[T: ClassTag](filter: T => Boolean): Seq[T] = {
      val partiallyFiltered = implicitly[SemanticContext].members(tree).collect{ case x: T => x }
      partiallyFiltered.filter(filter)
    }
    @hosted private[meta] def internalSingle[T <: Member : ClassTag](name: String, filter: T => Boolean, diagnostic: String): T = {
      val filtered = internalAll[T](x => x.ref.toString == name && filter(x))
      filtered match {
        case Seq(single) => single
        case Seq(_, _*) => throw new SemanticException(s"multiple $name $diagnostic found in ${tree.show[Summary]}")
        case Seq() => throw new SemanticException(s"no $name $diagnostic found in ${tree.show[Summary]}")
      }
    }
    @hosted def members: Seq[Member] = internalAll[Member](_ => true)
    @hosted def members(name: Name): Member = internalSingle[Member](name.toString, m => (name.isInstanceOf[Term.Name] && m.isInstanceOf[Member.Term]) || (name.isInstanceOf[Type.Name] && m.isInstanceOf[Member.Type]), if (name.isInstanceOf[Term.Name]) "term members" else "type members")
    @hosted def packages: Seq[Member.Term] = internalAll[Member.Term](_.isPackage)
    @hosted def packages(name: String): Member.Term = internalSingle[Member.Term](name, _.isPackage, "packages")
    @hosted def packages(name: scala.Symbol): Member.Term = internalSingle[Member.Term](name.toString, _.isPackage, "packages")
    @hosted def ctor: Member.Term = internalAll[Member.Term](_ => true) match { case Seq(primary, _*) => primary; case _ => throw new SemanticException(s"no constructors found in ${tree.show[Summary]}") }
    @hosted def ctors: Seq[Member.Term] = internalAll[Member.Term](_ => true)
    @hosted def classes: Seq[Member.Type] = internalAll[Member.Type](_.isClass)
    @hosted def classes(name: String): Member.Type = internalSingle[Member.Type](name, _.isClass, "classes")
    @hosted def classes(name: scala.Symbol): Member.Type = internalSingle[Member.Type](name.toString, _.isClass, "classes")
    @hosted def traits: Seq[Member.Type] = internalAll[Member.Type](_.isTrait)
    @hosted def traits(name: String): Member.Type = internalSingle[Member.Type](name, _.isTrait, "traits")
    @hosted def traits(name: scala.Symbol): Member.Type = internalSingle[Member.Type](name.toString, _.isTrait, "traits")
    @hosted def objects: Seq[Member.Term] = internalAll[Member.Term](_.isObject)
    @hosted def objects(name: String): Member.Term = internalSingle[Member.Term](name, _.isObject, "objects")
    @hosted def objects(name: scala.Symbol): Member.Term = internalSingle[Member.Term](name.toString, _.isObject, "objects")
    @hosted def vars: Seq[Term.Name] = internalAll[Term.Name](_.isVar)
    @hosted def vars(name: String): Term.Name = internalSingle[Term.Name](name, _.isVar, "vars")
    @hosted def vars(name: scala.Symbol): Term.Name = internalSingle[Term.Name](name.toString, _.isVar, "vars")
    @hosted def vals: Seq[Term.Name] = internalAll[Term.Name](_.isVal)
    @hosted def vals(name: String): Term.Name = internalSingle[Term.Name](name, _.isVal, "vals")
    @hosted def vals(name: scala.Symbol): Term.Name = internalSingle[Term.Name](name.toString, _.isVal, "vals")
    @hosted def defs: Seq[Member.Term] = internalAll[Member.Term](_.isDef)
    @hosted def defs(name: String): Member.Term = internalSingle[Member.Term](name, _.isDef, "defs")
    @hosted def defs(name: scala.Symbol): Member.Term = internalSingle[Member.Term](name.toString, _.isDef, "defs")
    @hosted def types: Seq[Member.Type] = internalAll[Member.Type](m => m.isAbstractType || m.isAliasType)
    @hosted def types(name: String): Member.Type = internalSingle[Member.Type](name, m => m.isAbstractType || m.isAliasType, "types")
    @hosted def types(name: scala.Symbol): Member.Type = internalSingle[Member.Type](name.toString, m => m.isAbstractType || m.isAliasType, "types")
    @hosted def params: Seq[Term.Param] = internalAll[Term.Param](_ => true)
    @hosted def paramss: Seq[Seq[Term.Param]] = ???
    @hosted def params(name: String): Term.Param = internalSingle[Term.Param](name, _ => true, "parameters")
    @hosted def params(name: scala.Symbol): Term.Param = internalSingle[Term.Param](name.toString, _ => true, "parameters")
    @hosted def tparams: Seq[Type.Param] = internalAll[Type.Param](_ => true)
    @hosted def tparams(name: String): Type.Param = internalSingle[Type.Param](name, _ => true, "type parameters")
    @hosted def tparams(name: scala.Symbol): Type.Param = internalSingle[Type.Param](name.toString, _ => true, "type parameters")
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
