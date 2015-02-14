package scala.meta
package semantic

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
import scala.meta.internal.{hygiene => h} // necessary only to implement APIs, not to define them
import scala.reflect.runtime.{universe => ru} // necessary only for a very hacky approximation of hygiene

trait Api {
  // ===========================
  // PART 1: ATTRIBUTES
  // ===========================

  implicit class SemanticTermDesugarOps(val tree: Term) {
    @hosted def desugar: Term = implicitly[SemanticContext].desugar(tree)
  }

  implicit class SemanticTermTpeOps(val tree: Term) {
    @hosted def tpe: Type = implicitly[SemanticContext].tpe(tree)
  }

  implicit class SemanticMemberTpeOps(val tree: Member) {
    @hosted def tpe: Type.Arg = tpe.asInstanceOf[impl.Member] match {
      case tree: impl.Pat.Var.Term => tree.name.tpe
      case tree: impl.Pat.Var.Type => tree.name
      case tree: impl.Decl.Def => tree.decltpe
      case tree: impl.Decl.Type => tree.name
      case tree: impl.Defn.Def => tree.decltpe.getOrElse(tree.body.tpe)
      case tree: impl.Defn.Macro => tree.tpe
      case tree: impl.Defn.Type => tree.name
      case tree: impl.Defn.Class => tree.name
      case tree: impl.Defn.Trait => tree.name
      case tree: impl.Defn.Object => impl.Type.Singleton(tree.name)
      case       impl.Pkg(name: impl.Term.Name, _) => impl.Type.Singleton(name)
      case       impl.Pkg(impl.Term.Select(_, name: impl.Term.Name), _) => impl.Type.Singleton(name)
      case tree: impl.Pkg.Object => impl.Type.Singleton(tree.name)
      case tree: impl.Term.Param if tree.parent.map(_.isInstanceOf[impl.Template]).getOrElse(false) => ??? // TODO: don't forget to intersect with the owner type
      case tree: impl.Term.Param => tree.decltpe.getOrElse(???) // TODO: infer it from context
      case tree: impl.Type.Param => tree.name
      case tree: impl.Ctor.Primary => tree.owner.asInstanceOf[meta.Member].tpe
      case tree: impl.Ctor.Secondary => tree.owner.asInstanceOf[meta.Member].tpe
    }
  }

  implicit class SemanticRefDefnOps(val tree: Ref) {
    @hosted def defns: Seq[Member] = implicitly[SemanticContext].defns(tree)
    @hosted def defn: Member = {
      defns match {
        case Seq(single) => single
        case Seq(_, _*) => throw new SemanticException(s"multiple definitions found for ${tree.show[Summary]}")
        case Seq() => unreachable
      }
    }
  }

  implicit class SemanticTermRefDefnOps(val tree: Term.Ref) {
    @hosted def defns: Seq[Member.Term] = (tree: Ref).defns.map(_.asInstanceOf[Member.Term])
    @hosted def defn: Member.Term = (tree: Ref).defn.asInstanceOf[Member.Term]
  }

  // ===========================
  // PART 2: TYPES
  // ===========================

  implicit class SemanticTypeOps(val tree: Type) {
    @hosted def <:<(other: Type): Boolean = implicitly[SemanticContext].isSubType(tree, other)
    @hosted def weak_<:<(other: Type): Boolean = ???
    @hosted def =:=(other: Type): Boolean = (tree =:= other) && (other =:= tree)
    @hosted def widen: Type = implicitly[SemanticContext].widen(tree)
    @hosted def dealias: Type = implicitly[SemanticContext].dealias(tree)
    @hosted def companion: Type.Ref = ???
    @hosted def parents: Seq[Type] = implicitly[SemanticContext].parents(tree)
  }

  @hosted def lub(tpes: Seq[Type]): Type = implicitly[SemanticContext].lub(tpes)
  @hosted def glb(tpes: Seq[Type]): Type = implicitly[SemanticContext].glb(tpes)

  // ===========================
  // PART 3: MEMBERS
  // ===========================

  implicit class SemanticMemberOps(val tree: Member) {
    // TODO: An alternative design for typeSignatureIn that is very much worth exploring
    // consists in lazy recalculation of signatures produced by Scope.members.
    // Much like we plan to remember lexical contexts, we could also remember type parameters to be instantiated.
    // For example, `t"List[Int]".defs("head")` would give us `def head: A = ...`,
    // with A carrying information about the fact that it should be substituted for Int.
    // My only immediate concern here is what to do with `show[Code]`, but that we can figure out.
    // Even though this design looks more principled and arguably more elegant that eager recalculation,
    // I ended up not going for it, because it is much less straightforward implementation-wise,
    // and any time savings are worth very much at this stage of the project.
    @hosted def source: Member = {
      def stripPrefix(denot: h.Denotation) = denot match {
        case h.Denotation.Zero => h.Denotation.Zero
        case denot: h.Denotation.Precomputed => denot.copy(prefix = h.Prefix.Zero)
      }
      val prefixlessName = tree.name match {
        case name: impl.Name.Anonymous => name
        case name: impl.Term.Name => name.copy(denot = stripPrefix(name.denot))
        case name: impl.Type.Name => name.copy(denot = stripPrefix(name.denot))
        case name: impl.Ctor.Name => name.copy(denot = stripPrefix(name.denot))
        case name: impl.Term.This => name.copy(denot = stripPrefix(name.denot))
        case name: impl.Term.Super => unreachable
        case name: impl.Mod.PrivateThis => unreachable
        case name: impl.Mod.PrivateWithin => unreachable
        case name: impl.Mod.ProtectedThis => unreachable
        case name: impl.Mod.ProtectedWithin => unreachable
      }
      prefixlessName.defn
    }
    @hosted def owner: Scope = ???
    @hosted def name: Name = {
      tree.require[impl.Member] match {
        case tree: impl.Pat.Var.Term => tree.name
        case tree: impl.Pat.Var.Type => tree.name
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
        case tree: impl.Term.Param => tree.name
        case tree: impl.Type.Param => tree.name
        case tree: impl.Ctor.Primary => tree.name
        case tree: impl.Ctor.Secondary => tree.name
      }
    }
    @hosted def isAnonymous: Boolean = {
      tree.require[impl.Member] match {
        case tree: impl.Term.Param => tree.name.isAnonymous
        case tree: impl.Type.Param => tree.name.isAnonymous
        case _ => false
      }
    }
    @hosted def parents: Seq[Member] = implicitly[SemanticContext].parents(tree)
    @hosted def children: Seq[Member] = implicitly[SemanticContext].children(tree)
    @hosted def companion: Member = {
      val candidates = {
        if (tree.isClass || tree.isTrait) tree.owner.members.filter(m => m.isObject && m.name.toString == tree.name.toString)
        else if (tree.isObject) tree.owner.members.filter(m => (m.isClass || m.isTrait) && m.name.toString == tree.name.toString)
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
        case tree: impl.Pat.Var.Term => firstNonPatParent(tree).collect{case member: Member => member}.map(_.mods).getOrElse(Nil)
        case tree: impl.Pat.Var.Type => Nil
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
    @hosted private def firstNonPatParent(pat: Pat): Option[Tree] = {
      pat.parent.collect{case pat: Pat => pat}.flatMap(firstNonPatParent).orElse(pat.parent)
    }
    @hosted def isVal: Boolean = {
      val patVarTerm = Some(tree).collect{case tree: impl.Pat.Var.Term => tree}
      val relevantParent = patVarTerm.flatMap(firstNonPatParent)
      relevantParent.map(s => s.isInstanceOf[impl.Decl.Val] || s.isInstanceOf[impl.Defn.Val]).getOrElse(false)
    }
    @hosted def isVar: Boolean = {
      val patVarTerm = Some(tree).collect{case tree: impl.Pat.Var.Term => tree}
      val relevantParent = patVarTerm.flatMap(firstNonPatParent)
      relevantParent.map(s => s.isInstanceOf[impl.Decl.Var] || s.isInstanceOf[impl.Defn.Var]).getOrElse(false)
    }
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
    @hosted def isPrivate: Boolean = (
      tree.mods.exists(_.isInstanceOf[impl.Mod.Private]) ||
      tree.mods.exists(_.isInstanceOf[impl.Mod.PrivateThis]) ||
      tree.mods.exists(_.isInstanceOf[impl.Mod.PrivateWithin])
    )
    @hosted def isProtected: Boolean = (
      tree.mods.exists(_.isInstanceOf[impl.Mod.Protected]) ||
      tree.mods.exists(_.isInstanceOf[impl.Mod.ProtectedThis]) ||
      tree.mods.exists(_.isInstanceOf[impl.Mod.ProtectedWithin])
    )
    @hosted def isPublic: Boolean = !tree.isPrivate && !tree.isProtected
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
    @hosted def isAbstract: Boolean = {
      val isAbstractClass = !isAbstractOverride && tree.mods.exists(_.isInstanceOf[impl.Mod.Abstract])
      val isAbstractMember = tree.isInstanceOf[impl.Decl]
      isAbstractClass || isAbstractMember
    }
    @hosted def isCovariant: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Covariant])
    @hosted def isContravariant: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Contravariant])
    @hosted def isLazy: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.Lazy])
    @hosted def isAbstractOverride: Boolean = (
      tree.mods.exists(_.isInstanceOf[impl.Mod.Abstract]) &&
      tree.mods.exists(_.isInstanceOf[impl.Mod.Override])
    )
    @hosted def isTermBind: Boolean = !tree.isVal && !tree.isVar && tree.isInstanceOf[impl.Pat.Var.Term]
    @hosted def isTypeBind: Boolean = tree.isInstanceOf[impl.Pat.Var.Type]
    @hosted def isTermParam: Boolean = tree.isInstanceOf[impl.Term.Param]
    @hosted def isTypeParam: Boolean = tree.isInstanceOf[impl.Type.Param]
    @hosted def isByNameParam: Boolean = tree match { case impl.Term.Param(_, _, Some(impl.Type.Arg.ByName(_)), _) => true; case _ => false }
    @hosted def isVarargParam: Boolean = tree match { case impl.Term.Param(_, _, Some(impl.Type.Arg.Repeated(_)), _) => true; case _ => false }
    @hosted def isValParam: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.ValParam])
    @hosted def isVarParam: Boolean = tree.mods.exists(_.isInstanceOf[impl.Mod.VarParam])
  }

  implicit class SemanticTermMemberOps(val tree: Member.Term) {
    @hosted def name: Name with Term.Ref = new SemanticMemberOps(tree).name.require[Name with Term.Ref]
    @hosted def parents: Seq[Member.Term] = new SemanticMemberOps(tree).parents.require[Seq[Member.Term]]
    @hosted def children: Seq[Member.Term] = new SemanticMemberOps(tree).children.require[Seq[Member.Term]]
    @hosted def companion: Member.Type = new SemanticMemberOps(tree).companion.require[Member.Type]
  }

  implicit class SemanticTypeMemberOps(val tree: Member.Type) {
    @hosted def name: Type.Name = new SemanticMemberOps(tree).name.require[Type.Name]
    @hosted def parents: Seq[Member.Type] = new SemanticMemberOps(tree).parents.require[Seq[Member.Type]]
    @hosted def children: Seq[Member.Type] = new SemanticMemberOps(tree).parents.require[Seq[Member.Type]]
    @hosted def companion: Member.Term = new SemanticMemberOps(tree).companion.require[Member.Term]
  }

  implicit class SemanticTermParameterOps(val tree: Term.Param) {
    @hosted def default: Option[meta.Term] = tree.require[impl.Term.Param].default
  }

  implicit class SemanticTypeParameterOps(val tree: Type.Param) {
    @hosted def contextBounds: Seq[meta.Type] = tree.require[impl.Type.Param].contextBounds
    @hosted def viewBounds: Seq[meta.Type] = tree.require[impl.Type.Param].viewBounds
    @hosted def lo: meta.Type = tree.require[impl.Type.Param].lo
    @hosted def hi: meta.Type = tree.require[impl.Type.Param].hi
  }

  // ===========================
  // PART 4: SCOPES
  // ===========================

  implicit class SemanticScopeOps(val tree: Scope) {
    @hosted private[meta] def deriveEvidences(tparam: Type.Param): Seq[Term.Param] = {
      def deriveEvidence(evidenceTpe: Type): Term.Param = {
        // TODO: it's almost a decent parameter except for the facts that:
        // 1) the tree doesn't have a parent or a denotation, and we'd have remember that it comes from tparam
        //    otherwise, tree.owner is going crash (luckily, i can't imagine this tree participating in other semantic calls)
        // 2) the name is anonymous, but it's not actually a correct way of modelling it,
        //    because the user can refer to that name via implicit search
        //    so far we strip off all desugarings and, hence, all inferred implicit arguments, so that's not a problem for us, but it will be
        // NOTE: potential solution would involve having the symbol of the parameter to be of a special, new kind
        // Symbol.Synthetic(origin: Symbol, generator: ???)
        impl.Term.Param(List(impl.Mod.Implicit()), impl.Name.Anonymous(), Some(evidenceTpe.asInstanceOf[impl.Type]), None)
      }
      def deriveViewEvidence(tpe: Type) = deriveEvidence(impl.Type.Function(List(tparam.name.asInstanceOf[impl.Type]), tpe.asInstanceOf[impl.Type]))
      def deriveContextEvidence(tpe: Type) = deriveEvidence(impl.Type.Apply(tpe.asInstanceOf[impl.Type], List(tparam.name.asInstanceOf[impl.Type])))
      tparam.viewBounds.map(deriveViewEvidence) ++ tparam.contextBounds.map(deriveContextEvidence)
    }
    @hosted private[meta] def mergeEvidences(paramss: Seq[Seq[Term.Param]], evidences: Seq[Term.Param]): Seq[Seq[Term.Param]] = {
      paramss match {
        case init :+ last if last.exists(_.isImplicit) => init :+ (last ++ evidences)
        case init :+ last => init :+ last :+ evidences
        case Nil => List(evidences)
      }
    }
    @hosted private[meta] def internalAll: Seq[Member] = {
      def membersOfStats(stats: Seq[impl.Tree]) = stats.collect{
        case member: Member => member
      }
      def membersOfEnumerator(enum: impl.Enumerator) = enum match {
        case impl.Enumerator.Generator(pat, _) => membersOfPat(pat)
        case impl.Enumerator.Val(pat, _) => membersOfPat(pat)
        case impl.Enumerator.Guard(_) => Nil
      }
      def membersOfPatType(ptpe: impl.Pat.Type): Seq[impl.Member] = ptpe match {
        case impl.Pat.Type.Wildcard() => Nil
        case ptpe @ impl.Pat.Var.Type(_) => List(ptpe)
        case impl.Type.Name(_) => Nil
        case impl.Type.Select(_, _) => Nil
        case impl.Pat.Type.Project(ptpe, _) => membersOfPatType(ptpe)
        case impl.Type.Singleton(_) => Nil
        case impl.Pat.Type.Apply(tpe, args) => membersOfPatType(tpe) ++ args.flatMap(membersOfPatType)
        case impl.Pat.Type.ApplyInfix(lhs, _, rhs) => membersOfPatType(lhs) ++ membersOfPatType(rhs)
        case impl.Pat.Type.Function(params, res) => params.flatMap(membersOfPatType) ++ membersOfPatType(res)
        case impl.Pat.Type.Tuple(elements) => elements.flatMap(membersOfPatType)
        case impl.Pat.Type.Compound(tpes, _) => tpes.flatMap(membersOfPatType)
        case impl.Pat.Type.Existential(tpe, _) => membersOfPatType(tpe)
        case impl.Pat.Type.Annotate(tpe, _) => membersOfPatType(tpe)
        case impl.Type.Placeholder(_) => Nil
        case _: impl.Lit => Nil
      }
      def membersOfPat(pat: impl.Pat.Arg): Seq[impl.Member] = pat match {
        case impl.Pat.Wildcard() => Nil
        case pat @ impl.Pat.Var.Term(name) => List(pat)
        case impl.Pat.Bind(lhs, rhs) => membersOfPat(lhs) ++ membersOfPat(rhs)
        case impl.Pat.Alternative(lhs, rhs) => membersOfPat(lhs) ++ membersOfPat(rhs)
        case impl.Pat.Tuple(elements) => elements.flatMap(membersOfPat)
        case impl.Pat.Extract(_, _, elements) => elements.flatMap(membersOfPat)
        case impl.Pat.ExtractInfix(lhs, _, rhs) => membersOfPat(lhs) ++ rhs.flatMap(membersOfPat)
        case impl.Pat.Interpolate(_, _, args) => args.flatMap(membersOfPat)
        case impl.Pat.Typed(lhs, ptpe) => membersOfPat(lhs) ++ membersOfPatType(ptpe)
        case impl.Pat.Arg.SeqWildcard() => Nil
        case impl.Term.Name(_) => Nil
        case impl.Term.Select(_, _) => Nil
        case _: impl.Lit => Nil
      }
      tree.require[impl.Scope] match {
        case tree: impl.Term.Block => membersOfStats(tree.stats)
        case tree: impl.Term.Function => tree.params
        case tree: impl.Term.For => tree.enums.flatMap(membersOfEnumerator)
        case tree: impl.Term.ForYield => tree.enums.flatMap(membersOfEnumerator)
        case tree: impl.Term.Param => Nil
        case tree: impl.Type => implicitly[SemanticContext].members(tree)
        case tree: impl.Type.Param => tree.tparams
        case tree: impl.Pat.Var.Term => Nil
        case tree: impl.Pat.Var.Type => Nil
        case tree: impl.Decl.Def => tree.tparams ++ mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences)).flatten
        case tree: impl.Decl.Type => tree.tparams
        case tree: impl.Defn.Def => tree.tparams ++ mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences)).flatten
        case tree: impl.Defn.Macro => tree.tparams ++ mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences)).flatten
        case tree: impl.Defn.Type => tree.tparams
        case tree: impl.Defn.Class => tree.tparams ++ tree.tpe.asInstanceOf[impl.Type].members
        case tree: impl.Defn.Trait => tree.tparams ++ tree.tpe.asInstanceOf[impl.Type].members
        case tree: impl.Defn.Object => tree.tparams ++ tree.tpe.asInstanceOf[impl.Type].members
        case tree: impl.Pkg => tree.tpe.asInstanceOf[impl.Type].members
        case tree: impl.Pkg.Object => tree.tparams ++ tree.tpe.asInstanceOf[impl.Type].members
        case tree: impl.Ctor.Primary => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences)).flatten
        case tree: impl.Ctor.Secondary => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences)).flatten
        case tree: impl.Case => membersOfPat(tree.pat)
      }
    }
    @hosted private[meta] def internalFilter[T: ClassTag](filter: T => Boolean): Seq[T] = {
      internalAll.collect{ case x: T => x }.filter(filter)
    }
    @hosted private[meta] def internalSingle[T <: Member : ClassTag](name: String, filter: T => Boolean, diagnostic: String): T = {
      val filtered = internalFilter[T](x => x.name.toString == name && filter(x))
      filtered match {
        case Seq() => throw new SemanticException(s"""no $diagnostic named "$name" found in ${tree.show[Summary]}""")
        case Seq(single) => single
        case Seq(_, _*) => throw new SemanticException(s"""multiple $diagnostic named "$name" found in ${tree.show[Summary]}""")
      }
    }
    @hosted private[meta] def internalMulti[T <: Member : ClassTag](name: String, filter: T => Boolean, diagnostic: String): Seq[T] = {
      val filtered = internalFilter[T](x => x.name.toString == name && filter(x))
      filtered match {
        case Seq() => throw new SemanticException(s"""no $diagnostic named "$name" found in ${tree.show[Summary]}""")
        case Seq(single) => List(single)
        case Seq(multi @ _*) => multi.toList
      }
    }
    @hosted def members: Seq[Member] = internalFilter[Member](_ => true)
    @hosted def members(name: Name): Member = {
      val filter = (m: Member) => (name.isInstanceOf[Term.Name] && m.isInstanceOf[Member.Term]) || (name.isInstanceOf[Type.Name] && m.isInstanceOf[Member.Type])
      val description = if (name.isInstanceOf[Term.Name]) "term members" else "type members"
      internalSingle[Member](name.toString, filter, description)
    }
    @hosted def members[T <: Member : ClassTag](member: T): T = {
      member.name match {
        case _: impl.Term.This =>
          ???
        case _: impl.Term.Super =>
          ???
        case thisName: impl.Name =>
          internalFilter[T](that => {
            def thisDenot = thisName.denot.require[h.Denotation.Precomputed]
            def thatDenot = that.name.require[impl.Name].denot.require[h.Denotation.Precomputed]
            scala.util.Try(thisDenot.symbol == thatDenot.symbol).getOrElse(false)
          }) match {
            case Seq() => throw new SemanticException(s"no prototype for $member found in ${tree.show[Summary]}")
            case Seq(single) => single
            case _ => unreachable
          }
      }
    }
    @hosted def packages: Seq[Member.Term] = internalFilter[Member.Term](_.isPackage)
    @hosted def packages(name: String): Member.Term = internalSingle[Member.Term](name, _.isPackage, "packages")
    @hosted def packages(name: scala.Symbol): Member.Term = packages(name.toString)
    @hosted def ctor: Member.Term = internalFilter[Member.Term](_ => true) match { case Seq(primary, _*) => primary; case _ => throw new SemanticException(s"no constructors found in ${tree.show[Summary]}") }
    @hosted def ctors: Seq[Member.Term] = internalFilter[Member.Term](_ => true)
    @hosted def classes: Seq[Member.Type] = internalFilter[Member.Type](_.isClass)
    @hosted def classes(name: String): Member.Type = internalSingle[Member.Type](name, _.isClass, "classes")
    @hosted def classes(name: scala.Symbol): Member.Type = classes(name.toString)
    @hosted def traits: Seq[Member.Type] = internalFilter[Member.Type](_.isTrait)
    @hosted def traits(name: String): Member.Type = internalSingle[Member.Type](name, _.isTrait, "traits")
    @hosted def traits(name: scala.Symbol): Member.Type = traits(name.toString)
    @hosted def objects: Seq[Member.Term] = internalFilter[Member.Term](_.isObject)
    @hosted def objects(name: String): Member.Term = internalSingle[Member.Term](name, _.isObject, "objects")
    @hosted def objects(name: scala.Symbol): Member.Term = objects(name.toString)
    @hosted def vars: Seq[Member.Term] = internalFilter[impl.Pat.Var.Term](_.isVar)
    @hosted def vars(name: String): Member.Term = internalSingle[impl.Pat.Var.Term](name, _.isVar, "vars")
    @hosted def vars(name: scala.Symbol):Member.Term = vars(name.toString)
    @hosted def vals: Seq[Member.Term] = internalFilter[impl.Pat.Var.Term](_.isVal)
    @hosted def vals(name: String): Member.Term = internalSingle[impl.Pat.Var.Term](name, _.isVal, "vals")
    @hosted def vals(name: scala.Symbol): Member.Term = vals(name.toString)
    @hosted def defs: Seq[Member.Term] = internalFilter[Member.Term](_.isDef)
    @hosted def defs(name: String): Member.Term = internalSingle[Member.Term](name, _.isDef, "defs")
    @hosted def defs(name: scala.Symbol): Member.Term = defs(name.toString)
    @hosted def overloads(name: String): Seq[Member.Term] = internalMulti[Member.Term](name, _.isDef, "defs")
    @hosted def overloads(name: scala.Symbol): Seq[Member.Term] = overloads(name.toString)
    @hosted def types: Seq[Member.Type] = internalFilter[Member.Type](m => m.isAbstractType || m.isAliasType)
    @hosted def types(name: String): Member.Type = internalSingle[Member.Type](name, m => m.isAbstractType || m.isAliasType, "types")
    @hosted def types(name: scala.Symbol): Member.Type = types(name.toString)
    @hosted def params: Seq[Term.Param] = internalFilter[Term.Param](_ => true)
    @hosted def paramss: Seq[Seq[Term.Param]] = tree match {
      case tree: impl.Decl.Def => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences))
      case tree: impl.Defn.Def => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences))
      case tree: impl.Defn.Macro => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences))
      case tree: impl.Ctor.Primary => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences))
      case tree: impl.Ctor.Secondary => mergeEvidences(tree.paramss, tree.tparams.flatMap(deriveEvidences))
      case _ => Nil
    }
    @hosted def params(name: String): Term.Param = internalSingle[Term.Param](name, _ => true, "parameters")
    @hosted def params(name: scala.Symbol): Term.Param = params(name.toString)
    @hosted def tparams: Seq[Type.Param] = internalFilter[Type.Param](_ => true)
    @hosted def tparams(name: String): Type.Param = internalSingle[Type.Param](name, _ => true, "type parameters")
    @hosted def tparams(name: scala.Symbol): Type.Param = tparams(name.toString)
  }

  // ===========================
  // PART 5: BINDINGS
  // ===========================

  implicit class SemanticNameOps(val tree: Name) {
    def isBinder: Boolean = tree.parent.map(_.isInstanceOf[impl.Member]).getOrElse(false)
    def isReference: Boolean = !isBinder
    def isAnonymous: Boolean = tree.isInstanceOf[impl.Name.Anonymous]
  }
}