package scala.reflect
package core

import org.scalareflect.invariants._
import org.scalareflect.adt._
import org.scalareflect.errors._
import org.scalareflect.annotations._

/*
list of ugliness discovered so far
1. function parameters can not be annotated
2. try expr catch expr syntax
3. package objects don't have annotations
4. lambdas can not be disambiguated from self type in template
5. patterns in vals are parsed inconsistently
7. awkward meaning of infix patterns: `a infix (b, c)` means `infix(a, b, c)`
8. pq"_: F[_]" is something completely different from pq"_: F[_ >: lo <: hi]"
9. pq"a[b]" is a legal pattern
10. guess what pq"a -1" means
11. no way to fully qualify things that are in empty package
12. vars with default values may not contain patterns
13. constr block
14. q"def x { case 1 => 2 }"
15. q"trait F[T] <: Sobaka[T] with Balabaka { def bazinga } "
*/

// TODO: collection-like methods (see http://clang.llvm.org/docs/LibASTMatchersReference.html)
// TODO: rewriting/transformation methods
// TODO: parser
// TODO: unhygienic quasiquotes
// TODO: consider adding default values for case class fields whenever applicable
// TODO: prettyprinter
// TODO: implement srewrite with palladium
// TODO: implement scaladoc with palladium
// TODO: add moar requires
// TODO: invariants: tree should either have at least one non-trival token or be eq to it's empty value
// TODO: converter: double check conversion of `(f _)(x)` (bug 46)

// Some notes on Tree APIs:
// 1) Tree.parent is supposed to automatically track references to tree parents without the user doing anything.
// If a tree has just been created, then its parent is Pkg.Root.
// If a tree has been inserted into another tree, then its parent is updated accordingly (in a copy-on-write fashion, no mutability).
// If a tree comes from host, then it's the host's responsibility to set the parent according to the tree's place in the program's AST.
// With the only exception: If a tree is a macro application, then its parent is Root. That's done to prevent non-local expansions.
// 2) Tree.attrs is supposed to provide the users with semantic info about the given tree respecting the parent chain
// Thus, if a tree has been inserted into a bigger tree, its attrs should take into account lexical context of the bigger tree.
// In order to achieve that, host is free to either typecheck the entire tree every time or to typecheck it once and cache the results.
// For the time being, while we don't have hygiene and while our only supported flavor or host contexts is MacroContext,
// the lexical context of newly created trees is assumed to be the one of the macro expansion site.
@root trait Tree {
  // TODO: we also need some sort of host-specific metadata in trees
  implicit def src: SourceContext
  def owner: Scope = parent match { case owner: Scope => owner; case tree => tree.owner }
  def parent: Tree = ??? // TODO: We still need to figure out how to implement this - either going the Roslyn route or the by-name arguments route.
  @hosted def attrs: List[Attribute] = delegate
  @hosted protected def internalTpe: Type = attrs.flatMap(_.collect{ case tpe: Attribute.Type => tpe } match {
    case Attribute.Type(tpe) :: Nil => succeed(tpe)
    case _ => fail(ReflectionException("typecheck has failed"))
  })
  // TODO: hygienic equality
}

@branch trait Ref extends Tree {
  private[core] def toTypeRef: Type.Ref = ??? // TODO: t"$this"
}

@branch trait Ident extends Ref {
  def value: String
  def isBackquoted: Boolean
}

@branch trait Term extends Arg with Stmt.Template with Stmt.Block {
  @hosted def tpe: Type = internalTpe
}
object Term {
  @branch trait Ref extends Term with core.Ref {
    @hosted def defn: Overload[Member.Term] = delegate
  }
  @ast class This(qual: Option[core.Ident]) extends Ref
  @ast class Ident(value: scala.Predef.String @nonEmpty, isBackquoted: Boolean = false) extends core.Ident with Ref with Pat with Member with Member.Val with Member.Var {
    // TODO: require(!keywords.contains(value) || isBackquoted)
    // TODO: if not backquoted, then not all values should be allowed
    def ident: Ident = this
    def mods: List[Mod] = Nil
    @hosted override def tpe: Type = internalTpe
  }
  @ast class SuperSelect(qual: Option[core.Ident], supertpe: Option[Type.Ident], selector: Term.Ident) extends Ref
  @ast class Select(qual: Term, selector: Term.Ident) extends Ref with Pat {
    @hosted override def tpe: Type = internalTpe
  }

  @ast class Interpolate(prefix: Ident, parts: List[Lit.String] @nonEmpty, args: List[Term]) extends Term {
    // TODO: require(prefix.isInterpolationId)
    require(parts.length == args.length + 1)
  }
  @ast class Apply(fun: Term, args: List[Arg]) extends Term
  @ast class ApplyType(fun: Term, args: List[Type] @nonEmpty) extends Term
  @ast class ApplyRight(lhs: Term, op: Ident, targs: List[Type], rhs: Term) extends Term {
    // TODO: require(op.isRightAssocOp)
  }
  @ast class ApplyUnary(op: Ident, arg: Term) extends Term {
    // TODO: require(op.isUnaryOp)
  }
  @ast class Assign(lhs: Term.Ref, rhs: Term) extends Term
  @ast class Update(lhs: Apply, rhs: Term) extends Term
  @ast class Return(expr: Term) extends Term
  @ast class Throw(expr: Term) extends Term
  @ast class Ascribe(expr: Term, decltpe: Type) extends Term
  @ast class Annotate(expr: Term, annots: List[Mod.Annot] @nonEmpty) extends Term with Has.Mods {
    def mods: List[Mod] = annots
  }
  @ast class Tuple(elements: List[Term] @nonEmpty) extends Term
  @ast class Block(stats: List[Stmt.Block]) extends Term with Scope {
    require(stats.collect{ case v: Defn.Var => v }.forall(_.rhs.isDefined))
  }
  @ast class If(cond: Term, thenp: Term, elsep: Option[Term]) extends Term
  @ast class Match(scrut: Term, cases: Cases) extends Term
  @ast class Try(expr: Term, catchp: Option[Term], finallyp: Option[Term]) extends Term
  @ast class Function(params: List[Aux.Param], body: Term) extends Term with Scope.Params {
    require(params.forall(_.default.isEmpty))
    require(params.exists(_.mods.contains(Mod.Implicit)) ==> (params.length == 1))
  }
  @ast class Cases(cases: List[Aux.Case]) extends Term {
    def isPartialFunction = !parent.isInstanceOf[Match]
  }
  @ast class While(expr: Term, body: Term) extends Term
  @ast class Do(body: Term, expr: Term) extends Term
  @ast class For(enums: List[Enum] @nonEmpty, body: Term) extends Term with Scope {
    require(enums.head.isInstanceOf[Enum.Generator])
  }
  @ast class ForYield(enums: List[Enum] @nonEmpty, body: Term) extends Term
  @ast class New(templ: Aux.Template) extends Term
  // (Denys) TODO: might need additional validation
  @ast class Placeholder() extends Term
  @ast class Eta(term: Term) extends Term
}

@branch trait Type extends Tree with Scope.Template {
  @hosted def <:<(other: Type): Boolean = delegate
  @hosted def weak_<:<(other: Type): Boolean = delegate
  @hosted def widen: Type = delegate
  @hosted def dealias: Type = delegate
  @hosted def erasure: Type = delegate
  @hosted def companion: Type.Ref = this match {
    case ref: Type.Ref => ref.defn.flatMap(_.companion).map(_.ref.toTypeRef)
    case _ => fail(ReflectionException(s"companion not found"))
  }
  // TODO: directSuperclasses and others
  @hosted override def superclasses: List[Member.Template] = supertypes.flatMap(tpes => supertypesToMembers(tpes))
  @hosted override def supertypes: List[Type] = delegate
  @hosted override def self: Aux.Self = delegate
  @hosted def subclasses: List[Member.Template] = delegate
  // TODO: simple type validation
}
object Type {
  @branch trait Ref extends Type with core.Ref {
    @hosted def defn: Member = delegate
  }
  @ast class Ident(value: String @nonEmpty, isBackquoted: Boolean = false) extends core.Ident with Ref {
    // TODO: require(keywords.contains(value) ==> isBackquoted)
  }
  @ast class Select(qual: Term.Ref, ident: Type.Ident) extends Ref {
    // TODO: require(qual.isPath)
  }
  @ast class SuperSelect(qual: Option[core.Ident], supertpe: Option[Type.Ident], selector: Type.Ident) extends Ref
  @ast class Project(qual: Type, ident: Type.Ident) extends Ref
  @ast class Singleton(ref: Term.Ref) extends Ref {
    // TODO: require(ref.isPath)
  }
  @ast class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
  @ast class Function(params: List[Type], res: Type) extends Type
  @ast class Tuple(elements: List[Type] @nonEmpty) extends Type
  @ast class Compound(tpes: List[Type], refinement: List[Stmt.Refine]) extends Type with Scope.Refine {
    // TODO: require(tpes.length == 1 ==> hasExplicitRefinement)
  }
  @ast class Existential(tpe: Type, quants: List[Stmt.Existential] @nonEmpty) extends Type with Scope.Existential
  @ast class Annotate(tpe: Type, annots: List[Mod.Annot] @nonEmpty) extends Type with Has.Mods {
    def mods: List[Mod] = annots
  }
  // (Denys) TODO: need to validate that placeholder appears within one of allowed contexts (e.g. `type T = _` is illegal)
  @ast class Placeholder(bounds: Aux.TypeBounds) extends Type
}

@branch trait Pat extends Tree {
  @hosted def tpe: Type = internalTpe
}
object Pat {
  @ast class Wildcard() extends Pat
  @ast class SeqWildcard() extends Pat
  @ast class Bind(lhs: Term.Ident, rhs: Pat) extends Pat
  @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
  @ast class Tuple(elements: List[Pat] @nonEmpty) extends Pat
  @ast class Extract(ref: Term.Ref, targs: List[Type], elements: List[Pat]) extends Pat {
    // TODO: require(ref.isStableId)
  }
  @ast class Interpolate(prefix: Term.Ident, parts: List[Lit.String] @nonEmpty, args: List[Pat]) extends Pat {
    // TODO: require(prefix.isInterpolationId)
    require(parts.length == args.length + 1)
  }
  @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
    require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Ident])
  }
}

@branch trait Member extends Tree with Has.Mods {
  def name: String = ??? // TODO: discuss how we want to see this method
  def ref: Ref = ??? // TODO: discuss the implementation of this method
  @hosted def overrides: List[Member]
  @hosted def companion: Member
  def annots: List[Mod.Annot] = mods.collect{ case annot: Mod.Annot => annot }
  def doc: Option[Mod.Doc] = mods.collect{ case doc: Mod.Doc => doc }.headOption
  def isVal: Boolean = this.isInstanceOf[Member.Val]
  def isVar: Boolean = this.isInstanceOf[Member.Var]
  def isDef: Boolean = this.isInstanceOf[Member.Def]
  def isType: Boolean = this.isInstanceOf[Member.AbstractOrAliasType]
  def isClass: Boolean = this.isInstanceOf[Defn.Class]
  def isTrait: Boolean = this.isInstanceOf[Defn.Trait]
  def isObject: Boolean = this.isInstanceOf[Defn.Object]
  def isPkg: Boolean = this.isInstanceOf[Pkg]
  def isPkgObject: Boolean = this.isInstanceOf[Pkg.Object]
  def isJava: Boolean = ??? // TODO: need special trees for Java artifacts
  def isPrivate: Boolean = mods.exists(_.isInstanceOf[Mod.Private])
  def isProtected: Boolean = mods.exists(_.isInstanceOf[Mod.Protected])
  def isPublic: Boolean = !isPrivate && !isProtected
  def isImplicit: Boolean = mods.exists(_.isInstanceOf[Mod.Implicit])
  def isFinal: Boolean = mods.exists(_.isInstanceOf[Mod.Final])
  def isSealed: Boolean = mods.exists(_.isInstanceOf[Mod.Sealed])
  @hosted def isOverride: Boolean = overrides.map(_.nonEmpty)
  def isCase: Boolean = mods.exists(_.isInstanceOf[Mod.Case])
  def isAbstract: Boolean = mods.exists(_.isInstanceOf[Mod.Abstract]) || this.isInstanceOf[Decl]
  def isCovariant: Boolean = mods.exists(_.isInstanceOf[Mod.Covariant])
  def isContravariant: Boolean = mods.exists(_.isInstanceOf[Mod.Contravariant])
  def isLazy: Boolean = mods.exists(_.isInstanceOf[Mod.Lazy])
  def isAbstractOverride: Boolean = mods.exists(_.isInstanceOf[Mod.AbstractOverride])
  def isMacro: Boolean = mods.exists(_.isInstanceOf[Mod.Macro])
  def isByNameParam: Boolean = mods.exists(_.isInstanceOf[Mod.ByNameParam])
  def isVarargParam: Boolean = mods.exists(_.isInstanceOf[Mod.VarargParam])
  def isValParam: Boolean = mods.exists(_.isInstanceOf[Mod.ValParam])
  def isVarParam: Boolean = mods.exists(_.isInstanceOf[Mod.VarParam])
}
object Member {
  @branch trait Term extends Member {
    @hosted def overrides: List[Member.Term] = delegate
    @hosted def companion: Member.Type = fail(ReflectionException(s"companion not found"))
  }
  @branch trait Type extends Member {
    @hosted def overrides: List[Member.Type] = delegate
    @hosted def companion: Member.Term = fail(ReflectionException(s"companion not found"))
  }
  @branch trait Field extends Term {
    def ident: core.Term.Ident
    @hosted def tpe: core.Type
  }
  @branch trait Val extends Field
  @branch trait Var extends Field
  @branch trait Def extends Term with Stmt.Template with Has.Paramss with Scope.Params {
    def ident: core.Term.Ident
    def tparams: List[Aux.TypeParam]
    @hosted def ret: core.Type
  }
  @branch trait AbstractOrAliasType extends Type {
    def ident: core.Type.Ident
    def tparams: List[Aux.TypeParam]
  }
  @branch trait Template extends Member with Stmt.TopLevel with Stmt.Block with Has.Paramss with Scope.Template {
    def ident: core.Ident
    def explicits: List[List[Aux.Param.Named]] = Nil
    def implicits: List[Aux.Param.Named] = Nil
    def tparams: List[Aux.TypeParam] = Nil
    def templ: Aux.Template
    @hosted def superclasses: List[Member.Template] = ref.toTypeRef.superclasses
    @hosted def supertypes: List[core.Type] = ref.toTypeRef.supertypes
    @hosted def subclasses: List[Member.Template] = ref.toTypeRef.subclasses
    @hosted def self: Aux.Self = templ.self
    @hosted def companion: CompanionType = {
      val companionId = if (ident.isInstanceOf[core.Term.Ident]) core.Type.Ident(ident.value) else core.Term.Ident(ident.value)
      val candidates = owner.members(ident)
      candidates.flatMap(candidates => {
        val relevant = findCompanion(candidates.alts)
        relevant.map(result => succeed(result)).getOrElse(fail(ReflectionException(s"companion not found")))
      })
    }
    protected type CompanionType <: Member
    protected def findCompanion(alts: List[Member]): Option[CompanionType]
  }
}
case class Overload[+A <: Member](alts: List[A]) {
  def resolve(tpes: core.Type*): A = ??? // TODO: implement this in terms of Tree.attrs and Attribute.Ref
}

@branch trait Decl extends Stmt.Template with Stmt.Refine
object Decl {
  @ast class Val(mods: List[Mod],
                 pats: List[Term.Ident] @nonEmpty,
                 decltpe: core.Type) extends Decl with Stmt.Existential with Has.Mods {
    @hosted def tpe: core.Type = succeed(decltpe)
  }
  @ast class Var(mods: List[Mod],
                 pats: List[Term.Ident] @nonEmpty,
                 decltpe: core.Type) extends Decl with Has.Mods {
    @hosted def tpe: core.Type = succeed(decltpe)
  }
  @ast class Def(mods: List[Mod],
                 ident: Term.Ident,
                 tparams: List[Aux.TypeParam],
                 explicits: List[List[Aux.Param.Named]],
                 implicits: List[Aux.Param.Named],
                 declret: core.Type) extends Decl with Member.Def {
    require(!isMacro)
    @hosted def ret: core.Type = succeed(declret)
  }
  @ast class Procedure(mods: List[Mod],
                       ident: Term.Ident,
                       tparams: List[Aux.TypeParam],
                       explicits: List[List[Aux.Param.Named]],
                       implicits: List[Aux.Param.Named]) extends Decl with Member.Def {
    @hosted def ret: core.Type = ??? // TODO: t"Unit"
  }
  @ast class Type(mods: List[Mod],
                  ident: core.Type.Ident,
                  tparams: List[Aux.TypeParam],
                  bounds: Aux.TypeBounds) extends Decl with Stmt.Existential with Member.AbstractOrAliasType
}

@branch trait Defn extends Stmt.Block with Stmt.Template
object Defn {
  @ast class Val(mods: List[Mod],
                 pats: List[Pat] @nonEmpty,
                 decltpe: Option[core.Type],
                 rhs: Term) extends Defn with Has.Mods {
    @hosted def tpe: core.Type = rhs.tpe
  }
  @ast class Var(mods: List[Mod],
                 pats: List[Pat] @nonEmpty,
                 decltpe: Option[core.Type],
                 rhs: Option[Term]) extends Defn with Has.Mods {
    require(rhs.nonEmpty || pats.forall(_.isInstanceOf[Term.Ident]))
    require(decltpe.nonEmpty || rhs.nonEmpty)
    @hosted def tpe: core.Type = rhs.map(_.tpe).getOrElse(succeed(decltpe.get))
  }
  @ast class Def(mods: List[Mod],
                 ident: Term.Ident,
                 tparams: List[Aux.TypeParam],
                 explicits: List[List[Aux.Param.Named]],
                 implicits: List[Aux.Param.Named],
                 declret: Option[core.Type],
                 body: Term) extends Defn with Member.Def {
    require(isMacro ==> declret.nonEmpty)
    @hosted def ret: core.Type = body.tpe
  }
  @ast class Procedure(mods: List[Mod],
                       ident: Term.Ident,
                       tparams: List[Aux.TypeParam],
                       explicits: List[List[Aux.Param.Named]],
                       implicits: List[Aux.Param.Named],
                       body: Term.Block) extends Defn with Member.Def {
    @hosted def ret: core.Type = ??? // TODO: t"Unit"
  }
  @ast class Type(mods: List[Mod],
                  ident: core.Type.Ident,
                  tparams: List[Aux.TypeParam],
                  body: core.Type) extends Defn with Stmt.Refine with Member.AbstractOrAliasType
  @ast class Class(mods: List[Mod],
                   ident: core.Type.Ident,
                   override val tparams: List[Aux.TypeParam],
                   declctor: Ctor.Primary,
                   templ: Aux.Template) extends Defn with Member.Template with Member.Type {
    @hosted override def ctor: Ctor.Primary = succeed(declctor)
    protected type CompanionType = Object
    protected def findCompanion(alts: List[Member]) = alts.collect{ case x: Object => x }.headOption
    @hosted override def companion: CompanionType = super[Template].companion
  }
  @ast class Trait(mods: List[Mod],
                   ident: core.Type.Ident,
                   override val tparams: List[Aux.TypeParam],
                   templ: Aux.Template) extends Defn with Member.Template with Member.Type {
    require(templ.parents.forall(_.argss.isEmpty))
    def isInterface: Boolean = templ.stats.forall(_.isInstanceOf[Decl])
    protected type CompanionType = Object
    protected def findCompanion(alts: List[Member]) = alts.collect{ case x: Object => x }.headOption
    @hosted override def companion: CompanionType = super[Template].companion
  }
  @ast class Object(mods: List[Mod],
                    ident: Term.Ident,
                    templ: Aux.Template) extends Defn with Member.Template with Member.Term {
    protected type CompanionType = Member.Template with Member.Type
    protected def findCompanion(alts: List[Member]) = alts.collect{ case x: Class => x; case x: Trait => x }.headOption
    @hosted override def companion: CompanionType = super[Template].companion
  }
}

@branch trait Pkg extends Tree {
  def stats: Seq[Stmt.TopLevel]
}
object Pkg {
  @ast class Root(stats: Seq[Stmt.TopLevel]) extends Pkg with Scope.TopLevel
  @ast class Empty(stats: Seq[Stmt.TopLevel]) extends Pkg with Scope.TopLevel
  @ast class Named(override val ref: Term.Ref,
                   stats: Seq[Stmt.TopLevel]) extends Pkg with Stmt.TopLevel with Scope.TopLevel with Member.Term {
    // TODO: require(ref.isQualId)
    def mods: List[Mod] = Nil
  }
  @ast class Object(mods: List[Mod],
                    ident: Term.Ident,
                    templ: Aux.Template) extends Tree with Stmt.TopLevel with Member.Template with Member.Term {
    protected type CompanionType = Member.Template with Member.Type
    protected def findCompanion(alts: List[Member]) = None
    @hosted override def companion: CompanionType = super[Template].companion
  }
}

// TODO: how do we ref a constructor?
@branch trait Ctor extends Tree with Has.Mods with Has.Paramss
object Ctor {
  @ast class Primary(mods: List[Mod] = Nil,
                     explicits: List[List[Aux.Param.Named]] = Nil,
                     implicits: List[Aux.Param.Named] = Nil) extends Ctor
  @ast class Secondary(mods: List[Mod] = Nil,
                       explicits: List[List[Aux.Param.Named]] @nonEmpty = List(Nil),
                       implicits: List[Aux.Param.Named] = Nil,
                       primaryCtorArgss: List[List[Arg]] = Nil,
                       stats: List[Stmt.Block] = Nil) extends Ctor with Stmt.Template with Scope.Params
}

object Stmt {
  @branch trait TopLevel extends Tree
  @branch trait Template extends Block
  @branch trait Block extends Refine
  @branch trait Refine extends Existential
  @branch trait Existential extends Tree
}

@branch trait Scope extends Tree {
  @hosted def members: Seq[Member] = delegate
  @hosted def members(name: Ident): Overload[Member] = members.flatMap(_.findOverloaded(name))
  protected implicit class RichIterable[T](val members: Seq[T]) {
    @hosted def findUnique: T = ???
    @hosted def findUnique(name: Ident): T = ???
    @hosted def findUnique(name: String): T = ???
    @hosted def findUnique(name: scala.Symbol): T = ???
  }
  protected implicit class RichMembers[T <: Member](val members: Seq[T]) {
    @hosted def findOverloaded(name: Ident): Overload[T] = ???
    @hosted def findOverloaded(name: String): Overload[T] = ???
    @hosted def findOverloaded(name: scala.Symbol): Overload[T] = ???
  }
}
object Scope {
  @branch trait TopLevel extends Scope {
    @hosted def packages: Seq[Pkg.Named] = members.map(_.collect { case pkg: Pkg.Named => pkg })
    @hosted def packages(name: Ident): Pkg.Named = packages.flatMap(_.findUnique(name))
    @hosted def packages(name: String): Pkg.Named = packages.flatMap(_.findUnique(name))
    @hosted def packages(name: scala.Symbol): Pkg.Named = packages.flatMap(_.findUnique(name))
    @hosted def classes: Seq[Defn.Class] = members.map(_.collect { case cls: Defn.Class => cls })
    @hosted def classes(name: Ident): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def classes(name: String): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def classes(name: scala.Symbol): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def traits: Seq[Defn.Trait] = members.map(_.collect { case trt: Defn.Trait => trt })
    @hosted def traits(name: Ident): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def traits(name: String): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def traits(name: scala.Symbol): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def objects: Seq[Defn.Object] = members.map(_.collect { case obj: Defn.Object => obj })
    @hosted def objects(name: Ident): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def objects(name: String): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def objects(name: scala.Symbol): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def pkgobject: Pkg.Object = members.flatMap(_.collect { case pkgobject: Pkg.Object => pkgobject }.findUnique)
    // TODO: consider extending Template in order to bring contents of package objects in
  }
  @branch trait Template extends Block with Params {
    // TODO: directSuperclasses and others
    @hosted def superclasses: List[Member.Template]
    @hosted def supertypes: List[Type]
    @hosted def self: Aux.Self
    @hosted def ctor: Ctor.Primary = ctors.flatMap(_.collect { case prim: Ctor.Primary => prim }.findUnique)
    @hosted def ctors: Seq[Ctor] = delegate
  }
  @branch trait Block extends Refine {
    @hosted def classes: Seq[Defn.Class] = members.map(_.collect { case cls: Defn.Class => cls })
    @hosted def classes(name: Ident): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def classes(name: String): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def classes(name: scala.Symbol): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def traits: Seq[Defn.Trait] = members.map(_.collect { case trt: Defn.Trait => trt })
    @hosted def traits(name: Ident): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def traits(name: String): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def traits(name: scala.Symbol): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def objects: Seq[Defn.Object] = members.map(_.collect { case obj: Defn.Object => obj })
    @hosted def objects(name: Ident): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def objects(name: String): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def objects(name: scala.Symbol): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def vars: Seq[Member.Var] = members.map(_.collect { case obj: Member.Var => obj })
    @hosted def vars(name: Ident): Member.Var = vars.flatMap(_.findUnique(name))
    @hosted def vars(name: String): Member.Var = vars.flatMap(_.findUnique(name))
    @hosted def vars(name: scala.Symbol): Member.Var = vars.flatMap(_.findUnique(name))
  }
  @branch trait Refine extends Existential {
    @hosted def defs: Seq[Member.Def] = members.map(_.collect { case obj: Member.Def => obj })
    @hosted def defs(name: Ident): Member.Def = defs.flatMap(_.findUnique(name))
    @hosted def defs(name: String): Member.Def = defs.flatMap(_.findUnique(name))
    @hosted def defs(name: scala.Symbol): Member.Def = defs.flatMap(_.findUnique(name))
  }
  @branch trait Existential extends Scope {
    @hosted def vals: Seq[Member.Val] = members.map(_.collect { case obj: Member.Val => obj })
    @hosted def vals(name: Ident): Member.Val = vals.flatMap(_.findUnique(name))
    @hosted def vals(name: String): Member.Val = vals.flatMap(_.findUnique(name))
    @hosted def vals(name: scala.Symbol): Member.Val = vals.flatMap(_.findUnique(name))
    @hosted def types: Seq[Member.AbstractOrAliasType] = members.map(_.collect { case obj: Member.AbstractOrAliasType => obj })
    @hosted def types(name: Ident): Member.AbstractOrAliasType = types.flatMap(_.findUnique(name))
    @hosted def types(name: String): Member.AbstractOrAliasType = types.flatMap(_.findUnique(name))
    @hosted def types(name: scala.Symbol): Member.AbstractOrAliasType = types.flatMap(_.findUnique(name))
  }
  @branch trait Params extends Scope {
    // TODO: might be nice to have `params("x")` or `tparams("T")`
    // but I've no idea how to marry this with pre-existing `paramss` and `tparams`
  }
}

@branch trait Lit extends Term with Pat {
  @hosted override def tpe: Type = super[Term].tpe
}
object Lit {
  // TODO: maybe add overloaded apply(value)
  // TODO: maybe add unapply(lit): Option[Any]
  @branch trait Bool extends Lit
  @ast class True() extends Bool
  @ast class False() extends Bool
  @ast class Int(value: scala.Int) extends Lit with Type
  @ast class Long(value: scala.Long) extends Lit with Type
  @ast class Float(value: scala.Float) extends Lit with Type
  @ast class Double(value: scala.Double) extends Lit with Type
  @ast class Char(value: scala.Char) extends Lit with Type
  @ast class String(value: Predef.String) extends Lit with Type
  // TODO: not all symbols are representable as literals, e.g. scala.Symbol("")
  @ast class Symbol(value: scala.Symbol) extends Lit with Type
  @ast class Null() extends Lit
  @ast class Unit() extends Lit
}

@ast class Import(clauses: List[Import.Clause] @nonEmpty) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
object Import {
  // TODO: validate that wildcard import can only be the last one in the list of sels
  @ast class Clause(ref: Term.Ref, sels: List[Selector] @nonEmpty) extends Tree {
    // TODO: require(ref.isStableId)
  }

  @branch trait Selector extends Tree
  object Selector {
    @ast class Wildcard() extends Selector
    // TODO: needs some kind of idents here but they can neither be term nor type
    @ast class Name(name: String) extends Selector
    @ast class Rename(from: String, to: String) extends Selector
    @ast class Unimport(name: String) extends Selector
  }
}

@branch trait Arg extends Tree
object Arg {
  @ast class Named(name: Term.Ident, arg: Term) extends Arg
  @ast class Seq(arg: Term) extends Arg
}

@branch trait Enum extends Tree
object Enum {
  @ast class Generator(pat: Pat, rhs: Term) extends Enum
  @ast class Val(pat: Pat, rhs: Term) extends Enum
  @ast class Guard(cond: Term) extends Enum
}

@branch trait Mod extends Tree
object Mod {
  @ast class Annot(tpe: Type, argss: List[List[Arg]]) extends Mod
  @ast class Doc(doc: String) extends Mod // TODO: design representation for scaladoc
  @ast class Private(within: String) extends Mod // TODO: design a name resolution API for these and imports
  @ast class Protected(within: String) extends Mod
  @ast class Implicit() extends Mod
  @ast class Final() extends Mod
  @ast class Sealed() extends Mod
  @ast class Override() extends Mod
  @ast class Case() extends Mod
  @ast class Abstract() extends Mod
  @ast class Covariant() extends Mod
  @ast class Contravariant() extends Mod
  @ast class Lazy() extends Mod
  @ast class AbstractOverride() extends Mod
  @ast class Macro() extends Mod
  @ast class ByNameParam() extends Mod
  @ast class VarargParam() extends Mod
  @ast class ValParam() extends Mod
  @ast class VarParam() extends Mod
}

@branch trait Attribute extends Tree
object Attribute {
  @ast class Ref(ref: core.Ref) extends Attribute
  @ast class Type(tpe: core.Type) extends Attribute
  @ast class InferredTargs(targs: List[core.Type]) extends Attribute
  @ast class InferredVargs(vargs: List[core.Term]) extends Attribute
  @ast class MacroExpansion(tree: core.Tree) extends Attribute
}

object Aux {
  @ast class Case(pat: Pat, cond: Option[Term] = None, body: Option[Term] = None) extends Tree with Scope
  @ast class Parent(tpe: Type, argss: List[List[Arg]] = Nil) extends Ref
  @ast class Template(early: List[Defn.Val] = Nil, parents: List[Parent] = Nil,
                      declself: Self = Self.empty, stats: Seq[Stmt.Template] = Nil) extends Tree with Scope.Template {
    require(parents.isEmpty || !parents.tail.exists(_.argss.nonEmpty))
    @hosted def tpe: Type = internalTpe
    @hosted def superclasses: List[Member.Template] = tpe.flatMap(_.superclasses)
    @hosted def supertypes: List[Type] = tpe.flatMap(_.supertypes)
    @hosted def self: Self = succeed(declself)
  }
  @ast class Self(ident: Option[Term.Ident] = None, decltpe: Option[Type] = None) extends Member.Term {
    def mods: List[Mod] = Nil
    @hosted def tpe: Type = internalTpe
  }
  @branch trait Param extends Tree with Has.Mods {
    @hosted def tpe: Type = internalTpe
    def default: Option[Term]
  }
  object Param {
    @ast class Anonymous(decltpe: Option[Type] = None,
                         mods: List[Mod] = Nil,
                         default: Option[Term] = None) extends Param
    @ast class Named(ident: Term.Ident,
                     decltpe: Type,
                     mods: List[Mod] = Nil,
                     default: Option[Term] = None) extends Param with Member.Term
  }
  @branch trait TypeParam extends Tree with Has.Mods {
    def tparams: List[Aux.TypeParam]
    def contextBounds: List[core.Type]
    def viewBounds: List[core.Type]
    def bounds: Aux.TypeBounds
  }
  object TypeParam {
    @ast class Anonymous(mods: List[Mod] = Nil,
                         tparams: List[Aux.TypeParam] = Nil,
                         contextBounds: List[core.Type] = Nil,
                         viewBounds: List[core.Type] = Nil,
                         bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends TypeParam
    @ast class Named(ident: core.Type.Ident,
                     mods: List[Mod] = Nil,
                     tparams: List[Aux.TypeParam] = Nil,
                     contextBounds: List[core.Type] = Nil,
                     viewBounds: List[core.Type] = Nil,
                     bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends TypeParam with Member.Type
  }
  @ast class TypeBounds(lo: Option[Type] = None, hi: Option[Type] = None) extends Tree
}

object Has {
  @branch trait Mods extends Tree {
    def mods: List[Mod]
    // (Eugene) TODO: https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
    // * write a script that fetches this google doc and converts it into a, say, CSV spec
    // * write a test that validates the spec by generating source files and parsing them
    // * write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
    private[core] def validateMods(): Either[Exception, Unit] = ???
  }

  @branch trait Paramss extends Tree {
    def explicits: List[List[Aux.Param.Named]]
    def implicits: List[Aux.Param.Named]
    def paramss: List[List[Aux.Param.Named]] = explicits :+ implicits
  }
}