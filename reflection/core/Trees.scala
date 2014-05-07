package scala.reflect
package core

import org.scalareflect.invariants._
import org.scalareflect.adt._
import org.scalareflect.errors._
import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq

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
// TODO: hygiene + hygienic tree equality
// TODO: what to do with references to particular overloads?
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
  @hosted def attrs: Seq[Attribute] = delegate
  @hosted protected def internalTpe: Type = attrs.flatMap(_.collect{ case tpe: Attribute.Type => tpe } match {
    case Attribute.Type(tpe) :: Nil => succeed(tpe)
    case _ => fail(ReflectionException("typecheck has failed"))
  })
}

@branch trait Ref extends Tree {
  private[core] def toTypeRef: Type.Ref = ??? // TODO: t"$this"
}

@branch trait Name extends Ref {
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
  @ast class This(qual: Option[core.Name]) extends Ref
  @ast class Name(value: scala.Predef.String @nonEmpty, isBackquoted: Boolean = false) extends core.Name with Ref with Pat with Has.TermName {
    // TODO: require(!keywords.contains(value) || isBackquoted)
    // TODO: if not backquoted, then not all values should be allowed
    def name: Name = this
    def mods: Seq[Mod] = Nil
  }
  @ast class SuperSelect(qual: Option[core.Name], supertpe: Option[Type.Name], selector: Term.Name) extends Ref
  @ast class Select(qual: Term, selector: Term.Name) extends Ref with Pat

  @ast class Interpolate(prefix: Name, parts: Seq[Lit.String] @nonEmpty, args: Seq[Term]) extends Term {
    // TODO: require(prefix.isInterpolationId)
    require(parts.length == args.length + 1)
  }
  @ast class Apply(fun: Term, args: Seq[Arg]) extends Term
  @ast class ApplyType(fun: Term, args: Seq[Type] @nonEmpty) extends Term
  @ast class ApplyRight(lhs: Term, op: Name, targs: Seq[Type], rhs: Term) extends Term {
    // TODO: require(op.isRightAssocOp)
  }
  @ast class ApplyUnary(op: Name, arg: Term) extends Term {
    // TODO: require(op.isUnaryOp)
  }
  @ast class Assign(lhs: Term.Ref, rhs: Term) extends Term
  @ast class Update(lhs: Apply, rhs: Term) extends Term
  @ast class Return(expr: Term) extends Term
  @ast class Throw(expr: Term) extends Term
  @ast class Ascribe(expr: Term, decltpe: Type) extends Term
  @ast class Annotate(expr: Term, annots: Seq[Mod.Annot] @nonEmpty) extends Term with Has.Mods {
    def mods: Seq[Mod] = annots
  }
  @ast class Tuple(elements: Seq[Term] @nonEmpty) extends Term
  @ast class Block(stats: Seq[Stmt.Block]) extends Term with Scope {
    require(stats.collect{ case v: Defn.Var => v }.forall(_.rhs.isDefined))
  }
  @ast class If(cond: Term, thenp: Term, elsep: Option[Term]) extends Term
  @ast class Match(scrut: Term, cases: Cases) extends Term
  @ast class Try(expr: Term, catchp: Option[Term], finallyp: Option[Term]) extends Term
  @ast class Function(params: Seq[Aux.Param], body: Term) extends Term with Scope.Params {
    require(params.forall(_.default.isEmpty))
    require(params.exists(_.mods.contains(Mod.Implicit)) ==> (params.length == 1))
  }
  @ast class Cases(cases: Seq[Aux.Case]) extends Term {
    def isPartialFunction = !parent.isInstanceOf[Match]
  }
  @ast class While(expr: Term, body: Term) extends Term
  @ast class Do(body: Term, expr: Term) extends Term
  @ast class For(enums: Seq[Enum] @nonEmpty, body: Term) extends Term with Scope {
    require(enums.head.isInstanceOf[Enum.Generator])
  }
  @ast class ForYield(enums: Seq[Enum] @nonEmpty, body: Term) extends Term
  @ast class New(templ: Aux.Template) extends Term
  // TODO: validate that placeholder is put into correct context
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
    case ref: Type.Ref => ref.defn.flatMap {
      case t: Member.Template => t.companion
      case _ => fail(ReflectionException(s"companion not found"))
    }.map(_.ref.toTypeRef)
    case _ => fail(ReflectionException(s"companion not found"))
  }
  // TODO: directSuperclasses and others
  @hosted override def superclasses: Seq[Member.Template] = supertypes.flatMap(tpes => supertypesToMembers(tpes))
  @hosted override def supertypes: Seq[Type] = delegate
  @hosted override def self: Aux.Self = delegate
  @hosted def subclasses: Seq[Member.Template] = delegate
  // TODO: simple type validation
}
object Type {
  @branch trait Ref extends Type with core.Ref {
    @hosted def defn: Member = delegate
  }
  @ast class Name(value: String @nonEmpty, isBackquoted: Boolean = false) extends core.Name with Ref {
    // TODO: require(keywords.contains(value) ==> isBackquoted)
  }
  @ast class Select(qual: Term.Ref, name: Type.Name) extends Ref {
    // TODO: require(qual.isPath)
  }
  @ast class SuperSelect(qual: Option[core.Name], supertpe: Option[Type.Name], selector: Type.Name) extends Ref
  @ast class Project(qual: Type, name: Type.Name) extends Ref
  @ast class Singleton(ref: Term.Ref) extends Ref {
    // TODO: require(ref.isPath)
  }
  @ast class Apply(tpe: Type, args: Seq[Type] @nonEmpty) extends Type
  @ast class Function(params: Seq[Type], res: Type) extends Type
  @ast class Tuple(elements: Seq[Type] @nonEmpty) extends Type
  @ast class Compound(tpes: Seq[Type], refinement: Seq[Stmt.Refine]) extends Type with Scope.Refine {
    // TODO: require(tpes.length == 1 ==> hasExplicitRefinement)
  }
  @ast class Existential(tpe: Type, quants: Seq[Stmt.Existential] @nonEmpty) extends Type with Scope.Existential
  @ast class Annotate(tpe: Type, annots: Seq[Mod.Annot] @nonEmpty) extends Type with Has.Mods {
    def mods: Seq[Mod] = annots
  }
  // TODO: need to validate that placeholder appears within one of allowed contexts (e.g. `type T = _` is illegal)
  @ast class Placeholder(bounds: Aux.TypeBounds) extends Type
}

@branch trait Pat extends Tree
object Pat {
  @ast class Wildcard() extends Pat
  @ast class SeqWildcard() extends Pat
  @ast class Bind(lhs: Term.Name, rhs: Pat) extends Pat
  @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
  @ast class Tuple(elements: Seq[Pat] @nonEmpty) extends Pat
  @ast class Extract(ref: Term.Ref, targs: Seq[Type], elements: Seq[Pat]) extends Pat {
    // TODO: require(ref.isStableId)
  }
  @ast class Interpolate(prefix: Term.Name, parts: Seq[Lit.String] @nonEmpty, args: Seq[Pat]) extends Pat {
    // TODO: require(prefix.isInterpolationId)
    require(parts.length == args.length + 1)
  }
  @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
    require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Name])
  }
}

@branch trait Member extends Tree with Has.Mods {
  def ref: Ref
  @hosted def overrides: Seq[Member]
  def annots: Seq[Mod.Annot] = mods.collect{ case annot: Mod.Annot => annot }
  def doc: Option[Mod.Doc] = mods.collect{ case doc: Mod.Doc => doc }.headOption
  def isVal: Boolean = this.isInstanceOf[Term.Name] && (this.parent.isInstanceOf[Decl.Val] || this.parent.isInstanceOf[Defn.Val])
  def isVar: Boolean = this.isInstanceOf[Term.Name] && (this.parent.isInstanceOf[Decl.Var] || this.parent.isInstanceOf[Defn.Var])
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
    def ref: Term.Ref
    @hosted def overrides: Seq[Member.Term] = delegate
  }
  @branch trait Type extends Member {
    def ref: Type.Ref
    @hosted def overrides: Seq[Member.Type] = delegate
  }
  @branch trait Def extends Term with Has.TermName with Stmt.Template with Has.Paramss with Scope.Params {
    def tparams: Seq[Aux.TypeParam]
    @hosted def ret: core.Type
  }
  @branch trait AbstractOrAliasType extends Type with Has.TypeName {
    def name: core.Type.Name
    def tparams: Seq[Aux.TypeParam]
  }
  @branch trait Template extends Member with Has.Name with Stmt.TopLevel with Stmt.Block with Has.Paramss with Scope.Template {
    def name: core.Name
    def explicits: Seq[Seq[Aux.Param.Named]] = Nil
    def implicits: Seq[Aux.Param.Named] = Nil
    def tparams: Seq[Aux.TypeParam] = Nil
    def templ: Aux.Template
    @hosted def superclasses: Seq[Member.Template] = ref.toTypeRef.superclasses
    @hosted def supertypes: Seq[core.Type] = ref.toTypeRef.supertypes
    @hosted def subclasses: Seq[Member.Template] = ref.toTypeRef.subclasses
    @hosted def self: Aux.Self = templ.self
    @hosted def companion: Member.Template
    @hosted protected def findCompanion[T <: Member.Template](f: PartialFunction[Member, T]): T = {
      val companionId = if (name.isInstanceOf[core.Term.Name]) core.Type.Name(name.value) else core.Term.Name(name.value)
      val candidates = owner.members(name)
      candidates.flatMap{candidates =>
        val relevant = candidates.alts.collect(f).headOption
        relevant.map(result => succeed(result)).getOrElse(fail(ReflectionException(s"companion not found")))
      }
    }
  }
}
final case class Overload[+A <: Member](alts: Seq[A]) {
  def resolve(tpes: core.Type*): A = ??? // TODO: implement this in terms of Tree.attrs and Attribute.Ref
}

@branch trait Decl extends Stmt.Template with Stmt.Refine
object Decl {
  @ast class Val(mods: Seq[Mod],
                 pats: Seq[Term.Name] @nonEmpty,
                 decltpe: core.Type) extends Decl with Stmt.Existential with Has.Mods {
    @hosted def tpe: core.Type = succeed(decltpe)
  }
  @ast class Var(mods: Seq[Mod],
                 pats: Seq[Term.Name] @nonEmpty,
                 decltpe: core.Type) extends Decl with Has.Mods {
    @hosted def tpe: core.Type = succeed(decltpe)
  }
  @ast class Def(mods: Seq[Mod],
                 name: Term.Name,
                 tparams: Seq[Aux.TypeParam],
                 explicits: Seq[Seq[Aux.Param.Named]],
                 implicits: Seq[Aux.Param.Named],
                 declret: core.Type) extends Decl with Member.Def {
    @hosted def ret: core.Type = succeed(declret)
  }
  @ast class Procedure(mods: Seq[Mod],
                       name: Term.Name,
                       tparams: Seq[Aux.TypeParam],
                       explicits: Seq[Seq[Aux.Param.Named]],
                       implicits: Seq[Aux.Param.Named]) extends Decl with Member.Def {
    @hosted def ret: core.Type = ??? // TODO: t"Unit"
  }
  @ast class Type(mods: Seq[Mod],
                  name: core.Type.Name,
                  tparams: Seq[Aux.TypeParam],
                  bounds: Aux.TypeBounds) extends Decl with Stmt.Existential with Member.AbstractOrAliasType
}

@branch trait Defn extends Stmt.Block with Stmt.Template
object Defn {
  @ast class Val(mods: Seq[Mod],
                 pats: Seq[Pat] @nonEmpty,
                 decltpe: Option[core.Type],
                 rhs: Term) extends Defn with Has.Mods {
    @hosted def tpe: core.Type = rhs.tpe
  }
  @ast class Var(mods: Seq[Mod],
                 pats: Seq[Pat] @nonEmpty,
                 decltpe: Option[core.Type],
                 rhs: Option[Term]) extends Defn with Has.Mods {
    require(rhs.nonEmpty || pats.forall(_.isInstanceOf[Term.Name]))
    require(decltpe.nonEmpty || rhs.nonEmpty)
    @hosted def tpe: core.Type = rhs.map(_.tpe).getOrElse(succeed(decltpe.get))
  }
  @ast class Def(mods: Seq[Mod],
                 name: Term.Name,
                 tparams: Seq[Aux.TypeParam],
                 explicits: Seq[Seq[Aux.Param.Named]],
                 implicits: Seq[Aux.Param.Named],
                 declret: Option[core.Type],
                 body: Term) extends Defn with Member.Def {
    require(isMacro ==> declret.nonEmpty)
    @hosted def ret: core.Type = body.tpe
  }
  @ast class Procedure(mods: Seq[Mod],
                       name: Term.Name,
                       tparams: Seq[Aux.TypeParam],
                       explicits: Seq[Seq[Aux.Param.Named]],
                       implicits: Seq[Aux.Param.Named],
                       body: Term.Block) extends Defn with Member.Def {
    @hosted def ret: core.Type = ??? // TODO: t"Unit"
  }
  @ast class Type(mods: Seq[Mod],
                  name: core.Type.Name,
                  tparams: Seq[Aux.TypeParam],
                  body: core.Type) extends Defn with Stmt.Refine with Member.AbstractOrAliasType
  @ast class Class(mods: Seq[Mod],
                   name: core.Type.Name,
                   override val tparams: Seq[Aux.TypeParam],
                   declctor: Ctor.Primary,
                   templ: Aux.Template) extends Defn with Member.Template with Member.Type with Has.TypeName {
    @hosted override def ctor: Ctor.Primary = succeed(declctor)
    @hosted override def companion: Object = findCompanion{ case x: Object => x }
  }
  @ast class Trait(mods: Seq[Mod],
                   name: core.Type.Name,
                   override val tparams: Seq[Aux.TypeParam],
                   templ: Aux.Template) extends Defn with Member.Template with Member.Type with Has.TypeName {
    require(templ.parents.forall(_.argss.isEmpty))
    def isInterface: Boolean = templ.stats.forall(_.isInstanceOf[Decl])
    @hosted override def companion: Object = findCompanion{ case x: Object => x }
  }
  @ast class Object(mods: Seq[Mod],
                    name: Term.Name,
                    templ: Aux.Template) extends Defn with Member.Template with Member.Term with Has.TermName {
    @hosted override def companion: Member.Template with Member.Type = findCompanion{ case x: Class => x; case x: Trait => x }
  }
}

@branch trait Pkg extends Tree {
  def stats: Seq[Stmt.TopLevel]
}
object Pkg {
  @ast class Root private[core] (stats: Seq[Stmt.TopLevel]) extends Pkg with Scope.TopLevel
  @ast class Empty(stats: Seq[Stmt.TopLevel]) extends Pkg with Scope.TopLevel
  @ast class Named(override val ref: Term.Ref,
                   stats: Seq[Stmt.TopLevel]) extends Pkg with Stmt.TopLevel with Scope.TopLevel with Member.Term {
    // TODO: require(ref.isQualId)
    def mods: Seq[Mod] = Nil
  }
  @ast class Object(mods: Seq[Mod],
                    name: Term.Name,
                    templ: Aux.Template) extends Tree with Stmt.TopLevel with Member.Template with Member.Term with Has.TermName {
    @hosted override def companion: Member.Template with Member.Type = fail(ReflectionException("companion not found"))
  }
}

@branch trait Ctor extends Tree with Has.Mods with Has.Paramss {
  @hosted def tpe: Type = internalTpe
}
object Ctor {
  @ast class Primary(mods: Seq[Mod] = Nil,
                     explicits: Seq[Seq[Aux.Param.Named]] = Nil,
                     implicits: Seq[Aux.Param.Named] = Nil) extends Ctor
  @ast class Secondary(mods: Seq[Mod] = Nil,
                       explicits: Seq[Seq[Aux.Param.Named]] @nonEmpty = List(Nil),
                       implicits: Seq[Aux.Param.Named] = Nil,
                       primaryCtorArgss: Seq[Seq[Arg]] = Nil,
                       stats: Seq[Stmt.Block] = Nil) extends Ctor with Stmt.Template with Scope.Params
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
  @hosted def members(name: Name): Overload[Member] = members.flatMap(_.findOverloaded(name))
  protected implicit class RichIterable[T](val members: Seq[T]) {
    @hosted def findUnique: T = ???
    @hosted def findUnique(name: Name): T = ???
    @hosted def findUnique(name: String): T = ???
    @hosted def findUnique(name: scala.Symbol): T = ???
  }
  protected implicit class RichMembers[T <: Member](val members: Seq[T]) {
    @hosted def findOverloaded(name: Name): Overload[T] = ???
    @hosted def findOverloaded(name: String): Overload[T] = ???
    @hosted def findOverloaded(name: scala.Symbol): Overload[T] = ???
  }
}
object Scope {
  @branch trait TopLevel extends Scope with Block {
    @hosted def packages: Seq[Pkg.Named] = members.map(_.collect { case pkg: Pkg.Named => pkg })
    @hosted def packages(name: Name): Pkg.Named = packages.flatMap(_.findUnique(name))
    @hosted def packages(name: String): Pkg.Named = packages.flatMap(_.findUnique(name))
    @hosted def packages(name: scala.Symbol): Pkg.Named = packages.flatMap(_.findUnique(name))
    @hosted def pkgobject: Pkg.Object = members.flatMap(_.collect { case pkgobject: Pkg.Object => pkgobject }.findUnique)
  }
  @branch trait Template extends Block with Params {
    // TODO: directSuperclasses and others
    @hosted def superclasses: Seq[Member.Template]
    @hosted def supertypes: Seq[Type]
    @hosted def self: Aux.Self
    @hosted def ctor: Ctor.Primary = ctors.flatMap(_.collect { case prim: Ctor.Primary => prim }.findUnique)
    @hosted def ctors: Seq[Ctor] = delegate
  }
  @branch trait Block extends Refine {
    @hosted def classes: Seq[Defn.Class] = members.map(_.collect { case cls: Defn.Class => cls })
    @hosted def classes(name: Name): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def classes(name: String): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def classes(name: scala.Symbol): Defn.Class = classes.flatMap(_.findUnique(name))
    @hosted def traits: Seq[Defn.Trait] = members.map(_.collect { case trt: Defn.Trait => trt })
    @hosted def traits(name: Name): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def traits(name: String): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def traits(name: scala.Symbol): Defn.Trait = traits.flatMap(_.findUnique(name))
    @hosted def objects: Seq[Defn.Object] = members.map(_.collect { case obj: Defn.Object => obj })
    @hosted def objects(name: Name): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def objects(name: String): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def objects(name: scala.Symbol): Defn.Object = objects.flatMap(_.findUnique(name))
    @hosted def vars: Seq[Term.Name] = members.map(_.collect { case obj: Term.Name => obj })
    @hosted def vars(name: Name): Term.Name = vars.flatMap(_.findUnique(name))
    @hosted def vars(name: String): Term.Name = vars.flatMap(_.findUnique(name))
    @hosted def vars(name: scala.Symbol): Term.Name = vars.flatMap(_.findUnique(name))
  }
  @branch trait Refine extends Existential {
    @hosted def defs: Seq[Member.Def] = members.map(_.collect { case obj: Member.Def => obj })
    @hosted def defs(name: Name): Member.Def = defs.flatMap(_.findUnique(name))
    @hosted def defs(name: String): Member.Def = defs.flatMap(_.findUnique(name))
    @hosted def defs(name: scala.Symbol): Member.Def = defs.flatMap(_.findUnique(name))
  }
  @branch trait Existential extends Scope {
    @hosted def vals: Seq[Term.Name] = members.map(_.collect { case obj: Term.Name => obj })
    @hosted def vals(name: Name): Term.Name = vals.flatMap(_.findUnique(name))
    @hosted def vals(name: String): Term.Name = vals.flatMap(_.findUnique(name))
    @hosted def vals(name: scala.Symbol): Term.Name = vals.flatMap(_.findUnique(name))
    @hosted def types: Seq[Member.AbstractOrAliasType] = members.map(_.collect { case obj: Member.AbstractOrAliasType => obj })
    @hosted def types(name: Name): Member.AbstractOrAliasType = types.flatMap(_.findUnique(name))
    @hosted def types(name: String): Member.AbstractOrAliasType = types.flatMap(_.findUnique(name))
    @hosted def types(name: scala.Symbol): Member.AbstractOrAliasType = types.flatMap(_.findUnique(name))
  }
  @branch trait Params extends Scope {
    @hosted def params: Seq[Aux.Param.Named] = members.map(_.collect { case obj: Aux.Param.Named => obj })
    @hosted def params(name: Name): Aux.Param.Named = params.flatMap(_.findUnique(name))
    @hosted def params(name: String): Aux.Param.Named = params.flatMap(_.findUnique(name))
    @hosted def params(name: scala.Symbol): Aux.Param.Named = params.flatMap(_.findUnique(name))
    @hosted def tparams: Seq[Aux.TypeParam.Named] = members.map(_.collect { case obj: Aux.TypeParam.Named => obj })
    @hosted def tparams(name: Name): Aux.TypeParam.Named = tparams.flatMap(_.findUnique(name))
    @hosted def tparams(name: String): Aux.TypeParam.Named = tparams.flatMap(_.findUnique(name))
    @hosted def tparams(name: scala.Symbol): Aux.TypeParam.Named = tparams.flatMap(_.findUnique(name))
  }
}

@branch trait Lit extends Term with Pat
object Lit {
  @branch trait Bool extends Lit
  @ast class True() extends Bool
  @ast class False() extends Bool
  @ast class Int(value: scala.Int) extends Lit with Type
  @ast class Long(value: scala.Long) extends Lit with Type
  @ast class Float(value: scala.Float) extends Lit with Type
  @ast class Double(value: scala.Double) extends Lit with Type
  @ast class Char(value: scala.Char) extends Lit with Type
  @ast class String(value: Predef.String) extends Lit with Type
  // TODO: validate that not all symbols are representable as literals, e.g. scala.Symbol("")
  @ast class Symbol(value: scala.Symbol) extends Lit with Type
  @ast class Null() extends Lit
  @ast class Unit() extends Lit
}

@ast class Import(clauses: Seq[Import.Clause] @nonEmpty) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
object Import {
  // TODO: validate that wildcard import can only be the last one in the list of sels
  @ast class Clause(ref: Term.Ref, sels: Seq[Selector] @nonEmpty) extends Tree {
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
  @ast class Named(name: Term.Name, arg: Term) extends Arg
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
  @ast class Annot(tpe: Type, argss: Seq[Seq[Arg]]) extends Mod
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
  @ast class Ref(ref: Tree) extends Attribute
  @ast class Type(tpe: core.Type) extends Attribute
  @ast class InferredTargs(targs: Seq[core.Type]) extends Attribute
  @ast class InferredVargs(vargs: Seq[core.Term]) extends Attribute
  @ast class MacroExpansion(tree: core.Tree) extends Attribute
}

object Aux {
  @ast class Case(pat: Pat, cond: Option[Term] = None, body: Option[Term] = None) extends Tree with Scope
  @ast class Parent(tpe: Type, argss: Seq[Seq[Arg]] = Nil) extends Ref {
    @hosted def ctor: Ctor = attrs.flatMap(_.collect{ case ref: Attribute.Ref => ref } match {
      case Attribute.Ref(ref: Ctor) :: Nil => succeed(ref)
      case _ => fail(ReflectionException("typecheck has failed"))
    })
  }
  @ast class Template(early: Seq[Defn.Val] = Nil, parents: Seq[Parent] = Nil,
                      declself: Self = Self.empty, stats: Seq[Stmt.Template] = Nil) extends Tree with Scope.Template {
    require(parents.isEmpty || !parents.tail.exists(_.argss.nonEmpty))
    @hosted def tpe: Type = internalTpe
    @hosted def superclasses: Seq[Member.Template] = tpe.flatMap(_.superclasses)
    @hosted def supertypes: Seq[Type] = tpe.flatMap(_.supertypes)
    @hosted def self: Self = succeed(declself)
  }
  @ast class Self(name: Option[Term.Name] = None, decltpe: Option[Type] = None) extends Member.Term {
    def mods: Seq[Mod] = Nil
    @hosted def tpe: Type = internalTpe
    def ref: Term.Ref = name.getOrElse(Term.This(None))
  }
  @branch trait Param extends Tree with Has.Mods {
    @hosted def tpe: Type = internalTpe
    def default: Option[Term]
  }
  object Param {
    @ast class Anonymous(decltpe: Option[Type] = None,
                         mods: Seq[Mod] = Nil,
                         default: Option[Term] = None) extends Param
    @ast class Named(name: Term.Name,
                     decltpe: Type,
                     mods: Seq[Mod] = Nil,
                     default: Option[Term] = None) extends Param with Member.Term with Has.TermName
  }
  @branch trait TypeParam extends Tree with Has.Mods {
    def tparams: Seq[Aux.TypeParam]
    def contextBounds: Seq[core.Type]
    def viewBounds: Seq[core.Type]
    def bounds: Aux.TypeBounds
  }
  object TypeParam {
    @ast class Anonymous(mods: Seq[Mod] = Nil,
                         tparams: Seq[Aux.TypeParam] = Nil,
                         contextBounds: Seq[core.Type] = Nil,
                         viewBounds: Seq[core.Type] = Nil,
                         bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends TypeParam
    @ast class Named(name: core.Type.Name,
                     mods: Seq[Mod] = Nil,
                     tparams: Seq[Aux.TypeParam] = Nil,
                     contextBounds: Seq[core.Type] = Nil,
                     viewBounds: Seq[core.Type] = Nil,
                     bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends TypeParam with Member.Type with Has.TypeName
  }
  @ast class TypeBounds(lo: Option[Type] = None, hi: Option[Type] = None) extends Tree
}

object Has {
  @branch trait Mods extends Tree {
    def mods: Seq[Mod]
    // TODO: https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
    // * write a script that fetches this google doc and converts it into a, say, CSV spec
    // * write a test that validates the spec by generating source files and parsing them
    // * write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
    private[core] def validateMods(): Unit = ???
  }

  @branch trait Paramss extends Tree {
    def explicits: Seq[Seq[Aux.Param.Named]]
    def implicits: Seq[Aux.Param.Named]
    def paramss: Seq[Seq[Aux.Param.Named]] = explicits :+ implicits
  }

  @branch trait Name extends Member {
    def name: core.Name
  }

  @branch trait TermName extends Member.Term {
    def name: Term.Name
    def ref: Term.Ref = name
  }

  @branch trait TypeName extends Member.Type {
    def name: Type.Name
    def ref: Type.Ref = name
  }
}