import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.ast._
import scala.{meta => api}

package scala.meta {
  @root trait Tree extends Product {
    type ThisType <: Tree
    def parent: Option[Tree]
    def origin: Origin
    final override def canEqual(that: Any): Boolean = that.isInstanceOf[Tree]
    final override def equals(that: Any): Boolean = that match { case that: Tree => scala.meta.internal.hygiene.equals(this, that); case _ => false }
    final override def hashCode: Int = scala.meta.internal.hygiene.hashcode(this)
    final override def toString = scala.meta.internal.ui.toString(this)
  }

  @branch trait Name extends Ref
  object Name {
    @branch trait Anonymous extends Name with Term.Param.Name with Type.Param.Name with Qualifier
    @branch trait Indeterminate extends Name with Qualifier
    @branch trait Qualifier extends Ref
  }

  @branch trait Ref extends Tree
  @branch trait Stat extends Tree
  @branch trait Scope extends Tree

  @branch trait Term extends Stat with Term.Arg
  object Term {
    @branch trait Ref extends Term with api.Ref
    @branch trait Name extends api.Name with Term.Ref with Pat with Param.Name with api.Name.Qualifier
    @branch trait Arg extends Tree
    @branch trait Param extends Member
    object Param {
      @branch trait Name extends api.Name
    }
  }

  @branch trait Type extends Tree with Type.Arg with Scope
  object Type {
    @branch trait Ref extends Type with api.Ref
    @branch trait Name extends api.Name with Type.Ref with Pat.Type.Ref with Param.Name with api.Name.Qualifier
    @branch trait Arg extends Tree
    @branch trait Param extends Member
    object Param {
      @branch trait Name extends api.Name
    }
  }

  @branch trait Pat extends Tree with Pat.Arg
  object Pat {
    @branch trait Var extends Tree
    object Var {
      @branch trait Term extends Var with Pat with Member.Term
      @branch trait Type extends Var with Pat.Type with Member.Type
    }
    @branch trait Arg extends Tree
    @branch trait Type extends Tree
    object Type {
      @branch trait Ref extends Type with api.Ref
    }
  }

  @branch trait Lit extends Term with Pat with Type with Pat.Type

  @branch trait Member extends Tree with Scope
  object Member {
    @branch trait Term extends Member
    @branch trait Type extends Member
  }

  object Ctor {
    @branch trait Ref extends Term.Ref
    @branch trait Name extends api.Name with Ref with Term
  }

  @branch trait Template extends Tree
  @branch trait Mod extends Tree
  @branch trait Enumerator extends Tree
  @branch trait Importee extends Tree with Ref
  @branch trait Case extends Tree with Scope
  @branch trait Source extends Tree with Stat
}

package scala.meta.internal.ast {
  import org.scalameta.invariants._
  import org.scalameta.annotations._
  import org.scalameta.unreachable
  import scala.meta.internal.{ast => impl}
  import scala.meta.internal.hygiene._
  import scala.meta.internal.parsers.Helpers._
  import scala.meta.internal.tokenizers.keywords

  @branch trait Tree extends api.Tree

  @branch trait Name extends api.Name with Ref { def value: String; def denot: Denotation; def sigma: Sigma }
  object Name {
    @ast class Anonymous extends api.Name.Anonymous with Name with Term.Param.Name with Type.Param.Name with Qualifier { def value = "_" }
    @ast class Indeterminate(value: Predef.String @nonEmpty) extends api.Name.Indeterminate with Name with Qualifier
    @branch trait Qualifier extends api.Name.Qualifier with Ref
  }

  @branch trait Ref extends api.Ref with Tree
  @branch trait Stat extends api.Stat with Tree
  @branch trait Scope extends api.Scope with Tree

  @branch trait Term extends api.Term with Stat with Term.Arg
  object Term {
    @branch trait Ref extends api.Term.Ref with Term with impl.Ref
    @ast class This(qual: impl.Name.Qualifier) extends Term.Ref with impl.Name.Qualifier {
      require(!qual.isInstanceOf[Term.This])
    }
    @ast class Super(thisp: impl.Name.Qualifier, superp: impl.Name.Qualifier) extends Term.Ref {
      require(!thisp.isInstanceOf[Term.This] && !superp.isInstanceOf[Term.This])
    }
    @ast class Name(value: Predef.String @nonEmpty) extends api.Term.Name with impl.Name with Term.Ref with Pat with Param.Name with impl.Name.Qualifier {
      // TODO: revisit this once we have trivia in place
      // require(keywords.contains(value) ==> isBackquoted)
    }
    @ast class Select(qual: Term, name: Term.Name) extends Term.Ref with Pat
    @ast class Interpolate(prefix: Name, parts: Seq[Lit.String] @nonEmpty, args: Seq[Term]) extends Term {
      require(parts.length == args.length + 1)
    }
    @ast class Apply(fun: Term, args: Seq[Arg]) extends Term
    @ast class ApplyType(fun: Term, targs: Seq[Type] @nonEmpty) extends Term
    @ast class ApplyInfix(lhs: Term, op: Name, targs: Seq[Type], args: Seq[Arg]) extends Term
    @ast class ApplyUnary(op: Name, arg: Term) extends Term {
      require(op.isUnaryOp)
    }
    @ast class Assign(lhs: Term.Ref, rhs: Term) extends Term
    @ast class Update(fun: Term, argss: Seq[Seq[Arg]], rhs: Term) extends Term
    @ast class Return(expr: Term) extends Term
    @ast class Throw(expr: Term) extends Term
    @ast class Ascribe(expr: Term, tpe: Type) extends Term
    @ast class Annotate(expr: Term, annots: Seq[Mod.Annot] @nonEmpty) extends Term
    @ast class Tuple(elements: Seq[Term] @nonEmpty) extends Term {
      require(elements.length > 1)
    }
    @ast class Block(stats: Seq[Stat]) extends Term with Scope {
      require(stats.forall(_.isBlockStat))
    }
    @ast class If(cond: Term, thenp: Term, elsep: Term) extends Term
    @ast class Match(scrut: Term, cases: Seq[Case] @nonEmpty) extends Term
    @ast class TryWithCases(expr: Term, catchp: Seq[Case], finallyp: Option[Term]) extends Term
    @ast class TryWithTerm(expr: Term, catchp: Term, finallyp: Option[Term]) extends Term
    @ast class Function(params: Seq[Term.Param], body: Term) extends Term with Scope {
      require(params.forall(param => (param.name.isInstanceOf[impl.Name.Anonymous] ==> param.default.isEmpty)))
      require(params.exists(_.mods.exists(_.isInstanceOf[Mod.Implicit])) ==> (params.length == 1))
    }
    @ast class PartialFunction(cases: Seq[Case] @nonEmpty) extends Term
    @ast class While(expr: Term, body: Term) extends Term
    @ast class Do(body: Term, expr: Term) extends Term
    @ast class For(enums: Seq[Enumerator] @nonEmpty, body: Term) extends Term with Scope {
      require(enums.head.isInstanceOf[Enumerator.Generator])
    }
    @ast class ForYield(enums: Seq[Enumerator] @nonEmpty, body: Term) extends Term with Scope
    @ast class New(templ: Template) extends Term
    @ast class Placeholder() extends Term
    @ast class Eta(term: Term) extends Term
    @branch trait Arg extends api.Term.Arg with Tree
    object Arg {
      @ast class Named(name: Name, rhs: Term) extends Arg
      @ast class Repeated(arg: Term) extends Arg
    }
    @ast class Param(mods: Seq[Mod], name: Param.Name, decltpe: Option[Type.Arg], default: Option[Term]) extends api.Term.Param with Member
    object Param {
      @branch trait Name extends impl.Name with api.Term.Param.Name
    }
  }

  @branch trait Type extends api.Type with Tree with Type.Arg with Scope
  object Type {
    @branch trait Ref extends api.Type.Ref with Type with impl.Ref
    @ast class Name(value: String @nonEmpty) extends api.Type.Name with impl.Name with Type.Ref with Pat.Type.Ref with Param.Name with impl.Name.Qualifier {
      // TODO: revisit this once we have trivia in place
      // require(keywords.contains(value) ==> isBackquoted)
    }
    @ast class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref with Pat.Type.Ref {
      require(qual.isPath || qual.isInstanceOf[Term.Super])
    }
    @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
    @ast class Singleton(ref: Term.Ref) extends Type.Ref with Pat.Type.Ref {
      require(ref.isPath || ref.isInstanceOf[Term.Super])
    }
    @ast class Apply(tpe: Type, args: Seq[Type] @nonEmpty) extends Type
    @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
    @ast class Function(params: Seq[Type.Arg], res: Type) extends Type
    @ast class Tuple(elements: Seq[Type] @nonEmpty) extends Type {
      require(elements.length > 1)
    }
    @ast class Compound(tpes: Seq[Type], refinement: Seq[Stat]) extends Type {
      // TODO: revisit this once we have trivia in place
      // require(tpes.length == 1 ==> hasRefinement)
      require(refinement.forall(_.isRefineStat))
    }
    @ast class Existential(tpe: Type, quants: Seq[Stat] @nonEmpty) extends Type {
      require(quants.forall(_.isExistentialStat))
    }
    @ast class Annotate(tpe: Type, annots: Seq[Mod.Annot] @nonEmpty) extends Type
    @ast class Placeholder(bounds: Bounds) extends Type with Pat.Type
    @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
    @branch trait Arg extends api.Type.Arg with Tree
    object Arg {
      @ast class ByName(tpe: Type) extends Arg
      @ast class Repeated(tpe: Type) extends Arg
    }
    @ast class Param(mods: Seq[Mod],
                     name: Param.Name,
                     tparams: Seq[impl.Type.Param],
                     typeBounds: impl.Type.Bounds,
                     viewBounds: Seq[impl.Type],
                     contextBounds: Seq[impl.Type]) extends api.Type.Param with Member
    object Param {
      @branch trait Name extends impl.Name with api.Type.Param.Name
    }
  }

  @branch trait Pat extends api.Pat with Tree with Pat.Arg
  object Pat {
    // TODO: Introduction of Pat.Var.Term and Pat.Var.Type is a very far-reaching design decision.
    //
    // Here we would like to model Scala's extravagant binding rules for term and type variables in patterns,
    // according to which both `x` and ``x`` might mean same or different things depending on whether they are used
    // a) in normal contexts, b) in patterns, c) in special parts of patterns.
    // Concretely, `X` in `X + 2` and `case X` means the same, whereas `x` in `x + 2` and `x` does not.
    // Also, `T` in `val x: T = ...` and `case x: T => ...` means the same, whereas `t` in the same conditions does not.
    //
    // The two approaches to this are as follows:
    // 1) Model names in both bindee and binder roles as the same AST node (i.e. Term.Name for terms and Type.Name for types)
    // 2) Model bindees as Term.Name/Type.Name and binders as Pat.Var.Term and Pat.Var.Type
    //
    // Benefits of the first approach:
    // + One less AST node for terms
    // + A lot less AST nodes for types (type vars are viral in type patterns, so we have to replicate almost the entire type hierarchy!)
    //
    // Benefits of the second approach:
    // + Impossible to mix up bindee/binder roles when unquoting
    // + The aforementioned safety guarantee is static
    // + Trivial to figure out whether a name is a bindee or binder
    // + Does not conflate Name and Member (in the first approach, Name has to be Member, because names in binder role are members)
    //
    // Here are the arguments that attempt to adjust the first approach to address the lack of benefits of the second approach:
    // + Term.Name and Type.Name are names, so they have denotations, and that's where we can keep track of the role.
    //   We have to make sure that we unquote binder names correctly (not $x, but ${x.name}).
    //   We also have to make sure that we unquote pattern types correctly, and that should be done in a deep fashion!
    // + On multiple occasions, we have sacrificed static safety guarantees in favor of more compact solutions.
    // + In the first approach, it's also possible to figure out the role of a given name, even though in a bit trickier fashion.
    // + This conflation might be unfortunate, but it doesn't create soundness holes.
    //
    // After a lot of deliberation, I've picked the second approach.
    // However the benefits of the first approach are definitely tangible, and we will need to revisit this choice later.
    @branch trait Var extends Tree with api.Pat.Var
    object Var {
      @ast class Term(name: impl.Term.Name) extends api.Pat.Var.Term with Var with Pat with Member.Term
      @ast class Type(name: impl.Type.Name) extends api.Pat.Var.Type with Var with Pat.Type with Member.Type
    }
    @ast class Wildcard() extends Pat
    @ast class Bind(lhs: Pat.Var.Term, rhs: Pat.Arg) extends Pat
    @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
    @ast class Tuple(elements: Seq[Pat] @nonEmpty) extends Pat
    @ast class Extract(ref: Term.Ref, targs: Seq[impl.Type], args: Seq[Pat.Arg]) extends Pat {
      require(ref.isStableId)
    }
    @ast class ExtractInfix(lhs: Pat, ref: Term.Name, rhs: Seq[Pat.Arg] @nonEmpty) extends Pat {
      require(ref.isStableId)
    }
    @ast class Interpolate(prefix: Term.Name, parts: Seq[Lit.String] @nonEmpty, args: Seq[Pat]) extends Pat {
      require(parts.length == args.length + 1)
    }
    @ast class Typed(lhs: Pat, rhs: Pat.Type) extends Pat {
      require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Pat.Var.Term] || lhs.isInstanceOf[Pat.Quasi])
    }
    @branch trait Arg extends api.Pat.Arg with Tree
    object Arg {
      @ast class SeqWildcard() extends Arg
    }
    @branch trait Type extends api.Pat.Type with Tree
    object Type {
      @branch trait Ref extends api.Pat.Type.Ref with Pat.Type with impl.Ref
      @ast class Wildcard() extends Pat.Type
      @ast class Project(qual: Pat.Type, name: impl.Type.Name) extends Pat.Type with Pat.Type.Ref
      @ast class Apply(tpe: Pat.Type, args: Seq[Pat.Type] @nonEmpty) extends Pat.Type
      @ast class ApplyInfix(lhs: Pat.Type, op: impl.Type.Name, rhs: Pat.Type) extends Pat.Type
      @ast class Function(params: Seq[Pat.Type], res: Pat.Type) extends Pat.Type
      @ast class Tuple(elements: Seq[Pat.Type] @nonEmpty) extends Pat.Type {
        require(elements.length > 1)
      }
      @ast class Compound(tpes: Seq[Pat.Type], refinement: Seq[Stat]) extends Pat.Type {
        // TODO: revisit this once we have trivia in place
        // require(tpes.length == 1 ==> hasRefinement)
        require(refinement.forall(_.isRefineStat))
      }
      @ast class Existential(tpe: Pat.Type, quants: Seq[Stat] @nonEmpty) extends Pat.Type {
        require(quants.forall(_.isExistentialStat))
      }
      @ast class Annotate(tpe: Pat.Type, annots: Seq[Mod.Annot] @nonEmpty) extends Pat.Type
    }
  }

  @branch trait Lit extends Term with Pat with Type with Pat.Type with api.Lit
  object Lit {
    @ast class Bool(value: scala.Boolean) extends Lit
    @ast class Byte(value: scala.Byte) extends Lit
    @ast class Short(value: scala.Short) extends Lit
    @ast class Int(value: scala.Int) extends Lit
    @ast class Long(value: scala.Long) extends Lit
    @ast class Float(value: scala.Float) extends Lit
    @ast class Double(value: scala.Double) extends Lit
    @ast class Char(value: scala.Char) extends Lit
    @ast class String(value: Predef.String) extends Lit
    @ast class Symbol(value: scala.Symbol) extends Lit
    @ast class Null() extends Lit
    @ast class Unit() extends Lit
  }

  @branch trait Member extends api.Member with Tree with Scope
  object Member {
    @branch trait Term extends api.Member.Term with Member
    @branch trait Type extends api.Member.Type with Member
  }

  @branch trait Decl extends Stat
  object Decl {
    @ast class Val(mods: Seq[Mod],
                   pats: Seq[Pat.Var.Term] @nonEmpty,
                   decltpe: impl.Type) extends Decl
    @ast class Var(mods: Seq[Mod],
                   pats: Seq[Pat.Var.Term] @nonEmpty,
                   decltpe: impl.Type) extends Decl
    @ast class Def(mods: Seq[Mod],
                   name: Term.Name,
                   tparams: Seq[impl.Type.Param],
                   paramss: Seq[Seq[Term.Param]],
                   decltpe: impl.Type) extends Decl with Member.Term
    @ast class Type(mods: Seq[Mod],
                    name: impl.Type.Name,
                    tparams: Seq[impl.Type.Param],
                    bounds: impl.Type.Bounds) extends Decl with Member.Type
  }

  @branch trait Defn extends Stat
  object Defn {
    @ast class Val(mods: Seq[Mod],
                   pats: Seq[Pat] @nonEmpty,
                   decltpe: Option[impl.Type],
                   rhs: Term) extends Defn {
      require(pats.forall(!_.isInstanceOf[Term.Name]))
    }
    @ast class Var(mods: Seq[Mod],
                   pats: Seq[Pat] @nonEmpty,
                   decltpe: Option[impl.Type],
                   rhs: Option[Term]) extends Defn {
      require(pats.forall(!_.isInstanceOf[Term.Name]))
      require(decltpe.nonEmpty || rhs.nonEmpty)
      require(rhs.isEmpty ==> pats.forall(_.isInstanceOf[Pat.Var.Term]))
    }
    @ast class Def(mods: Seq[Mod],
                   name: Term.Name,
                   tparams: Seq[impl.Type.Param],
                   paramss: Seq[Seq[Term.Param]],
                   decltpe: Option[impl.Type],
                   body: Term) extends Defn with Member.Term
    @ast class Macro(mods: Seq[Mod],
                     name: Term.Name,
                     tparams: Seq[impl.Type.Param],
                     paramss: Seq[Seq[Term.Param]],
                     tpe: impl.Type,
                     body: Term) extends Defn with Member.Term
    @ast class Type(mods: Seq[Mod],
                    name: impl.Type.Name,
                    tparams: Seq[impl.Type.Param],
                    body: impl.Type) extends Defn with Member.Type
    @ast class Class(mods: Seq[Mod],
                     name: impl.Type.Name,
                     tparams: Seq[impl.Type.Param],
                     ctor: Ctor.Primary,
                     templ: Template) extends Defn with Member.Type
    @ast class Trait(mods: Seq[Mod],
                     name: impl.Type.Name,
                     tparams: Seq[impl.Type.Param],
                     ctor: Ctor.Primary,
                     templ: Template) extends Defn with Member.Type {
      // TODO: hardcoded in the @ast macro, find out a better way
      // require(templ.stats.getOrElse(Nil).forall(!_.isInstanceOf[Ctor]))
      require(ctor.mods.isEmpty && ctor.paramss.isEmpty)
    }
    @ast class Object(mods: Seq[Mod],
                      name: Term.Name,
                      ctor: Ctor.Primary,
                      templ: Template) extends Defn with Member.Term {
      // TODO: hardcoded in the @ast macro, find out a better way
      // require(templ.stats.getOrElse(Nil).forall(!_.isInstanceOf[Ctor]))
      require(ctor.mods.isEmpty && ctor.paramss.isEmpty)
    }
  }

  @ast class Pkg(ref: Term.Ref, stats: Seq[Stat])
       extends Member.Term with Stat {
    require(ref.isQualId)
    // TODO: hardcoded in the @ast macro, find out a better way
    // require(stats.forall(_.isTopLevelStat))
  }
  object Pkg {
    @ast class Object(mods: Seq[Mod], name: Term.Name, ctor: Ctor.Primary, templ: Template)
         extends Member.Term with Stat {
      // TODO: hardcoded in the @ast macro, find out a better way
      // require(templ.stats.getOrElse(Nil).forall(!_.isInstanceOf[Ctor]))
    }
  }

  @branch trait Ctor extends Tree with Member.Term
  object Ctor {
    @ast class Primary(mods: Seq[Mod],
                       name: Ctor.Name,
                       paramss: Seq[Seq[Term.Param]]) extends Ctor
    @ast class Secondary(mods: Seq[Mod],
                         name: Ctor.Name,
                         paramss: Seq[Seq[Term.Param]] @nonEmpty,
                         body: Term) extends Ctor with Stat {
      require(body.isCtorBody)
    }
    @branch trait Ref extends api.Ctor.Ref with impl.Term.Ref
    val Name = Ref.Name
    type Name = Ref.Name
    object Ref {
      // TODO: current design with Ctor.Name(value) has a problem of sometimes meaningless `value`
      // for example, q"def this() = ..." is going to have Ctor.Name("this"), because we're parsing
      // this constructor outside of any enclosure, so we can't call it Ctor.Name("C") or Ctor.Name("D")
      // an alternative design might reintroduce the Ctor.Ref ast node that would have the structure of:
      // Ctor.Ref(tpe: Type, ctor: Ctor.Name), where Ctor.Name would be Ctor.Name()
      // in that design, we also won't have to convert between Type and Ctor.Ref hierarchies, which is a definite plus
      @ast class Name(value: String @nonEmpty) extends api.Ctor.Name with impl.Name with Ref
      @ast class Select(qual: Term.Ref, name: Name) extends Ref
      @ast class Project(qual: Type, name: Name) extends Ref
      @ast class Function(name: Name) extends Ref
    }
  }

  @ast class Template(early: Seq[Stat],
                      parents: Seq[Term],
                      self: Term.Param,
                      stats: Option[Seq[Stat]]) extends api.Template with Tree {
    require(parents.forall(_.isCtorCall))
    // TODO: hardcoded in the @ast macro, find out a better way
    // require(early.nonEmpty ==> parents.nonEmpty)
    // require(early.forall(_.isEarlyStat))
    // require(stats.getOrElse(Nil).forall(_.isTemplateStat))
  }

  @branch trait Mod extends api.Mod with Tree
  object Mod {
    @ast class Annot(body: Term) extends Mod {
      require(body.isCtorCall)
    }
    @ast class Private(within: Name.Qualifier) extends Mod
    @ast class Protected(within: Name.Qualifier) extends Mod
    @ast class Implicit() extends Mod
    @ast class Final() extends Mod
    @ast class Sealed() extends Mod
    @ast class Override() extends Mod
    @ast class Case() extends Mod
    @ast class Abstract() extends Mod
    @ast class Covariant() extends Mod
    @ast class Contravariant() extends Mod
    @ast class Lazy() extends Mod
    // TODO: ValParam and VarParam being mods might not be the best idea ever
    // however the alternative is to have a dedicated XXX.Param type for class/trait parameters
    // and that has proven to be very clunky (e.g. such XXX.Param type has to be a supertype for Term.Param)
    @ast class ValParam() extends Mod
    @ast class VarParam() extends Mod
    @ast class Ffi(signature: String) extends Mod
  }

  @branch trait Enumerator extends api.Enumerator with Tree
  object Enumerator {
    @ast class Generator(pat: Pat, rhs: Term) extends Enumerator
    @ast class Val(pat: Pat, rhs: Term) extends Enumerator
    @ast class Guard(cond: Term) extends Enumerator
  }

  @ast class Import(clauses: Seq[Import.Clause] @nonEmpty) extends Stat
  object Import {
    @ast class Clause(ref: Term.Ref, sels: Seq[Selector] @nonEmpty) extends Tree {
      require(ref.isStableId)
    }
    @branch trait Selector extends api.Importee with Tree with Ref
    object Selector {
      @ast class Wildcard() extends Selector
      @ast class Name(value: impl.Name.Indeterminate) extends Selector
      @ast class Rename(from: impl.Name.Indeterminate, to: impl.Name.Indeterminate) extends Selector
      @ast class Unimport(name: impl.Name.Indeterminate) extends Selector
    }
  }

  @ast class Case(pat: Pat, cond: Option[Term], body: Term.Block) extends api.Case with Tree with Scope

  @ast class Source(stats: Seq[Stat]) extends api.Source with Tree with Stat {
    require(stats.forall(_.isTopLevelStat))
  }

  // TODO: after we bootstrap, Quasi.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @branch trait Quasi extends Tree {
    def tree: Any
    def rank: Int
    def pt: Class[_]
  }
 
  // TODO: since trees are no longer sealed, we need a mechanism that would keep track of all of them 
  @registry object Registry
}