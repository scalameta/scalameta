import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.ast._
import org.scalameta.invariants._
import org.scalameta.annotations._
import org.scalameta.unreachable
import scala.{meta => api}
import scala.meta.internal.{ast => impl} // necessary only to define internal classes, not to define the APIs
import scala.meta.internal.hygiene._ // necessary only to define internal classes, not to define the APIs
import scala.meta.syntactic.parsers.SyntacticInfo._ // necessary only for sanity checks in trees
import scala.meta.syntactic.tokenizers.keywords // necessary only for sanity checks in trees

package scala.meta {
  @root trait Tree extends Product {
    type ThisType <: Tree
    def parent: Option[Tree]
    final override def canEqual(that: Any): Boolean = that.isInstanceOf[Tree]
    final override def equals(that: Any): Boolean = that match { case that: Tree => scala.meta.internal.hygiene.equals(this, that); case _ => false }
    final override def hashCode: Int = scala.meta.internal.hygiene.hashcode(this)
    final override def toString = scala.meta.internal.ui.show(this)
  }

  @branch trait Ref extends Tree
  @branch trait Name extends Ref
  @branch trait Stat extends Tree
  @branch trait Scope extends Tree

  @branch trait Term extends Stat with Term.Arg
  object Term {
    @branch trait Ref extends Term with api.Ref
    @branch trait Name extends api.Name with Term.Ref with Pat with Member
    @branch trait Arg extends Tree
    @branch trait Param extends Member.Term
  }

  @branch trait Type extends Tree with Type.Arg with Scope
  object Type {
    @branch trait Ref extends Type with api.Ref
    @branch trait Name extends api.Name with Type.Ref
    @branch trait Arg extends Tree
    @branch trait Param extends Member.Type
  }

  @branch trait Pat extends Tree with Pat.Arg
  object Pat {
    @branch trait Arg extends Tree
  }

  @branch trait Member extends Tree with Scope
  object Member {
    @branch trait Term extends Member
    @branch trait Type extends Member
  }

  object Ctor {
    @branch trait Ref extends Term.Ref
  }

  @branch trait Template extends Tree
  @branch trait Mod extends Tree
  @branch trait Enumerator extends Tree
  @branch trait Importee extends Tree
  @branch trait Case extends Tree with Scope
  @branch trait Source extends Tree
}

package scala.meta.internal.ast {
  @branch trait Tree extends api.Tree

  @branch trait Ref extends api.Ref with Tree
  @branch trait Name extends api.Name with Ref { def value: String; def denot: Denotation; def sigma: Sigma }
  @branch trait Stat extends api.Stat with Tree
  @branch trait Scope extends api.Scope with Tree

  @branch trait Term extends api.Term with Stat with Term.Arg
  object Term {
    @branch trait Ref extends api.Term.Ref with Term with impl.Ref
    @ast class This(qual: Option[Predef.String]) extends Term.Ref
    @ast class Super(thisp: Option[Predef.String], superp: Option[Predef.String]) extends Term.Ref
    @ast class Name(value: Predef.String @nonEmpty) extends api.Term.Name with impl.Name with Term.Ref with Pat with Member {
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
      require(params.forall(param => (param.name.nonEmpty ==> param.default.isEmpty)))
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
    @ast class Param(mods: Seq[Mod], name: Option[impl.Term.Name], decltpe: Option[Type.Arg], default: Option[Term]) extends api.Term.Param with Member.Term
  }

  @branch trait Type extends api.Type with Tree with Type.Arg with Scope
  object Type {
    @branch trait Ref extends api.Type.Ref with Type with impl.Ref
    @ast class Name(value: String @nonEmpty) extends api.Type.Name with impl.Name with Type.Ref {
      // TODO: revisit this once we have trivia in place
      // require(keywords.contains(value) ==> isBackquoted)
    }
    @ast class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref {
      require(qual.isPath || qual.isInstanceOf[Term.Super])
    }
    @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
    @ast class Singleton(ref: Term.Ref) extends Type.Ref {
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
    @ast class Placeholder(bounds: Bounds) extends Type
    @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
    @branch trait Arg extends api.Type.Arg with Tree
    object Arg {
      @ast class ByName(tpe: Type) extends Arg
      @ast class Repeated(tpe: Type) extends Arg
    }
    @ast class Param(mods: Seq[Mod],
                     name: Option[impl.Type.Name],
                     tparams: Seq[impl.Type.Param],
                     contextBounds: Seq[impl.Type],
                     viewBounds: Seq[impl.Type],
                     typeBounds: impl.Type.Bounds) extends api.Type.Param with Member.Type
  }

  @branch trait Pat extends api.Pat with Tree with Pat.Arg
  object Pat {
    @ast class Wildcard() extends Pat
    @ast class Var(name: Term.Name) extends Pat
    @ast class Bind(lhs: Pat.Var, rhs: Pat.Arg) extends Pat
    @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
    @ast class Tuple(elements: Seq[Pat] @nonEmpty) extends Pat
    @ast class Extract(ref: Term.Ref, targs: Seq[Type], elements: Seq[Pat.Arg]) extends Pat {
      require(ref.isStableId)
    }
    @ast class ExtractInfix(lhs: Pat, ref: Term.Name, rhs: Seq[Pat.Arg] @nonEmpty) extends Pat {
      require(ref.isStableId)
    }
    @ast class Interpolate(prefix: Term.Name, parts: Seq[Lit.String] @nonEmpty, args: Seq[Pat]) extends Pat {
      require(parts.length == args.length + 1)
    }
    @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
      require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Pat.Var])
    }
    @branch trait Arg extends api.Pat.Arg with Tree
    object Arg {
      @ast class SeqWildcard() extends Arg
    }
  }

  @branch trait Lit extends Term with Pat with Type
  object Lit {
    @ast class Bool(value: scala.Boolean) extends Lit
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
                   pats: Seq[Pat.Var] @nonEmpty,
                   decltpe: impl.Type) extends Decl
    @ast class Var(mods: Seq[Mod],
                   pats: Seq[Pat.Var] @nonEmpty,
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
                   rhs: Term) extends Defn
    @ast class Var(mods: Seq[Mod],
                   pats: Seq[Pat] @nonEmpty,
                   decltpe: Option[impl.Type],
                   rhs: Option[Term]) extends Defn {
      require(rhs.isEmpty ==> pats.forall(_.isInstanceOf[Pat.Var]))
      require(decltpe.nonEmpty || rhs.nonEmpty)
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
      require(templ.stats.forall(!_.isInstanceOf[Ctor]))
      require(ctor.mods.isEmpty && ctor.paramss.isEmpty)
    }
    @ast class Object(mods: Seq[Mod],
                      name: Term.Name,
                      ctor: Ctor.Primary,
                      templ: Template) extends Defn with Member.Term {
      require(templ.stats.forall(!_.isInstanceOf[Ctor]))
      require(ctor.mods.isEmpty && ctor.paramss.isEmpty)
    }
  }

  @ast class Pkg(ref: Term.Ref, stats: Seq[Stat])
       extends Member.Term with Stat {
    require(ref.isQualId)
    require(stats.forall(_.isTopLevelStat))
  }
  object Pkg {
    @ast class Object(mods: Seq[Mod], name: Term.Name, ctor: Ctor.Primary, templ: Template)
         extends Member.Term with Stat
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
      @ast class Name(value: String @nonEmpty) extends impl.Name with Ref
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
    require(early.nonEmpty ==> parents.nonEmpty)
    require(early.forall(_.isEarlyStat))
    require(stats.getOrElse(Nil).forall(_.isTemplateStat))
  }

  @branch trait Mod extends api.Mod with Tree
  object Mod {
    @ast class Annot(tree: Term) extends Mod {
      require(tree.isCtorCall)
    }
    @ast class Private extends Mod
    @ast class PrivateThis extends Mod
    @ast class PrivateWithin(name: Predef.String) extends Mod
    @ast class Protected extends Mod
    @ast class ProtectedThis extends Mod
    @ast class ProtectedWithin(name: Predef.String) extends Mod
    @ast class Implicit() extends Mod
    @ast class Final() extends Mod
    @ast class Sealed() extends Mod
    @ast class Override() extends Mod
    @ast class Case() extends Mod
    @ast class Abstract() extends Mod
    @ast class Covariant() extends Mod
    @ast class Contravariant() extends Mod
    @ast class Lazy() extends Mod
    @ast class ValParam() extends Mod
    @ast class VarParam() extends Mod
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
    @branch trait Selector extends api.Importee with Tree
    object Selector {
      @ast class Wildcard() extends Selector
      @ast class Name(value: String) extends Selector
      @ast class Rename(from: String, to: String) extends Selector
      @ast class Unimport(name: String) extends Selector
    }
  }

  @ast class Case(pat: Pat, cond: Option[Term], body: Term.Block) extends api.Case with Tree with Scope

  @ast class Source(stats: Seq[Stat]) extends api.Source with Tree {
    require(stats.forall(_.isTopLevelStat))
  }
}