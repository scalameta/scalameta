package scala.reflect
package core

import org.scalareflect.invariants._
import org.scalareflect.adt._

// (Together) TODO: tree-based symbols and types (see https://github.com/paulbutcher/implementor/blob/f1921de2b7de3d5ea8cf7f230c8e4e9f8c7f4b26/core/src/main/scala/org/scalamock/Implement.scala)
// (Together) TODO: .tpe vs .signature?
// (Together) TODO: collection-like methods (see http://clang.llvm.org/docs/LibASTMatchersReference.html)
// (Together) TODO: rewriting/transformation methods
// (Together) TODO: decide on entry point tree (compilation unit? package?; use-cases compile-time, runtime, presentation)
// (Together) TODO: add tree-specific equalities as ref_==, =:= etc
// (Denys)    TODO: add moar requires
// (Denys)    TODO: think about requiring ident values to be non-keyword
// (Denys)    TODO: parser
// (Together) TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
// (Together) TODO: history vs positions (can trivia be inferred from positions only?)
// (Denys)    TODO: unhygienic quasiquotes
// (Denys)    TODO: consider adding default values for case class fields whenever applicable
// (Eugene)   TODO: pretty printer

@root trait Tree

@branch trait Ident extends Tree {
  def value: String
  def isBackquoted: Boolean = ???
}

@branch trait Term extends Arg with Stmt.Template with Stmt.Block
object Term {
  @branch trait Ref extends Term {
    def isPath: Boolean = isStableId || this.isInstanceOf[This]
    def isQualId: Boolean = this match {
      case _: Ident             => true
      case Select(qual: Ref, _) => qual.isQualId
      case _                    => false
    }
    def isStableId: Boolean = this match {
      case _: Ident | _: SuperSelect => true
      case Select(qual: Ref, _)      => qual.isPath
      case _                         => false
    }
  }
  @leaf class This(qual: Option[core.Ident]) extends Ref
  @leaf class Ident(value: scala.Predef.String) extends core.Ident with Ref with Pat {
    require(!keywords.contains(value) || isBackquoted)
  }
  @leaf class SuperSelect(qual: Option[core.Ident], supertyp: Option[Type.Ident], selector: Term.Ident) extends Ref
  @leaf class Select(qual: Ref, selector: Term.Ident) extends Ref with Pat

  @branch trait Lit extends Term with Pat
  @leaf class Bool(value: scala.Boolean) extends Lit
  @leaf class Int(value: scala.Int) extends Lit
  @leaf class Long(value: scala.Long) extends Lit
  @leaf class Float(value: scala.Float) extends Lit
  @leaf class Double(value: scala.Double) extends Lit
  @leaf class Char(value: scala.Char) extends Lit
  @leaf class String(value: Predef.String) extends Lit
  // TODO: not all symbols are representable as literals, e.g. scala.Symbol("")
  @leaf class Symbol(value: scala.Symbol) extends Lit
  @leaf class Null() extends Lit
  @leaf class Unit() extends Lit

  @leaf class Interpolate(prefix: Ident, parts: List[Term.String] @nonEmpty, args: List[Term]) extends Term {
    // (Denys) TODO: also check that prefix is alphanumeric
    require(parts.length == args.length + 1)
  }
  @leaf class Apply(fun: Term, args: List[Arg]) extends Term
  @leaf class ApplyRight(lhs: Term, op: Ident, rhs: Term) extends Term {
    require(op.value.last == ':')
  }
  @leaf class TypeApply(fun: Term, args: List[Type] @nonEmpty) extends Term
  @leaf class Assign(lhs: Term.Ref, rhs: Term) extends Term
  @leaf class Update(expr: Term, args: List[List[Term]] @nonEmpty) extends Term
  @leaf class Return(expr: Term) extends Term
  @leaf class Throw(expr: Term) extends Term
  @leaf class Ascribe(expr: Term, typ: Type) extends Term
  @leaf class Annotate(expr: Term, annots: List[Annot] @nonEmpty) extends Term with HasAnnots
  @leaf class Tuple(elements: List[Term] @nonEmpty) extends Term
  @leaf class Block(stats: List[Stmt.Block]) extends Term
  @leaf class If(cond: Term, thenp: Term, elsep: Term) extends Term
  @leaf class Match(scrut: Term, cases: List[Aux.Case] @nonEmpty) extends Term
  @leaf class Try(expr: Term, catchp: List[Aux.Case], finallyp: Option[Term]) extends Term
  @leaf class Function(params: List[Param.Function], body: Term) extends Term {
    require(params.length == 1 || !params.exists(_.annots.contains(Annot.Implicit)))
  }
  @leaf class PartialFunction(cases: List[Aux.Case] @nonEmpty) extends Term
  @leaf class While(expr: Term, body: Term) extends Term
  @leaf class Do(body: Term, expr: Term) extends Term
  @leaf class For(enums: List[Enum] @nonEmpty, body: Term) extends Term {
    require(enums.head.isInstanceOf[Enum.Generator])
  }
  @leaf class ForYield(enums: List[Enum] @nonEmpty, body: Term) extends Term
  @leaf class New(templ: Aux.Template) extends Term
  // (Denys) TODO: might need additional validation
  @leaf class Placeholder() extends Term
  @leaf class Eta(term: Term) extends Term
}

@branch trait Type extends Tree
object Type {
  @leaf class Ident(value: String) extends core.Ident with Type {
    require(!keywords.contains(value) || isBackquoted)
  }
  @leaf class Select(qual: Term.Ref, name: Type.Ident) extends Type {
    require(qual.isPath)
  }
  @leaf class SuperSelect(qual: Option[core.Ident], supertyp: Option[Type.Ident], selector: Type.Ident) extends Type
  @leaf class Project(qual: Type, name: Type.Ident) extends Type
  @leaf class Singleton(ref: Term.Ref) extends Type {
    require(ref.isPath)
  }
  @leaf class Constant(value: Term.Lit) extends Type
  @leaf class Apply(typ: Type, targs: List[Type] @nonEmpty) extends Type
  @leaf class Compound(parents: List[Type], stmts: List[Stmt.Refine] @nonEmpty) extends Type
  @leaf class Existential(typ: Type, quants: List[Stmt.Existential] @nonEmpty) extends Type
  @leaf class Function(params: Type, res: Type) extends Type
  @leaf class Tuple(elements: List[Type] @nonEmpty) extends Type
  @leaf class Annotate(typ: Type, annots: List[Annot] @nonEmpty) extends Type with HasAnnots
  // (Denys) TODO: might need additional validation
  @leaf class Placeholder() extends Type
}

@branch trait Pat extends Tree
object Pat {
  @leaf class Wildcard() extends Pat
  @leaf class SeqWildcard() extends Pat
  @leaf class Bind(lhs: Term.Ident, rhs: Pat) extends Pat
  @leaf class Alternative(lhs: Pat, rhs: Pat) extends Pat
  @leaf class Tuple(elements: List[Pat] @nonEmpty) extends Pat
  @leaf class Extract(ref: Term.Ref, elements: List[Pat]) extends Pat {
    require(ref.isStableId)
  }
  @leaf class Interpolate(prefix: Term.Ident, parts: List[Term.String] @nonEmpty, args: List[Pat]) extends Pat {
    // (Denys) TODO: check that prefix is alphanumeric
    require(parts.length == args.length + 1)
  }
  @leaf class Ascribe(lhs: Pat, rhs: Type) extends Pat {
    require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Ident])
  }
}

@branch trait Decl extends Stmt.Template with Stmt.Refine with HasAnnots
object Decl {
  @leaf class Val(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Type) extends Decl with Stmt.Existential
  @leaf class Var(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Type) extends Decl
  @leaf class Def(annots: List[Annot], name: Term.Ident, tparams: List[TypeParam.Def],
                  paramss: List[List[Param.Def]], implicits: List[Param.Def], typ: Option[Type]) extends Decl
  @leaf class Type(annots: List[Annot], name: core.Type.Ident, tparams: List[TypeParam.Type],
                   bounds: Aux.TypeBounds) extends Decl with Stmt.Existential
}

@branch trait Defn extends Stmt.Template
object Defn {
  @leaf class Val(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Option[Type], rhs: Term) extends Defn with Stmt.Block with HasAnnots

  @leaf class Var(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Option[Type], rhs: Option[Term]) extends Defn with Stmt.Block with HasAnnots {
    require(typ.nonEmpty || rhs.nonEmpty)
  }

  @leaf class Def(annots: List[Annot], name: Term.Ident, tparams: List[TypeParam.Def],
                  paramss: List[List[Param.Def]], implicits: List[Param.Def],
                  typ: Option[Type], body: Term) extends Defn with Stmt.Block with HasAnnots

  @leaf class Macro(annots: List[Annot], name: Term.Ident, tparams: List[TypeParam.Def],
                    paramss: List[List[Param.Def]], implicits: List[Param.Def],
                    typ: Type, body: Term) extends Defn with Stmt.Block with HasAnnots

  @leaf class Type(annots: List[Annot], name: core.Type.Ident,
                   tparams: List[TypeParam.Type], body: Type) extends Defn with Stmt.Refine with Stmt.Block with HasAnnots

  @branch trait Ctor extends Defn with HasAnnots {
    def paramss: List[List[Param.Def]]
    def implicits: List[Param.Def]
  }
  object Ctor {
    @leaf class Primary(annots: List[Annot] = Nil, paramss: List[List[Param.Def]] = Nil,
                        implicits: List[Param.Def] = Nil) extends Ctor
    @leaf class Secondary(annots: List[Annot], paramss: List[List[Param.Def]],
                          implicits: List[Param.Def], primaryCtorArgss: List[List[Term]]) extends Ctor
  }

  @leaf class Class(annots: List[Annot], name: core.Type.Ident, tparams: List[TypeParam.Def],
                    ctor: Ctor.Primary, templ: Aux.Template) extends Defn with Stmt.TopLevel with Stmt.Block with HasAnnots

  @leaf class Trait(annots: List[Annot], name: core.Type.Ident, tparams: List[TypeParam.Type],
                    templ: Aux.Template) extends Defn with Stmt.TopLevel with Stmt.Block with HasAnnots {
    def isInterface: Boolean = templ.stats.forall(_.isInstanceOf[Decl])
  }

  @leaf class Object(annots: List[Annot], name: Term.Ident, templ: Aux.Template) extends Defn with Stmt.TopLevel with Stmt.Block with HasAnnots

  @leaf class Package(ref: Term.Ref, stats: List[Stmt.TopLevel]) extends Defn with Stmt.TopLevel {
    require(ref.isQualId)
  }

  @leaf class PackageObject(name: Term.Ident, templ: Aux.Template) extends Defn with Stmt.TopLevel
}

object Stmt {
  @branch trait TopLevel extends Tree
  @branch trait Template extends Block
  @branch trait Block extends Refine
  @branch trait Refine extends Existential
  @branch trait Existential extends Tree
}

@leaf class Import(clauses: List[Import.Clause] @nonEmpty) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
object Import {
  @leaf class Clause(ref: Term.Ref, sels: List[Selector] @nonEmpty) extends Tree {
    require(ref.isStableId)
  }

  @branch trait Selector extends Tree
  object Selector {
    @leaf class Wildcard() extends Selector
    @leaf class Name(name: String) extends Selector
    @leaf class Rename(from: String, to: String) extends Selector
    @leaf class Unimport(name: String) extends Selector
  }
}

@branch trait Arg extends Tree
object Arg {
  @leaf class Named(name: Term.Ident, arg: Term) extends Arg
  @leaf class Seq(arg: Term) extends Arg
}

@branch trait Enum extends Tree
object Enum {
  @leaf class Generator(pat: Pat, rhs: Term) extends Enum
  @leaf class Val(pat: Pat, rhs: Term) extends Enum
  @leaf class Guard(cond: Term) extends Enum
}

@branch trait Param extends HasAnnots
object Param {
  @leaf class Function(name: Option[Term.Ident] = None, typ: Option[Type] = None, annots: List[Annot] = Nil) extends Param
  @leaf class Def(name: Term.Ident, typ: Type, default: Option[Term] = None, annots: List[Annot] = Nil) extends Param
}

@branch trait TypeParam extends HasAnnots
object TypeParam {
  @leaf class Def(annots: List[Annot] = Nil,
                  name: Option[core.Type.Ident] = None,
                  tparams: List[TypeParam.Type] = Nil,
                  contextBounds: List[core.Type] = Nil,
                  viewBounds: List[core.Type] = Nil,
                  bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends TypeParam
  @leaf class Type(annots: List[Annot] = Nil,
                   name: Option[core.Type.Ident] = None,
                   tparams: List[TypeParam.Type] = Nil,
                   bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends TypeParam
}

@branch trait HasAnnots extends Tree {
  def annots: List[Annot]
  // (Eugene) TODO: https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
  // * write a script that fetches this google doc and converts it into a, say, CSV spec
  // * write a test that validates the spec by generating source files and parsing them
  // * write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
  private[reflect] def validateAnnots(enclosing: Tree): Boolean = ???
}

@branch trait Annot extends Tree
object Annot {
  @branch trait Transient extends Annot
  // (Together) TODO: design the attachment API

  @branch trait Source extends Annot
  @leaf class UserDefined(tpe: Type, argss: List[List[Term]]) extends Source

  @branch trait Mod extends Source
  @leaf class Private(within: String) extends Mod
  @leaf class Protected(within: String) extends Mod
  @leaf class Implicit() extends Mod
  @leaf class Final() extends Mod
  @leaf class Sealed() extends Mod
  @leaf class Override() extends Mod
  @leaf class Case() extends Mod
  @leaf class Abstract() extends Mod
  @leaf class Covariant() extends Mod
  @leaf class Contravariant() extends Mod
  @leaf class Lazy() extends Mod
  @leaf class Doc(doc: String) extends Mod
  @leaf class AbstractOverride() extends Mod

  @branch trait Param extends Source
  @leaf class ByName() extends Param
  @leaf class VarArg() extends Param
  @leaf class Val() extends Param
  @leaf class Var() extends Param
}

object Aux {
  @leaf class Case(pat: Pat, cond: Option[Term] = None, body: Term = Term.Unit()) extends Tree
  @leaf class Parent(tpe: Type, argss: List[List[Term]] = Nil) extends Tree
  @leaf class Template(early: List[Defn.Val] = Nil, parents: List[Parent] = Nil,
                       self: Self = Self.empty, stats: List[Stmt.Template] = Nil) extends Tree {
    require(parents.isEmpty || !parents.tail.exists(_.argss.nonEmpty))
  }
  @leaf class Self(name: Option[Term.Ident] = None, typ: Option[Type] = None) extends Tree
  @leaf class TypeBounds(lo: Option[Type] = None, hi: Option[Type] = None) extends Tree
}
