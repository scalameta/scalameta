package scala.meta

import scala.language.experimental.macros
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.ast._
import org.scalameta.invariants._
import org.scalameta.annotations._
import org.scalameta.unreachable
import semantic._
import syntactic.show._
import syntactic.parsers._, SyntacticInfo._

@ast class EmptyTree() extends Term with Type with Pat {
  override def isEmpty = true
}

@root trait Tree extends Product {
  type ThisType <: Tree
  def origin: Origin
  def parent: Tree

  def isEmpty                             = false
  def map(f: Tree => Tree): Tree          = if (isEmpty) this else f(this)
  def exists(p: Tree => Boolean): Boolean = !isEmpty && p(this)

  final override def toString: String = this.show[Raw]
}

@branch trait Term extends Arg with Stmt.Template with Stmt.Block with Qual.Term
object Term {
  @branch trait Ref extends Term with meta.Ref with Qual.Type
  @ast class This(qual: Option[Qual.Name]) extends Ref with Mod.AccessQualifier
  @ast class Name(value: scala.Predef.String @nonEmpty, @trivia isBackquoted: Boolean = false) extends meta.Name with Ref with Pat with Member with Has.TermName {
    require(keywords.contains(value) ==> isBackquoted)
    def name: Name = this
    def mods: Seq[Mod] = Nil
  }
  @ast class Select(qual: Qual.Term, selector: Term.Name, @trivia isPostfix: Boolean = false) extends Ref with Pat

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
  @ast class Update(lhs: Apply, rhs: Term) extends Term
  @ast class Return(expr: Term = Lit.Unit()) extends Term
  @ast class Throw(expr: Term) extends Term
  @ast class Ascribe(expr: Term, tpe: Type) extends Term
  @ast class Annotate(expr: Term, annots: Seq[Mod.Annot] @nonEmpty) extends Term with Has.Mods {
    def mods: Seq[Mod] = annots
  }
  @ast class Tuple(elements: Seq[Term] @nonEmpty) extends Term {
    require(elements.length > 1)
  }
  @ast class Block(stats: Seq[Stmt.Block]) extends Term with Scope {
    require(stats.collect { case v: Defn.Var => v }.forall(_.rhs.isDefined))
    require(stats.collect { case m: Member if m.isPkgObject => m }.isEmpty)
  }
  @ast class If(cond: Term, thenp: Term, elsep: Term = Lit.Unit()) extends Term
  @ast class Match(scrut: Term, cases: Cases) extends Term
  @ast class Try(expr: Term, catchp: Option[Term], finallyp: Option[Term]) extends Term
  @ast class Function(params: Seq[Param], body: Term) extends Term with Scope.Params {
    require(params.collect{ case named: Param.Named => named }.forall(_.default.isEmpty))
    require(params.exists(_.mods.exists(_.isInstanceOf[Mod.Implicit])) ==> (params.length == 1))
  }
  @ast class Cases(cases: Seq[Aux.Case] @nonEmpty) extends Term {
    def isPartialFunction = parent.isEmpty || {
      parent exists {
        case _: Match | _: Try => true
        case _                 => false
      }
    }
  }
  @ast class While(expr: Term, body: Term) extends Term
  @ast class Do(body: Term, expr: Term) extends Term
  @ast class For(enums: Seq[Enum] @nonEmpty, body: Term) extends Term with Scope {
    require(enums.head.isInstanceOf[Enum.Generator])
  }
  @ast class ForYield(enums: Seq[Enum] @nonEmpty, body: Term) extends Term with Scope
  @ast class New(templ: Aux.Template) extends Term
  @ast class Placeholder() extends Term
  @ast class Eta(term: Term) extends Term
}

@branch trait Type extends Tree with Param.Type with Scope.Template
object Type {
  @branch trait Ref extends Type with meta.Ref
  @ast class Name(value: String @nonEmpty, @trivia isBackquoted: Boolean = false) extends meta.Name with Ref {
    require(keywords.contains(value) ==> isBackquoted)
  }
  @ast class Select(qual: Qual.Type, selector: Type.Name) extends Ref {
    require(qual match { case qual: Term.Ref => qual.isPath; case qual: Qual.Super => true; case _ => unreachable })
  }
  @ast class Project(qual: Type, selector: Type.Name) extends Ref
  @ast class Singleton(ref: Qual.Type) extends Ref {
    require(ref match { case ref: Term.Ref => ref.isPath; case ref: Qual.Super => true; case _ => unreachable })
  }
  @ast class Apply(tpe: Type, args: Seq[Type] @nonEmpty) extends Type
  @ast class ApplyInfix(lhs: Type, op: Type, rhs: Type) extends Type
  @ast class Function(params: Seq[Param.Type], res: Type) extends Type
  @ast class Tuple(elements: Seq[Type] @nonEmpty) extends Type {
    require(elements.length > 1)
  }
  @ast class Compound(tpes: Seq[Type], refinement: Seq[Stmt.Refine] = Nil) extends Type with Scope.Refine {
    require(tpes.length == 1 ==> hasRefinement)
  }
  @ast class Existential(tpe: Type, quants: Seq[Stmt.Existential] @nonEmpty) extends Type with Scope.Existential
  @ast class Annotate(tpe: Type, annots: Seq[Mod.Annot] @nonEmpty) extends Type with Has.Mods {
    def mods: Seq[Mod] = annots
  }
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
    require(ref.isStableId)
  }
  @ast class ExtractInfix(lhs: Pat, ref: Term.Ref, rhs: Seq[Pat] @nonEmpty) extends Pat {
    require(ref.isStableId)
  }
  @ast class Interpolate(prefix: Term.Name, parts: Seq[Lit.String] @nonEmpty, args: Seq[Pat]) extends Pat {
    require(parts.length == args.length + 1)
  }
  @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
    require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Name])
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

@branch trait Member extends Tree with Has.Mods
object Member {
  @branch trait Term extends Member
  @branch trait Type extends Member
  @branch trait ValOrVar extends Stmt.Template with Has.Mods // NOTE: vals and vars are not members!
  @branch trait Def extends Term with Has.TermName with Stmt.Refine with Has.TypeParams with Has.Paramss with Scope.Params {
    def tparams: Seq[TypeParam]
  }
  @branch trait AbstractOrAliasType extends Type with Has.TypeName with Stmt.Refine with Has.TypeParams {
    def name: meta.Type.Name
    def tparams: Seq[TypeParam]
  }
  @branch trait Template extends Defn with Has.Name with Stmt.TopLevel with Stmt.Block with Has.TypeParams with Has.Paramss with Scope.Template {
    def name: meta.Name
    def explicits: Seq[Seq[Param.Named]] = Nil
    def implicits: Seq[Param.Named] = Nil
    def tparams: Seq[TypeParam] = Nil
    def templ: Aux.Template
  }
}

@branch trait Decl extends Stmt.Template with Stmt.Refine
object Decl {
  @ast class Val(mods: Seq[Mod],
                 pats: Seq[Term.Name] @nonEmpty,
                 decltpe: meta.Type) extends Decl with Member.ValOrVar with Stmt.Existential
  @ast class Var(mods: Seq[Mod],
                 pats: Seq[Term.Name] @nonEmpty,
                 decltpe: meta.Type) extends Decl with Member.ValOrVar
  @ast class Def(mods: Seq[Mod],
                 name: Term.Name,
                 tparams: Seq[TypeParam],
                 explicits: Seq[Seq[Param.Named]],
                 implicits: Seq[Param.Named],
                 decltpe: meta.Type) extends Decl with Member.Def
  @ast class Procedure(mods: Seq[Mod],
                       name: Term.Name,
                       tparams: Seq[TypeParam],
                       explicits: Seq[Seq[Param.Named]],
                       implicits: Seq[Param.Named]) extends Decl with Member.Def
  @ast class Type(mods: Seq[Mod],
                  name: meta.Type.Name,
                  tparams: Seq[TypeParam],
                  bounds: Aux.TypeBounds) extends Decl with Stmt.Existential with Member.AbstractOrAliasType
}

@branch trait Defn extends Stmt.Block with Stmt.Template
object Defn {
  @ast class Val(mods: Seq[Mod],
                 pats: Seq[Pat] @nonEmpty,
                 decltpe: Option[meta.Type],
                 rhs: Term) extends Defn with Member.ValOrVar with Stmt.Early
  @ast class Var(mods: Seq[Mod],
                 pats: Seq[Pat] @nonEmpty,
                 decltpe: Option[meta.Type],
                 rhs: Option[Term]) extends Defn with Member.ValOrVar with Stmt.Early {
    require(rhs.isEmpty ==> pats.forall(_.isInstanceOf[Term.Name]))
    require(decltpe.nonEmpty || rhs.nonEmpty)
  }
  @ast class Def(mods: Seq[Mod],
                 name: Term.Name,
                 tparams: Seq[TypeParam],
                 explicits: Seq[Seq[Param.Named]],
                 implicits: Seq[Param.Named],
                 decltpe: Option[meta.Type],
                 body: Term) extends Defn with Member.Def
  @ast class Procedure(mods: Seq[Mod],
                       name: Term.Name,
                       tparams: Seq[TypeParam],
                       explicits: Seq[Seq[Param.Named]],
                       implicits: Seq[Param.Named],
                       stats: Seq[Stmt.Block]) extends Defn with Member.Def {
    require(stats.collect { case m: Member if m.isPkgObject => m }.isEmpty)
  }
  @ast class Macro(mods: Seq[Mod],
                   name: Term.Name,
                   tparams: Seq[TypeParam],
                   explicits: Seq[Seq[Param.Named]],
                   implicits: Seq[Param.Named],
                   tpe: meta.Type,
                   body: Term) extends Defn with Member.Term with Has.TermName
  @ast class Type(mods: Seq[Mod],
                  name: meta.Type.Name,
                  tparams: Seq[TypeParam],
                  body: meta.Type) extends Defn with Stmt.Refine with Member.AbstractOrAliasType
  @ast class Class(mods: Seq[Mod],
                   name: meta.Type.Name,
                   override val tparams: Seq[TypeParam],
                   ctor: Ctor.Primary,
                   templ: Aux.Template) extends Defn with Member.Template with Has.TypeName
  @ast class Trait(mods: Seq[Mod],
                   name: meta.Type.Name,
                   override val tparams: Seq[TypeParam],
                   templ: Aux.Template) extends Defn with Member.Template with Has.TypeName {
    require(templ.stats.forall(!_.isInstanceOf[Ctor]))
    require(templ.parents.forall(_.argss.isEmpty))
  }
  @ast class Object(mods: Seq[Mod],
                    name: Term.Name,
                    templ: Aux.Template) extends Defn with Member.Template with Has.TermName {
  }
}

@ast class Pkg(ref: Term.Ref, stats: Seq[Stmt.TopLevel], @trivia hasBraces: Boolean = true)
     extends Stmt.TopLevel with Scope.TopLevel with Member.Term with Has.TermName {
  require(ref.isQualId)
  def mods: Seq[Mod] = Nil
  def name: Term.Name = ref match {
    case name: Term.Name      => name
    case Term.Select(_, name) => name
    case _                    => unreachable
  }
}

@branch trait Ctor extends Tree with Has.Mods with Has.Paramss
object Ctor {
  @ast class Primary(mods: Seq[Mod],
                     explicits: Seq[Seq[Param.Named]],
                     implicits: Seq[Param.Named]) extends Ctor
  @ast class Secondary(mods: Seq[Mod],
                       explicits: Seq[Seq[Param.Named]] @nonEmpty,
                       implicits: Seq[Param.Named],
                       primaryCtorArgss: Seq[Seq[Arg]],
                       stats: Seq[Stmt.Block]) extends Ctor with Stmt.Template with Scope.Params {
    require(stats.collect { case m: Member if m.isPkgObject => m }.isEmpty)
  }
}

@ast class Import(clauses: Seq[Import.Clause] @nonEmpty) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
object Import {
  @ast class Clause(ref: Term.Ref, sels: Seq[Selector] @nonEmpty) extends Tree {
    require(ref.isStableId)
  }

  @branch trait Selector extends Tree
  @ast class Wildcard() extends Selector
  @ast class Name(value: String, @trivia isBackquoted: Boolean = false) extends meta.Name with Selector
  @ast class Rename(from: Name, to: Name) extends Selector
  @ast class Unimport(name: Name) extends Selector
}

@branch trait Param extends Tree with Has.Mods {
  def decltpe: Option[Param.Type]
}
object Param {
  @branch trait Type extends Tree
  object Type {
    @ast class ByName(tpe: Type) extends Type
    @ast class Repeated(tpe: Type) extends Type
  }
  @ast class Anonymous(mods: Seq[Mod],
                       decltpe: Option[Type]) extends Param
  @ast class Named(mods: Seq[Mod],
                   name: Term.Name,
                   decltpe: Option[Type],
                   default: Option[Term]) extends Param with Member.Term with Has.TermName
}

@branch trait TypeParam extends Tree with Has.Mods with Has.TypeParams {
  def tparams: Seq[TypeParam]
  def contextBounds: Seq[meta.Type]
  def viewBounds: Seq[meta.Type]
  def bounds: Aux.TypeBounds
}
object TypeParam {
  @ast class Anonymous(mods: Seq[Mod],
                       tparams: Seq[TypeParam],
                       contextBounds: Seq[meta.Type],
                       viewBounds: Seq[meta.Type],
                       bounds: Aux.TypeBounds) extends TypeParam
  @ast class Named(mods: Seq[Mod],
                   name: meta.Type.Name,
                   tparams: Seq[TypeParam],
                   contextBounds: Seq[meta.Type],
                   viewBounds: Seq[meta.Type],
                   bounds: Aux.TypeBounds) extends TypeParam with Member.Type with Has.TypeName
}

@branch trait Arg extends Tree
object Arg {
  @ast class Named(name: Term.Name, rhs: Term) extends Arg
  @ast class Repeated(arg: Term) extends Arg
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
  @ast class Doc(doc: String) extends Mod
  @branch trait Access extends Mod { def within: Option[AccessQualifier] }
  @branch trait AccessQualifier extends Tree
  @ast class Private(within: Option[AccessQualifier]) extends Access
  @ast class Protected(within: Option[AccessQualifier]) extends Access
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
  @ast class Package() extends Mod
}

object Aux {
  @ast class CompUnit(stats: Seq[Stmt.TopLevel]) extends Tree
  @ast class Case(pat: Pat, cond: Option[Term], stats: Seq[Stmt.Block]) extends Tree with Scope {
    require(stats.collect { case m: Member if m.isPkgObject => m }.isEmpty)
  }
  @ast class Parent(tpe: Type, argss: Seq[Seq[Arg]]) extends Tree
  @ast class Template(early: Seq[Stmt.Early],
                      parents: Seq[Parent],
                      self: Self,
                      stats: Seq[Stmt.Template] = Nil) extends Tree with Scope.Template {
    require(parents.isEmpty || !parents.tail.exists(_.argss.nonEmpty))
    require(early.nonEmpty ==> parents.nonEmpty)
    require(stats.collect { case m: Member if m.isPkgObject => m }.isEmpty)
  }
  @ast class Self(name: Option[Term.Name], decltpe: Option[Type], @trivia hasThis: Boolean = false) extends Member.Term {
    def mods: Seq[Mod] = Nil
    require(hasThis ==> name.isEmpty)
  }
  @ast class TypeBounds(lo: Type = Type.Name("Nothing"), hi: Type = Type.Name("Any")) extends Tree
}

@branch trait Ref extends Tree

@branch trait Name extends Ref {
  def value: String
  def isBackquoted: Boolean
}

object Qual {
  @branch trait Term extends Tree
  @branch trait Type extends Term
  @ast class Name(value: String, @trivia isBackquoted: Boolean = false) extends meta.Name with Mod.AccessQualifier
  @ast class Super(thisp: Option[Qual.Name], superp: Option[Type.Name]) extends Qual.Term with Qual.Type
}

object Has {
  @branch trait Mods extends Tree {
    def mods: Seq[Mod]
  }
  @branch trait Paramss extends Tree {
    def explicits: Seq[Seq[Param.Named]]
    def implicits: Seq[Param.Named]
    def paramss: Seq[Seq[Param.Named]] = explicits :+ implicits
  }
  @branch trait TypeParams extends Tree {
    def tparams: Seq[TypeParam]
  }
  @branch trait Name extends Member { def name: meta.Name }
  @branch trait TermName extends Member.Term with Has.Name { def name: Term.Name }
  @branch trait TypeName extends Member.Type with Has.Name { def name: Type.Name }
}

@branch trait Stmt extends Tree
object Stmt {
  @branch trait TopLevel extends Stmt
  @branch trait Template extends Stmt
  @branch trait Block extends Template
  @branch trait Refine extends Template
  @branch trait Existential extends Refine
  @branch trait Early extends Block
}

@branch trait Scope extends Tree
object Scope {
  @branch trait TopLevel extends Scope with Block
  @branch trait Template extends Block with Params
  @branch trait Block extends Refine
  @branch trait Refine extends Existential
  @branch trait Existential extends Scope
  @branch trait Params extends Scope
}
