package scala.reflect

import scala.language.experimental.{macros => prettyPlease}
import org.scalareflect.invariants._
import org.scalareflect.adt._

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
// (Together) TODO: figure out which apis need to be moved to subclasses and which apis (Tree.mods, Type.tpeCtor, Type.args, Symbol.companion, Type.companion) need to be added for convenience
// (Together) TODO: implement scaladoc with palladium

// TODO: converter: double check conversion of `(f _)(x)` (bug 46)
// TODO: converter: need api to discern `class C` from `class C {}` as the second one has to have EmptyTree in the body

package core {
  @root trait Tree extends cbc.Trees.Tree {
    def parent: Option[Tree] = ???
    override def equals(other: Any): Boolean = ???
    override def hashCode(): Int = ???
  }

  @branch trait Ref extends Tree {
    def sym: Symbol = ???
  }

  @branch trait Ident extends Ref {
    def value: String
    def isBackquoted: Boolean
    def isRightAssocOp: Boolean = value.last != ':'
    def isUnaryOp: Boolean = unaryOps contains value
    def isAssignmentOp = value match {
      case "!=" | "<=" | ">=" | "" => false
      case _                       => value.last == '=' && value.head != '=' && isOperatorPart(value.head)
    }
    // opPrecedence?
    def precedence: Int =
      if (isAssignmentOp) 0
      else if (isScalaLetter(value.head)) 1
      else (value.head: @scala.annotation.switch) match {
        case '|'             => 2
        case '^'             => 3
        case '&'             => 4
        case '=' | '!'       => 5
        case '<' | '>'       => 6
        case ':'             => 7
        case '+' | '-'       => 8
        case '*' | '/' | '%' => 9
        case _               => 10
      }
    def isVarPattern: Boolean = this match {
      case _: Term.Ident => !isBackquoted && value.head.isLower && value.head.isLetter
      case _             => false
    }
    def isInterpolationId: Boolean = ???

    private def isOperatorPart(ch: Char) = ???
    private def isScalaLetter(ch: Char) = ???
  }

  @branch trait Term extends Arg with Stmt.Template with Stmt.Block with Aux.Catch {
    def tpe: Type = ???
  }
  object Term {
    @branch trait Ref extends Term with core.Ref {
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
    @leaf class Ident(value: scala.Predef.String @nonEmpty, isBackquoted: Boolean = false) extends core.Ident with Ref with Pat with Symbol {
      require(!keywords.contains(value) || isBackquoted)
      def isBinding: Boolean = ???
      def mods: List[Mod] = Nil
    }
    @leaf class SuperSelect(qual: Option[core.Ident], supertpe: Option[Type.Ident], selector: Term.Ident) extends Ref
    @leaf class Select(qual: Term, selector: Term.Ident) extends Ref with Pat

    @leaf class Interpolate(prefix: Ident, parts: List[Lit.String] @nonEmpty, args: List[Term]) extends Term {
      require(prefix.isInterpolationId)
      require(parts.length == args.length + 1)
    }
    @leaf class Apply(fun: Term, args: List[Arg]) extends Term
    @leaf class ApplyType(fun: Term, args: List[Type] @nonEmpty) extends Term
    @leaf class ApplyRight(lhs: Term, op: Ident, targs: List[Type], rhs: Term) extends Term {
      require(op.isRightAssocOp)
    }
    @leaf class ApplyUnary(op: Ident, arg: Term) extends Term {
      require(op.isUnaryOp)
    }
    @leaf class Assign(lhs: Term.Ref, rhs: Term) extends Term
    @leaf class Update(lhs: Apply, rhs: Term) extends Term
    @leaf class Return(expr: Term) extends Term
    @leaf class Throw(expr: Term) extends Term
    @leaf class Ascribe(expr: Term, acsribedTpe: Type) extends Term
    @leaf class Annotate(expr: Term, mods: List[Mod] @nonEmpty) extends Term with Has.Mods
    @leaf class Tuple(elements: List[Term] @nonEmpty) extends Term
    @leaf class Block(stats: List[Stmt.Block]) extends Term
    @leaf class If(cond: Term, thenp: Term, elsep: Term) extends Term
    @leaf class Match(scrut: Term, cases: Aux.Cases @nonEmpty) extends Term
    @leaf class Try(expr: Term, catchp: Option[Aux.Catch], finallyp: Option[Term]) extends Term
    @leaf class Function(params: List[Aux.Param], body: Term) extends Term {
      require(params.forall(_.default.isEmpty))
      require(params.length == 1 || !params.exists(_.mods.contains(Mod.Implicit)))
    }
    @leaf class PartialFunction(cases: Aux.Cases) extends Term
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

  @branch trait Type extends Tree {
    def <:< (other: Type): Boolean = ???
    def weak_<:<(other: Type): Boolean = ???
    def widen: Type = ???
    def dealias: Type = ???
    def erasure: Type = ???
    def superclasses: List[Type] = ???
    def subclasses: List[Type] = ???
    // TODO: can we somehow inherit certain flags from type symbols here, s.t. we can write `t"T".isCaseClass`?
    // TODO: simple type validation
  }
  object Type {
    @branch trait Ref extends Type with core.Ref
    @leaf class Ident(value: String @nonEmpty, isBackquoted: Boolean = false) extends core.Ident with Ref {
      require(!keywords.contains(value) || isBackquoted)
    }
    @leaf class Select(qual: Term.Ref, name: Type.Ident) extends Ref {
      require(qual.isPath)
    }
    @leaf class SuperSelect(qual: Option[core.Ident], supertpe: Option[Type.Ident], selector: Type.Ident) extends Ref
    @leaf class Project(qual: Type, name: Type.Ident) extends Ref
    @leaf class Singleton(ref: Term.Ref) extends Ref {
      require(ref.isPath)
    }

    // TODO: validate that tpe can actually be applied
    @leaf class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
    @leaf class Function(params: List[Type], res: Type) extends Type
    @leaf class Tuple(elements: List[Type] @nonEmpty) extends Type
    @leaf class Compound(tpes: List[Type], refinement: List[Stmt.Refine] @nonEmpty) extends Type
    @leaf class Existential(tpe: Type, quants: List[Stmt.Existential] @nonEmpty) extends Type
    @leaf class Annotate(tpe: Type, mods: List[Mod] @nonEmpty) extends Type with Has.Mods
    // (Denys) TODO: need to validate that placeholder appears within one of allowed contexts (e.g. `type T = _` is illegal)
    @leaf class Placeholder(bounds: Aux.TypeBounds) extends Type
  }

  @branch trait Pat extends Tree {
    // TODO: how should tpe look like? inTpe/outTpe?
  }
  object Pat {
    @leaf class Wildcard() extends Pat
    @leaf class SeqWildcard() extends Pat
    @leaf class Bind(lhs: Term.Ident, rhs: Pat) extends Pat
    @leaf class Alternative(lhs: Pat, rhs: Pat) extends Pat
    @leaf class Tuple(elements: List[Pat] @nonEmpty) extends Pat
    @leaf class Extract(ref: Term.Ref, targs: List[Type], elements: List[Pat]) extends Pat {
      require(ref.isStableId)
    }
    @leaf class Interpolate(prefix: Term.Ident, parts: List[Lit.String] @nonEmpty, args: List[Pat]) extends Pat {
      require(prefix.isInterpolationId)
      require(parts.length == args.length + 1)
    }
    @leaf class Typed(lhs: Pat, rhs: Type) extends Pat {
      require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Ident])
    }
  }

  @branch trait Symbol extends Tree with Has.Mods {
    def owner: Symbol = ???
    def ref: core.Ref = ???
    def overrides: List[Symbol] = ???
    def doc: Option[Mod.Doc] = ???
    // TODO: knownDirectSubclasses => can we replace this with an optimized query?
    // TODO: methods from Scope. do we need them here? how do we deduplicate wrt types?
  }
  object Symbol {
    @branch trait Field extends Symbol
    @branch trait Val extends Field with Has.DeclaredType
    @branch trait Var extends Field with Has.DeclaredType
    @branch trait Def extends Symbol with Stmt.Template with Has.TparamClause with Has.ParamClauses
    @branch trait Type extends Symbol with Has.TparamClause
    @branch trait Template extends Symbol with Stmt.TopLevel with Stmt.Block
  }

  @branch trait Decl extends Symbol with Stmt.Template with Stmt.Refine
  object Decl {
    @leaf class Val(mods: List[Mod],
                    pats: List[Term.Ident] @nonEmpty,
                    declaredTpe: Option[core.Type] @nonEmpty) extends Decl with Stmt.Existential with Symbol.Val
    @leaf class Var(mods: List[Mod],
                    pats: List[Term.Ident] @nonEmpty,
                    declaredTpe: Option[core.Type] @nonEmpty) extends Decl with Symbol.Var
    @leaf class Def(mods: List[Mod],
                    name: Term.Ident,
                    tparamClause: Option[Aux.TypeParamClause],
                    paramClauses: Option[Aux.ParamClauses],
                    declaredTpe: Option[core.Type] @nonEmpty) extends Decl with Symbol.Def with Has.DeclaredType {
      require(paramss.flatten.forall(_.declaredTpe.nonEmpty))
    }
    @leaf class Procedure(mods: List[Mod],
                          name: Term.Ident,
                          tparamClause: Option[Aux.TypeParamClause],
                          paramClauses: Option[Aux.ParamClauses]) extends Decl with Symbol.Def
    @leaf class Type(mods: List[Mod],
                     name: core.Type.Ident,
                     tparamClause: Option[Aux.TypeParamClause],
                     bounds: Aux.TypeBounds) extends Decl with Stmt.Existential with Symbol.Type
  }

  @branch trait Defn extends Symbol with Stmt.Block with Stmt.Template
  object Defn {
    @leaf class Val(mods: List[Mod],
                    pats: List[Pat] @nonEmpty,
                    declaredTpe: Option[core.Type],
                    rhs: Term) extends Defn with Symbol.Val
    @leaf class Var(mods: List[Mod],
                    pats: List[Pat] @nonEmpty,
                    declaredTpe: Option[core.Type],
                    rhs: Option[Term]) extends Defn with Symbol.Var {
      require(rhs.nonEmpty || pats.forall(_.isInstanceOf[Term.Ident]))
      require(declaredTpe.nonEmpty || rhs.nonEmpty)
    }
    @leaf class Def(mods: List[Mod],
                    name: Term.Ident,
                    tparamClause: Option[Aux.TypeParamClause],
                    paramClauses: Option[Aux.ParamClauses],
                    declaredTpe: Option[core.Type],
                    body: Term) extends Defn with Symbol.Def with Has.DeclaredType
    @leaf class Procedure(mods: List[Mod],
                          name: Term.Ident,
                          tparamClause: Option[Aux.TypeParamClause],
                          paramClauses: Option[Aux.ParamClauses],
                          body: Term.Block) extends Defn with Symbol.Def
    @leaf class Type(mods: List[Mod],
                     name: core.Type.Ident,
                     tparamClause: Option[Aux.TypeParamClause],
                     body: core.Type) extends Defn with Stmt.Refine with Symbol.Type
    @leaf class Class(mods: List[Mod],
                      name: core.Type.Ident,
                      tparamClause: Option[Aux.TypeParamClause],
                      ctor: Ctor.Primary,
                      templ: Aux.Template) extends Defn with Symbol.Template
                                              with Has.TparamClause {
      def companion: Option[Object] = ???
    }
    @leaf class Trait(mods: List[Mod],
                      name: core.Type.Ident,
                      tparamClause: Option[Aux.TypeParamClause],
                      templ: Aux.Template) extends Defn with Symbol.Template
                                              with Has.TparamClause {
      def isInterface: Boolean = templ.stats.forall(_.isInstanceOf[Decl])
      def companion: Option[Object] = ???
    }
    @leaf class Object(mods: List[Mod],
                       name: Term.Ident,
                       templ: Aux.Template) extends Defn with Symbol.Template {
      def companion: Option[Symbol.Template] = ??? // TODO: Class | Trait
    }
  }

  @branch trait Ctor extends Symbol with Has.ParamClauses
  object Ctor {
    @leaf class Primary(mods: List[Mod] = Nil,
                        paramClauses: Option[Aux.ParamClauses] = None) extends Ctor
    @leaf class Secondary(mods: List[Mod] = Nil,
                          paramClauses: Option[Aux.ParamClauses] = None,
                          primaryCtorArgss: List[List[Arg]] = Nil,
                          declaredStats: Option[List[Stmt.Block]] = None) extends Ctor with Stmt.Template {
      def stats: List[Stmt.Block] = declaredStats.getOrElse(Nil)
    }
  }

  object Stmt {
    @branch trait TopLevel extends Tree
    @branch trait Template extends Block
    @branch trait Block extends Refine
    @branch trait Refine extends Existential
    @branch trait Existential extends Tree
  }

  // TODO: design scopes
  // should include parents, self, members, methods, types, etc
  // where things like methods should be Iterable[Symbol] + have apply(ident) and apply(symbol)
  @branch trait Scope extends Tree

  @branch trait Lit extends Term with Pat
  object Lit {
    // TODO: maybe add overloaded apply(value)
    // TODO: maybe add unapply(lit): Option[Any]
    @branch trait Bool extends Lit
    @leaf class True() extends Bool
    @leaf class False() extends Bool
    @leaf class Int(value: scala.Int) extends Lit with Type
    @leaf class Long(value: scala.Long) extends Lit with Type
    @leaf class Float(value: scala.Float) extends Lit with Type
    @leaf class Double(value: scala.Double) extends Lit with Type
    @leaf class Char(value: scala.Char) extends Lit with Type
    @leaf class String(value: Predef.String) extends Lit with Type
    // TODO: not all symbols are representable as literals, e.g. scala.Symbol("")
    @leaf class Symbol(value: scala.Symbol) extends Lit with Type
    @leaf class Null() extends Lit
    @leaf class Unit() extends Lit
  }

  @leaf class Import(clauses: List[Import.Clause] @nonEmpty) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
  object Import {
    // TODO: validate that wildcard import can only be the last one in the list of sels
    @leaf class Clause(ref: Term.Ref, sels: List[Selector] @nonEmpty) extends Tree {
      require(ref.isStableId)
    }

    @branch trait Selector extends Tree
    object Selector {
      @leaf class Wildcard() extends Selector
      // TODO: needs some kind of idents here but they can neither be term nor type
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

  @branch trait Mod extends Tree
  object Mod {
    @leaf class Annot(tpe: Type, argss: List[List[Arg]]) extends Mod
    @leaf class Doc(doc: String) extends Mod // TODO: design representation for scaladoc
    @leaf class Private(within: String) extends Mod // TODO: design a name resolution API for these and imports
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
    @leaf class AbstractOverride() extends Mod
    @leaf class Macro() extends Mod
    @leaf class ByNameParam() extends Mod
    @leaf class VarargParam() extends Mod
    @leaf class ValParam() extends Mod
    @leaf class VarParam() extends Mod
  }

  object Aux {
    @branch trait Catch extends Tree
    @leaf class Case(pat: Pat, cond: Option[Term] = None, body: Term = Lit.Unit()) extends Tree
    @leaf class Cases(cases: List[Case] @nonEmpty) extends Catch
    @leaf class Parent(tpe: Type, argss: List[List[Arg]] = Nil) extends Ref
    @leaf class Template(early: List[Defn.Val] = Nil, parents: List[Parent] = Nil,
                         self: Self = Self.empty, declaredStats: Option[List[Stmt.Template]] = None) extends Tree {
      require(parents.isEmpty || !parents.tail.exists(_.argss.nonEmpty))
      // TODO: validate that trait parents can't have value parameters
      def stats: List[Stmt.Template] = declaredStats.getOrElse(Nil)
    }
    @leaf class Self(name: Option[Term.Ident] = None, tpe: Option[Type] = None) extends Symbol {
      def mods: List[Mod] = Nil
    }
    @leaf class Param(mods: List[Mod] = Nil, name: Option[Term.Ident] = None,
                      declaredTpe: Option[Type] = None, default: Option[Term] = None) extends Symbol
    @leaf class ParamClauses(paramss: List[List[Param]] = Nil, implicits: List[Param] = Nil) extends Tree
    @leaf class TypeParam(mods: List[Mod] = Nil,
                          name: Option[core.Type.Ident] = None,
                          tparamClause: Option[Aux.TypeParamClause] = None,
                          contextBounds: List[core.Type] = Nil,
                          viewBounds: List[core.Type] = Nil,
                          bounds: Aux.TypeBounds = Aux.TypeBounds.empty) extends Symbol
    @leaf class TypeParamClause(tparams: List[TypeParam] @nonEmpty) extends Tree
    @leaf class TypeBounds(lo: Option[Type] = None, hi: Option[Type] = None) extends Tree
  }

  object Has {
    @branch trait ParamClauses {
      def paramClauses: Option[Aux.ParamClauses]
      def paramss: List[List[Aux.Param]] = paramClauses.map(pc => pc.paramss :+ pc.implicits).getOrElse(Nil)
    }

    @branch trait TparamClause {
      def tparamClause: Option[Aux.TypeParamClause]
      def tparams: List[Aux.TypeParam] = tparamClause.map(_.tparams).getOrElse(Nil)
    }

    @branch trait Mods extends Tree {
      def mods: List[Mod]
      // (Eugene) TODO: https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
      // * write a script that fetches this google doc and converts it into a, say, CSV spec
      // * write a test that validates the spec by generating source files and parsing them
      // * write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
      private[reflect] def validateMods(): Boolean = ???
    }

    @branch trait DeclaredType extends Tree {
      def declaredTpe: Option[Type]
      def tpe: Type = ???
    }
  }

  @branch trait Package extends Tree
}

package object core {
  object Package {
    @leaf class Empty(stats: List[Stmt.TopLevel]) extends Package
    @leaf class Named(name: Term.Ref,
                      declaredStats: Option[List[Stmt.TopLevel]]) extends Package with Stmt.TopLevel {
      require(name.isQualId)
      def stats: List[Stmt.TopLevel] = declaredStats.getOrElse(Nil)
    }
    @leaf class Object(mods: List[Mod],
                       name: Term.Ident,
                       templ: Aux.Template) extends Package with Stmt.TopLevel with Symbol.Template
  }

  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )
  val unaryOps = Set("-", "+", "~", "!")
  implicit class Quasiquotes(ctx: StringContext) {
    protected trait api {
      def apply[T](args: T*): Tree = macro ???
      def unapply(scrutinee: Any): Any = macro ???
    }
    object q extends api
    object tq extends api
    object cq extends api
    object pq extends api
    object fq extends api
  }
  implicit class RichTypes(val parents: List[Type]) extends AnyVal {
    def linearization: List[Type] = ???
  }
  implicit class RichMods(val mods: List[Mod]) extends AnyVal {
    def has[T <: Mod](implicit tag: ClassTag[T]): Boolean =
      mods.exists { _.getClass == tag.runtimeClass }
  }
  def lub(tpes: Type*): Type = ???
  def glb(tpes: Type*): Type = ???
}
