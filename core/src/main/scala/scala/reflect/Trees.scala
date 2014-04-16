package scala.reflect

// TODO: tree-based symbols and types (see https://github.com/paulbutcher/implementor/blob/f1921de2b7de3d5ea8cf7f230c8e4e9f8c7f4b26/core/src/main/scala/org/scalamock/Implement.scala)
// TODO: .tpe vs .signature?
// TODO: parser
// TODO: pretty printer
// TODO: decide on entry point tree (compilation unit? package?; use-cases compile-time, runtime, presentation)
// TODO: isModifier methods for annotations
// TODO: think about requiring ident values to be non-keyword
// TODO: newcase

sealed trait Tree {
  // TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  // TODO: history vs positions (can trivia be inferred from positions only?)
  // TODO: collection-like methods (see http://clang.llvm.org/docs/LibASTMatchersReference.html)
  // TODO: rewriting/transformation methods
  // TODO: add tree-specific equalitities as ref_==, =:= etc
}

object Tree {
  object Stmt {
    sealed trait TopLevel extends Tree
    sealed trait Template extends Tree
    sealed trait Block extends Tree
    sealed trait Refine extends Tree
    sealed trait Existential extends Tree
  }

  // TODO: wildcard (find all its usages in terms and types and defns and params and type params)

  sealed trait Term extends Arg with Stmt.Template with Stmt.Block
  object Term {
    sealed trait Ref extends Term {
      def isPath: Boolean = ???
      def isQualId: Boolean = ???
      def isStableId: Boolean = ???
    }
    final case class This(qual: Option[Ident]) extends Ref
    final case class Ident(name: String) extends Ref with Pat { def isBackquoted = ??? }
    final case class SuperSelect(qual: Option[Term.Ident], supertyp: Option[Term.Ident], selector: Term.Ident) extends Ref
    final case class Select(qual: Ref, selector: Term.Ident) extends Ref with Pat

    sealed trait Lit extends Term with Pat
    final case class Bool(value: scala.Boolean) extends Lit
    final case class Int(value: scala.Int) extends Lit
    final case class Long(value: scala.Long) extends Lit
    final case class Float(value: scala.Float) extends Lit
    final case class Double(value: scala.Double) extends Lit
    final case class Char(value: scala.Char) extends Lit
    final case class String(value: Predef.String) extends Lit
    final case class Symbol(value: scala.Symbol) extends Lit
    final case class Null() extends Lit
    final case class Unit() extends Lit

    final case class Interpolate(prefix: Ident, parts: List[Term.String], args: List[Term]) extends Term
    final case class Apply(fun: Term, args: List[Arg]) extends Term
    final case class ApplyRight(arg: Term, fun: Term) extends Term
    final case class TypeApply(fun: Term, args: List[Type]) extends Term
    final case class Assign(lhs: Term.Ref, rhs: Term) extends Term
    final case class Update(expr: Term, args: List[List[Term]]) extends Term
    final case class Return(expr: Term) extends Term
    final case class Throw(expr: Term) extends Term
    final case class Ascribe(expr: Term, typ: Type) extends Term
    final case class Annotate(expr: Term, annots: Annots.Term) extends Term
    final case class Tuple(elements: List[Term]) extends Term
    final case class Block(stats: List[Stmt.Block]) extends Term {
      // TODO: require(stats.flatMap(_.annots).forAll(_.isValidForBlock)))
      // modifiers
      //   implicit, lazy + val, var, def, type
      //   abstract, final, sealed, implicit, lazy + class, trait, object
    }
    final case class If(cond: Term, `then`: Term, `else`: Term) extends Term
    final case class Match(scrut: Term, cases: List[Case]) extends Term
    final case class Try(expr: Term, `catch`: List[Case], `finally`: Option[Term]) extends Term
    final case class Function(params: List[Param.Function], body: Term) extends Term {
      // TODO: require(params.length == 1 || params.flatMap(_.annots).forAll(!_.contains(Annot.Implicit)))
    }
    final case class PartialFunction(cases: List[Case]) extends Term
    final case class While(expr: Term, body: Term) extends Term
    final case class Do(body: Term, expr: Term) extends Term
    final case class For(enums: List[Enumerator], body: Term) extends Term
    final case class ForYield(enums: List[Enumerator], body: Term) extends Term
    final case class New(templ: Template) extends Term
  }

  sealed trait Pat extends Tree
  object Pat {
    final case class Wildcard() extends Pat
    final case class SequenceWildcard() extends Pat
    final case class Bind(lhs: Term.Ident, rhs: Pat) extends Pat // wishful thinking AND
    final case class Alt(lhs: Pat, rhs: Pat) extends Pat // wishful thinking OR
    final case class Tuple(elements: List[Pat]) extends Pat
    final case class Extractor(ref: Term.Ref, elements: List[Pat]) extends Pat { require(ref.isStableId) }
    // final case class Guard(pat: Pat, cond: Term) extends Pat // wishful thinking
    final case class Interpolate(prefix: Term.Ident, parts: List[Term.String], args: List[Pat]) extends Pat
    final case class Typed(lhs: Pat, rhs: Type) extends Pat { require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Ident]) }
  }

  sealed trait Type extends Tree
  object Type {
    final case class Ident(name: String) extends Type
    final case class Select(qual: Term.Ref, name: Type.Ident) extends Type { require(qual.isPath) }
    final case class SuperSelect(qual: Option[Term.Ident], supertyp: Option[Term.Ident], selector: Type.Ident) extends Type
    final case class Project(qual: Type, name: Type.Ident) extends Type
    final case class Singleton(ref: Term.Ref) extends Type { require(ref.isPath) }
    final case class Constant(value: Term.Lit) extends Type
    final case class Apply(typ: Type, targs: List[Type]) extends Type
    final case class Compound(parents: List[Type], stmts: List[Stmt.Refine]) extends Type
    final case class Existential(typ: Type, quants: List[Stmt.Existential]) extends Type
    final case class Function(params: Type, res: Type) extends Type
    final case class Tuple(elements: List[Type]) extends Type
    final case class Annotated(typ: Type, annots: Annots.Type) extends Type
  }

  sealed trait Decl extends Stmt.Template with Stmt.Refine
  object Decl {
    final case class Val(annots: Annots.Decl.Val, pats: List[Pat], typ: Type) extends Decl with Stmt.Existential
    final case class Var(annots: Annots.Decl.Var, pats: List[Pat], typ: Type) extends Decl
    final case class Def(annots: Annots.Decl.Def, name: Term.Ident, tparams: List[TypeParam.Method],
                         paramss: List[List[Param.Method]], implicits: List[Param.Method],
                         typ: Type) extends Decl
    final case class Type(annots: Annots.Decl.Type, name: Tree.Type.Ident, tparams: List[TypeParam.Type],
                          bounds: TypeBounds) extends Decl with Stmt.Existential
  }

  sealed trait Defn extends Stmt.Template
  object Defn {
    final case class Val(annots: Annots.Defn.Val, pats: List[Pat], typ: Option[Type], rhs: Term) extends Defn with Stmt.Block
    final case class Var(annots: Annots.Defn.Var, pats: List[Pat], typ: Option[Type], rhs: Term) extends Defn with Stmt.Block
    final case class Def(annots: Annots.Defn.Def, name: Term.Ident, tparams: List[TypeParam.Method],
                         paramss: List[List[Param.Method]], implicits: List[Param.Method],
                         typ: Option[Type], body: Term) extends Defn with Stmt.Block
    final case class Macro(annots: Annots.Defn.Macro, name: Term.Ident, tparams: List[TypeParam.Method],
                           paramss: List[List[Param.Method]], implicits: List[Param.Method],
                           typ: Type, body: Term) extends Defn with Stmt.Block
    final case class Type(annots: Annots.Defn.Type, name: Tree.Type.Ident, tparams: List[TypeParam.Type],
                          body: Type) extends Defn with Stmt.Refine with Stmt.Block
    final case class PrimaryCtor(annots: Annots.Defn.PrimaryCtor, paramss: List[List[Param.Class]],
                                 implicits: List[Param.Class]) extends Defn
    final case class SecondaryCtor(annots: Annots.Defn.SecondaryCtor, paramss: List[List[Param.Method]],
                                   implicits: List[Param.Method], primaryCtorArgss: List[List[Term]]) extends Defn with Stmt.Block
    final case class Class(annots: Annots.Defn.Class, name: Tree.Type.Ident, tparams: List[TypeParam.Class],
                           ctor: PrimaryCtor, templ: Template) extends Defn with Stmt.TopLevel with Stmt.Block
    final case class Trait(annots: Annots.Defn.Trait, name: Tree.Type.Ident, tparams: List[TypeParam.Trait],
                           templ: Template) extends Defn with Stmt.TopLevel with Stmt.Block {
      def isInterface: Boolean = templ.stats.forall(_.isInstanceOf[Decl])
    }
    final case class Object(annots: Annots.Defn.Object, name: Term.Ident,
                            templ: Template) extends Defn with Stmt.TopLevel with Stmt.Block
    final case class Package(ref: Term.Ref, body: List[Stmt.TopLevel]) extends Defn with Stmt.TopLevel { require(ref.isQualId) }
    final case class PackageObject(name: Term.Ident, templ: Template) extends Defn with Stmt.TopLevel
  }

  final case class Import(clauses: List[Import.Clause]) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
  object Import {
    final case class Clause(ref: Term.Ref, sels: List[Selector]) extends Tree { require(ref.isStableId) }

    sealed trait Selector extends Tree
    object Selector {
      // TODO: are we happy with having these as strings?
      // neither TermName, nor TypeName along won't cut this
      // however if we leave strings in place, then we won't be able to validate names
      final case class Wildcard() extends Selector
      final case class Name(name: String) extends Selector
      final case class Rename(from: String, to: String) extends Selector
      final case class Unimport(name: String) extends Selector
    }
  }

  sealed trait Arg extends Tree
  object Arg {
    final case class Named(name: Term.Ident, arg: Term) extends Arg
    final case class Sequence(arg: Term) extends Arg
  }

  final case class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree

  final case class Template(early: List[Defn.Val], parents: List[Parent],
                            self: Self, stats: List[Stmt.Template]) extends Tree {
    require(parents.length == 0 || parents.tail.forall(_.argss.isEmpty))
  }

  sealed trait Enumerator extends Tree
  object Enumerator {
    final case class Generator(pat: Pat, rhs: Term) extends Enumerator
    final case class ValueDefinition(pat: Pat, rhs: Term) extends Enumerator
    final case class Guard(cond: Term) extends Enumerator
  }

  // TODO: `name` can also be `this`
  final case class Self(name: Term.Ident, typ: Option[Type]) extends Tree

  final case class Parent(tpe: Type, argss: List[List[Term]]) extends Tree

  final case class TypeBounds(lo: Option[Type], hi: Option[Type]) extends Tree

  trait Param extends Tree
  object Param {
    final case class Function(annots: Annots.Param.Function, name: Term.Ident, typ: Option[Type]) extends Param
    // TODO: also support by-name and vararg parameters
    final case class Method(annots: Annots.Param.Method, name: Term.Ident, typ: Type, default: Option[Term]) extends Param
    // TODO: also support by-name and vararg parameters
    // TODO: also support `val` and `var` variations
    final case class Class(annots: Annots.Param.Class, name: Term.Ident, typ: Type, default: Option[Term]) extends Param
  }

  trait TypeParam extends Tree
  object TypeParam {
    final case class Method(annots: Annots.TypeParam.Method, name: Tree.Type.Ident,
                            tparams: List[TypeParam.Type],
                            contextBounds: List[Tree.Type],
                            viewBounds: List[Tree.Type],
                            bounds: TypeBounds) extends TypeParam
    final case class Type(annots: Annots.TypeParam.Type, name: Tree.Type.Ident,
                          tparams: List[TypeParam.Type],
                          bounds: TypeBounds) extends TypeParam
    final case class Trait(annots: Annots.TypeParam.Trait, name: Tree.Type.Ident,
                           tparams: List[TypeParam.Type],
                           bounds: TypeBounds) extends TypeParam
    final case class Class(annots: Annots.TypeParam.Class, name: Tree.Type.Ident,
                           tparams: List[TypeParam.Type],
                           contextBounds: List[Tree.Type],
                           viewBounds: List[Tree.Type],
                           bounds: TypeBounds) extends TypeParam
  }

  sealed trait Annot extends Tree {
    // TODO: convert annotations to value objects (Liftable?)
    // TODO: validate impossible combinations of modifiers (can't have private and protected at the same time)
    protected[reflect] def validate(enclosing: Tree, owner: Tree, annots: List[Annot]): Boolean = ???
  }
  object Annot {
    sealed trait Transient extends Annot // TODO: reserved for synthetic trees (e.g. resolved implicits) and attachments

    sealed trait Source extends Annot
    final case class UserDefined(tpe: Type, argss: List[List[Term]]) extends Source

    sealed trait Mod extends Source
    final case class Private(within: String) extends Mod
    final case class Protected(within: String) extends Mod
    final case class Implicit() extends Mod
    final case class Final() extends Mod
    final case class Sealed() extends Mod
    final case class Override() extends Mod
    final case class Case() extends Mod
    final case class Abstract() extends Mod
    final case class Covariant() extends Mod
    final case class Contravariant() extends Mod
    final case class Lazy() extends Mod
    final case class Doc(doc: String) extends Mod
    final case class AbstractOverride() extends Mod
    // can only be allowed for value members of traits
  }
}

