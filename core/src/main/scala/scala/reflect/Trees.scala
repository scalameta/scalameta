package scala.reflect

// TODO: tree-based symbols and types (see https://github.com/paulbutcher/implementor/blob/f1921de2b7de3d5ea8cf7f230c8e4e9f8c7f4b26/core/src/main/scala/org/scalamock/Implement.scala)
// TODO: .tpe vs .signature?
// TODO: parser
// TODO: pretty printer
// TODO: decide on entry point tree (compilation unit? package?; use-cases compile-time, runtime, presentation)
// TODO: think about requiring ident values to be non-keyword
// TODO: consider adding default values for case class fields whenever applicable

@adt trait Tree {
  // TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  // TODO: history vs positions (can trivia be inferred from positions only?)
  // TODO: collection-like methods (see http://clang.llvm.org/docs/LibASTMatchersReference.html)
  // TODO: rewriting/transformation methods
  // TODO: add tree-specific equalities as ref_==, =:= etc
}

object Tree {
  object Stmt {
    // TODO: statements must be related through inheritence whenever possible
    // otherwise it would be impossible to extract a statement from a template and insert it into a block
    // (which might not be a bad thing by the way...)
    @adt trait TopLevel extends Tree
    @adt trait Template extends Tree
    @adt trait Block extends Tree
    @adt trait Refine extends Tree
    @adt trait Existential extends Tree
  }

  // TODO: wildcard (find all its usages in terms and types and defns and params and type params)

  @adt trait Term extends Arg with Stmt.Template with Stmt.Block
  object Term {
    @adt trait Ref extends Term {
      def isPath: Boolean = ???
      def isQualId: Boolean = ???
      def isStableId: Boolean = ???
    }
    @leaf class This(qual: Option[Ident]) extends Ref
    @leaf class Ident(name: String) extends Ref with Pat {
      def isBackquoted = ???
    }
    @leaf class SuperSelect(qual: Option[Term.Ident], supertyp: Option[Term.Ident], selector: Term.Ident) extends Ref
    @leaf class Select(qual: Ref, selector: Term.Ident) extends Ref with Pat

    @adt trait Lit extends Term with Pat
    @leaf class Bool(value: scala.Boolean) extends Lit
    @leaf class Int(value: scala.Int) extends Lit
    @leaf class Long(value: scala.Long) extends Lit
    @leaf class Float(value: scala.Float) extends Lit
    @leaf class Double(value: scala.Double) extends Lit
    @leaf class Char(value: scala.Char) extends Lit
    @leaf class String(value: Predef.String) extends Lit
    @leaf class Symbol(value: scala.Symbol) extends Lit
    @leaf class Null() extends Lit
    @leaf class Unit() extends Lit

    @leaf class Interpolate(prefix: Ident, parts: List[Term.String] @nonEmpty, args: List[Term]) extends Term {
      // TODO: also check that prefix is alphanumeric
      require(parts.length == args.length + 1)
    }
    @leaf class Apply(fun: Term, args: List[Arg]) extends Term
    @leaf class ApplyRight(arg: Term, fun: Term) extends Term
    @leaf class TypeApply(fun: Term, args: List[Type] @nonEmpty) extends Term
    @leaf class Assign(lhs: Term.Ref, rhs: Term) extends Term
    @leaf class Update(expr: Term, args: List[List[Term]] @nonEmpty) extends Term
    @leaf class Return(expr: Term) extends Term
    @leaf class Throw(expr: Term) extends Term
    @leaf class Ascribe(expr: Term, typ: Type) extends Term
    @leaf class Annotate(expr: Term, annots: List[Annot] @nonEmpty) extends Term with Annottee
    @leaf class Tuple(elements: List[Term] @nonEmpty) extends Term
    @leaf class Block(stats: List[Stmt.Block]) extends Term
    @leaf class If(cond: Term, `then`: Term, `else`: Term) extends Term
    @leaf class Match(scrut: Term, cases: List[Case] @nonEmpty) extends Term
    @leaf class Try(expr: Term, `catch`: List[Case], `finally`: Option[Term]) extends Term
    @leaf class Function(params: List[Param.Function] @nonEmpty, body: Term) extends Term {
      require(params.length == 1 || params.forall(!_.annots.contains(Tree.Annot.Implicit)))
    }
    @leaf class PartialFunction(cases: List[Case] @nonEmpty) extends Term
    @leaf class While(expr: Term, body: Term) extends Term
    @leaf class Do(body: Term, expr: Term) extends Term
    @leaf class For(enums: List[Enumerator] @nonEmpty, body: Term) extends Term {
      require(enums.head.isInstanceOf[Enumerator.Generator])
    }
    @leaf class ForYield(enums: List[Enumerator] @nonEmpty, body: Term) extends Term
    @leaf class New(templ: Template) extends Term
  }

  @adt trait Pat extends Tree
  object Pat {
    @leaf class Wildcard() extends Pat
    @leaf class SequenceWildcard() extends Pat
    @leaf class Bind(lhs: Term.Ident, rhs: Pat) extends Pat
    @leaf class Alternative(lhs: Pat, rhs: Pat) extends Pat
    @leaf class Tuple(elements: List[Pat] @nonEmpty) extends Pat
    @leaf class Extractor(ref: Term.Ref, elements: List[Pat]) extends Pat {
      require(ref.isStableId)
    }
    @leaf class Interpolate(prefix: Term.Ident, parts: List[Term.String] @nonEmpty, args: List[Pat]) extends Pat {
      // TODO: also check that prefix is alphanumeric
      require(parts.length == args.length + 1)
    }
    @leaf class Typed(lhs: Pat, rhs: Type) extends Pat {
      require(lhs.isInstanceOf[Pat.Wildcard] || lhs.isInstanceOf[Term.Ident])
    }
  }

  @adt trait Type extends Tree
  object Type {
    @leaf class Ident(name: String) extends Type
    @leaf class Select(qual: Term.Ref, name: Type.Ident) extends Type {
      require(qual.isPath)
    }
    @leaf class SuperSelect(qual: Option[Term.Ident], supertyp: Option[Term.Ident], selector: Type.Ident) extends Type
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
    @leaf class Annotated(typ: Type, annots: List[Annot] @nonEmpty) extends Type with Annottee
  }

  @adt trait Decl extends Stmt.Template with Stmt.Refine with Annottee
  object Decl {
    @leaf class Val(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Type) extends Decl with Stmt.Existential
    @leaf class Var(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Type) extends Decl
    @leaf class Def(annots: List[Annot], name: Term.Ident, tparams: List[TypeParam.Def],
                         paramss: List[List[Param.Def]], implicits: List[Param.Def],
                         typ: Type) extends Decl
    @leaf class Type(annots: List[Annot], name: Tree.Type.Ident, tparams: List[TypeParam.Type],
                          bounds: TypeBounds) extends Decl with Stmt.Existential
  }

  @adt trait Defn extends Stmt.Template
  object Defn {
    @leaf class Val(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Option[Type], rhs: Term) extends Defn with Stmt.Block with Annottee
    @leaf class Var(annots: List[Annot], pats: List[Pat] @nonEmpty, typ: Option[Type], rhs: Term) extends Defn with Stmt.Block with Annottee
    @leaf class Def(annots: List[Annot], name: Term.Ident, tparams: List[TypeParam.Def],
                         paramss: List[List[Param.Def]], implicits: List[Param.Def],
                         typ: Option[Type], body: Term) extends Defn with Stmt.Block with Annottee
    @leaf class Macro(annots: List[Annot], name: Term.Ident, tparams: List[TypeParam.Def],
                           paramss: List[List[Param.Def]], implicits: List[Param.Def],
                           typ: Type, body: Term) extends Defn with Stmt.Block with Annottee
    @leaf class Type(annots: List[Annot], name: Tree.Type.Ident, tparams: List[TypeParam.Type],
                          body: Type) extends Defn with Stmt.Refine with Stmt.Block with Annottee
    @leaf class PrimaryCtor(annots: List[Annot], paramss: List[List[Param.Def]],
                                 implicits: List[Param.Def]) extends Defn with Annottee
    @leaf class SecondaryCtor(annots: List[Annot], paramss: List[List[Param.Def]],
                                   implicits: List[Param.Def], primaryCtorArgss: List[List[Term]]) extends Defn with Stmt.Block with Annottee
    @leaf class Class(annots: List[Annot], name: Tree.Type.Ident, tparams: List[TypeParam.Def],
                           ctor: PrimaryCtor, templ: Template) extends Defn with Stmt.TopLevel with Stmt.Block with Annottee
    @leaf class Trait(annots: List[Annot], name: Tree.Type.Ident, tparams: List[TypeParam.Type],
                           templ: Template) extends Defn with Stmt.TopLevel with Stmt.Block with Annottee {
      def isInterface: Boolean = templ.stats.forall(_.isInstanceOf[Decl])
    }
    @leaf class Object(annots: List[Annot], name: Term.Ident,
                            templ: Template) extends Defn with Stmt.TopLevel with Stmt.Block with Annottee
    @leaf class Package(ref: Term.Ref, body: List[Stmt.TopLevel]) extends Defn with Stmt.TopLevel
    @leaf class PackageObject(name: Term.Ident, templ: Template) extends Defn with Stmt.TopLevel
  }

  @leaf class Import(clauses: List[Import.Clause] @nonEmpty) extends Stmt.TopLevel with Stmt.Template with Stmt.Block
  object Import {
    @leaf class Clause(ref: Term.Ref, sels: List[Selector] @nonEmpty) extends Tree {
      require(ref.isStableId)
    }

    @adt trait Selector extends Tree
    object Selector {
      @leaf class Wildcard() extends Selector
      @leaf class Name(name: String) extends Selector
      @leaf class Rename(from: String, to: String) extends Selector
      @leaf class Unimport(name: String) extends Selector
    }
  }

  @adt trait Arg extends Tree
  object Arg {
    @leaf class Named(name: Term.Ident, arg: Term) extends Arg
    @leaf class Sequence(arg: Term) extends Arg
  }

  @leaf class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree

  @leaf class Template(early: List[Defn.Val], parents: List[Parent],
                            self: Self, stats: List[Stmt.Template]) extends Tree {
    require(parents.length == 0 || parents.tail.forall(_.argss.isEmpty))
  }

  @adt trait Enumerator extends Tree
  object Enumerator {
    @leaf class Generator(pat: Pat, rhs: Term) extends Enumerator
    @leaf class ValueDefinition(pat: Pat, rhs: Term) extends Enumerator
    @leaf class Guard(cond: Term) extends Enumerator
  }

  @leaf class Self(name: Option[Term.Ident], typ: Option[Type]) extends Tree

  @leaf class Parent(tpe: Type, argss: List[List[Term]]) extends Tree

  @leaf class TypeBounds(lo: Option[Type], hi: Option[Type]) extends Tree

  trait Param extends Tree with Annottee
  object Param {
    @leaf class Function(annots: List[Annot], name: Term.Ident, typ: Option[Type]) extends Param
    @leaf class Def(annots: List[Annot], name: Term.Ident, typ: Type, default: Option[Term]) extends Param
  }

  trait TypeParam extends Tree with Annottee
  object TypeParam {
    @leaf class Def(annots: List[Annot], name: Tree.Type.Ident,
                         tparams: List[TypeParam.Type],
                         contextBounds: List[Tree.Type],
                         viewBounds: List[Tree.Type],
                         bounds: TypeBounds) extends TypeParam
    @leaf class Type(annots: List[Annot], name: Tree.Type.Ident,
                          tparams: List[TypeParam.Type],
                          bounds: TypeBounds) extends TypeParam
  }

  trait Annottee extends Tree {
    def annots: List[Annot]
    // TODO: https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
    // 1) write a script that fetches this google doc and converts it into a, say, CSV spec
    // 2) write a test that validates the spec by generating source files and parsing them
    // 3) write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
    private[reflect] def validateAnnots(enclosing: Tree): Boolean = ???
  }
  @adt trait Annot extends Tree {
    // TODO: convert annotations to value objects (Liftable? Eval?)
  }
  object Annot {
    @adt trait Transient extends Annot // TODO: reserved for synthetic trees (e.g. resolved implicits) and attachments

    @adt trait Source extends Annot
    @leaf class UserDefined(tpe: Type, argss: List[List[Term]]) extends Source

    @adt trait Mod extends Source
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

    @adt trait Param extends Source
    @leaf class ByName() extends Param
    @leaf class VarArg() extends Param
    @leaf class Val() extends Param
    @leaf class Var() extends Param
  }
}
