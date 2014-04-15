package scala.reflect

sealed trait Tree {
  // ??? trivia: whitespace, comments, etc
  // http://msdn.microsoft.com/en-us/vstudio/hh500769
  // ??? history vs positions
  // ??? other stuff
  // https://docs.google.com/document/d/1CowAbpDOcyJyK8mECmEgel08lgmPcDCNnxAvcfudnf0/edit
}

object Tree {
  sealed trait Stmt extends TopLevelStmt with RefineStmt
  sealed trait TopLevelStmt extends Tree
  sealed trait RefineStmt extends Tree

  sealed trait Term extends Arg with Stmt
  object Term {
    sealed trait Ref extends Term
    sealed trait Path extends Ref
    sealed trait StableId extends Path
    final case class This(qual: Option[Ident]) extends Path
    final case class Ident(name: String) extends StableId with Pat.TypedLhs { def isBackquoted = ??? }
    trait Select { def qual: Term; def selector: Term.Ident }
    object Select { def unapply(sel: Select): Option[(Term, Term.Ident)] = Some((sel.qual, sel.selector)) }
    final case class SuperSelect(qual: Option[Term.Ident], supertyp: Option[Term.Ident], selector: Term.Ident) extends StableId
    final case class StableSelect(qual: Path, selector: Term.Ident) extends StableId with Select
    final case class UnstableSelect(qual: Term, selector: Term.Ident) extends Select with Ref

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
    final case class Assign(lhs: Term.Ref, rhs: Term) extends Term // TODO: limit lhs to select from simple exprs?
    final case class Update(expr: Term, args: List[List[Term]]) extends Term
    final case class Return(expr: Term) extends Term
    final case class Throw(expr: Term) extends Term
    final case class Ascribe(expr: Term, typ: Type) extends Term
    final case class Annotate(expr: Term, annots: Annots.Term) extends Term
    final case class Tuple(elements: List[Term]) extends Term
    final case class Block(stats: List[Stmt]) extends Term
    final case class If(cond: Term, `then`: Term, `else`: Term) extends Term
    final case class Match(scrut: Term, cases: List[Case]) extends Term
    final case class Try(expr: Term, `catch`: List[Case], `finally`: Option[Term]) extends Term
    final case class Function(params: List[FunctionParam], body: Term) extends Term
    final case class PartialFunction(cases: List[Case]) extends Term
    final case class While(expr: Term, body: Term) extends Term
    final case class Do(body: Term, expr: Term) extends Term
    final case class For(enums: List[Enumerator], body: Term) extends Term
    final case class ForYield(enums: List[Enumerator], body: Term) extends Term
    final case class New(templ: Template) extends Term
  }

  sealed trait Pat extends Tree
  object Pat {
    sealed trait TypedLhs extends Pat
    final case class Wildcard() extends TypedLhs
    final case class SequenceWildcard() extends Pat
    final case class Bind(lhs: Term.Ident, rhs: Pat) extends Pat // wishful thinking AND
    final case class Alt(lhs: Pat, rhs: Pat) extends Pat // wishful thinking OR
    final case class Tuple(elements: List[Pat]) extends Pat
    final case class Extractor(ref: Term.StableId, elements: List[Pat]) extends Pat
    // final case class Guard(pat: Pat, cond: Term) extends Pat // wishful thinking
    final case class Interpolate(prefix: Term.Ident, parts: List[Term.String], args: List[Pat]) extends Pat
    final case class Typed(lhs: TypedLhs, rhs: Type) extends Pat
  }

  sealed trait Type extends Tree
  object Type {
    sealed trait Simple extends Type
    sealed trait Ref extends Simple
    final case class Ident(name: String) extends Ref
    final case class Select(qual: Term.Path, name: Type.Ident) extends Ref
    final case class SuperSelect(qual: Option[Term.Ident], supertyp: Option[Term.Ident], selector: Type.Ident) extends Ref
    final case class Project(qual: Type.Simple, name: Type.Ident) extends Ref
    final case class Singleton(ref: Term.Path)  extends Simple
    final case class Constant(value: Term.Lit) extends Type
    final case class This(qual: Ident) extends Ref
    final case class Apply(typ: Type, targs: List[Type]) extends Simple
    final case class Compound(parents: List[Type], stmts: List[RefineStmt]) extends Type
    // ??? needs sharper type for forSome. not just any defn, but just abstract val and abstract type
    final case class Existential(typ: Type, quants: List[Defn]) extends Type
    final case class Function(params: Type, res: Type) extends Type
    final case class Tuple(elements: List[Type]) extends Simple
    final case class Annotated(typ: Type, annots: Annots.Type) extends Type
  }

  sealed trait Defn extends Stmt
  object Defn {
    sealed trait Abstract extends Defn

    // AbstractVal + ConcreteVal + TODO: base classes ???
    // @xeno-by: this makes sense, because that will allow us to specify precise type for Type.Existential
    // and also will rid ourselves of Term.Empty, but that would cause pattern matching problems.
    // that would also be consistent with AbstractType vs AliasType

    final case class AbstractVal(annots: Annots.AbstractVal, pats: List[Pat], typ: Type) extends Abstract with RefineStmt

    final case class Val(annots: Annots.Val, pats: List[Pat], typ: Option[Type], rhs: Term) extends Defn

    // ??? var x = _
    final case class AbstractVar(annots: Annots.AbstractVar, pats: List[Pat], typ: Type) extends Abstract with RefineStmt

    final case class Var(annots: Annots.Var, pats: List[Pat], typ: Option[Type], rhs: Term) extends Defn

    final case class AbstractDef(annots: Annots.AbstractDef, name: Term.Ident, tparams: List[MethodTypeParam],
                                 paramss: List[List[MethodParam]], implicits: List[MethodParam],
                                 typ: Type) extends Abstract with RefineStmt

    final case class Def(annots: Annots.Def, name: Term.Ident, tparams: List[MethodTypeParam],
                         paramss: List[List[MethodParam]], implicits: List[MethodParam],
                         typ: Option[Type], body: Term) extends Defn

    final case class Macro(annots: Annots.Macro, name: Term.Ident, tparams: List[MethodTypeParam],
                           paramss: List[List[MethodParam]], implicits: List[MethodParam],
                           typ: Type, body: Term) extends Defn

    final case class AbstractType(annots: Annots.AbstractType, name: Type.Ident, tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Defn with RefineStmt

    final case class AliasType(annots: Annots.AliasType, name: Type.Ident, tparams: List[TypeTypeParam],
                               body: Type) extends Defn with RefineStmt

    final case class PrimaryCtor(annots: Annots.PrimaryCtor, paramss: List[List[ClassParam]],
                                 implicits: List[ClassParam]) extends Defn

    final case class SecondaryCtor(annots: Annots.SecondaryCtor, paramss: List[List[MethodParam]],
                                   implicits: List[MethodParam], primaryCtorArgss: List[List[Term]]) extends Defn

    final case class Class(annots: Annots.Class, name: Type.Ident, tparams: List[ClassTypeParam],
                           ctor: PrimaryCtor, templ: Template) extends Defn with TopLevelStmt

    final case class Trait(annots: Annots.Trait, name: Type.Ident, tparams: List[TraitTypeParam],
                           templ: Template) extends Defn with TopLevelStmt

    final case class Object(annots: Annots.Object, name: Term.Ident,
                            templ: Template) extends Defn with TopLevelStmt

    // TODO: simple path?
    final case class Package(ref: Term.Path, body: List[TopLevelStmt]) extends Defn with TopLevelStmt
    final case class PackageObject(name: Term.Ident, templ: Template) extends Defn with TopLevelStmt
  }

  final case class Import(clauses: List[Import.Clause]) extends TopLevelStmt
  object Import {
    final case class Clause(ref: Term.StableId, sels: List[Selector]) extends Tree

    sealed trait Selector extends Tree
    object Selector {
      // ??? are we happy with having these as strings?
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
                            self: Self, stats: List[Stmt]) extends Tree

  sealed trait Enumerator extends Tree
  object Enumerator {
    final case class Generator(pat: Pat, rhs: Term) extends Enumerator
    final case class ValueDefinition(pat: Pat, rhs: Term) extends Enumerator
    final case class Guard(cond: Term) extends Enumerator
  }

  final case class Self(name: Term.Ident, typ: Option[Type]) extends Tree

  final case class Parent(name: Type.Ref, targs: List[Type], argss: List[List[Term]]) extends Tree

  final case class TypeBounds(lo: Option[Type], hi: Option[Type]) extends Tree

  final case class FunctionParam(name: Term.Ident, typ: Option[Type]) extends Tree
  final case class MethodParam(annots: Annots.MethodParam, name: Term.Ident, typ: Type, default: Term) extends Tree
  final case class ClassParam(annots: Annots.ClassParam, name: Term.Ident, typ: Type, default: Term) extends Tree
  final case class MethodTypeParam(annots: Annots.MethodTypeParam, name: Type.Ident,
                                   tparams: List[TypeTypeParam],
                                   contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                   viewBounds: List[Type.Ref],
                                   bounds: TypeBounds) extends Tree
  final case class TypeTypeParam(annots: Annots.TypeTypeParam, name: Type.Ident,
                                 tparams: List[TypeTypeParam],
                                 bounds: TypeBounds) extends Tree // ??? @xeno-by: btw why not have context bounds for type type params?
  final case class TraitTypeParam(annots: Annots.TraitTypeParam, name: Type.Ident,
                                  tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Tree
  final case class ClassTypeParam(annots: Annots.ClassTypeParam, name: Type.Ident,
                                  tparams: List[TypeTypeParam],
                                  contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                  viewBounds: List[Type.Ref],
                                  bounds: TypeBounds) extends Tree

  sealed trait Annots[T <: Annot] { def annots: List[T] }
  object Annots {
    final case class Term(annots: List[Annot.Term]) extends Annots[Annot.Term]
    final case class Type(annots: List[Annot.Type]) extends Annots[Annot.Type]
    final case class AbstractVal(annots: List[Annot.AbstractVal]) extends Annots[Annot.AbstractVal]
    final case class Val(annots: List[Annot.Val]) extends Annots[Annot.Val]
    final case class AbstractVar(annots: List[Annot.AbstractVar]) extends Annots[Annot.AbstractVar]
    final case class Var(annots: List[Annot.Var]) extends Annots[Annot.Var]
    final case class AbstractDef(annots: List[Annot.AbstractDef]) extends Annots[Annot.AbstractDef]
    final case class Def(annots: List[Annot.Def]) extends Annots[Annot.Def]
    final case class Macro(annots: List[Annot.Macro]) extends Annots[Annot.Macro]
    final case class AbstractType(annots: List[Annot.AbstractType]) extends Annots[Annot.AbstractType]
    final case class AliasType(annots: List[Annot.AliasType]) extends Annots[Annot.AliasType]
    final case class PrimaryCtor(annots: List[Annot.PrimaryCtor]) extends Annots[Annot.PrimaryCtor]
    final case class SecondaryCtor(annots: List[Annot.SecondaryCtor]) extends Annots[Annot.SecondaryCtor]
    final case class Class(annots: List[Annot.Class]) extends Annots[Annot.Class]
    final case class Trait(annots: List[Annot.Trait]) extends Annots[Annot.Trait]
    final case class Object(annots: List[Annot.Object]) extends Annots[Annot.Object]
    final case class MethodParam(annots: List[Annot.MethodParam]) extends Annots[Annot.MethodParam]
    final case class ClassParam(annots: List[Annot.ClassParam]) extends Annots[Annot.ClassParam]
    final case class MethodTypeParam(annots: List[Annot.MethodTypeParam]) extends Annots[Annot.MethodTypeParam]
    final case class TypeTypeParam(annots: List[Annot.TypeTypeParam]) extends Annots[Annot.TypeTypeParam]
    final case class TraitTypeParam(annots: List[Annot.TraitTypeParam]) extends Annots[Annot.TraitTypeParam]
    final case class ClassTypeParam(annots: List[Annot.ClassTypeParam]) extends Annots[Annot.ClassTypeParam]
  }

  sealed trait Annot extends Tree
  object Annot {
    sealed trait Transient extends Annot // TODO: reserved for synthetic trees (e.g. resolved implicits) and attachments
    sealed trait Source extends Annot
    sealed trait Mod extends Source

    final case class UserDefined(name: Type.Ref, targs: List[Type], argss: List[List[Term]])
                     extends Source with All
    final case class Private(within: String) extends Mod with NestedDefn
    final case class Protected(within: String) extends Mod with NestedDefn
    final case class Implicit() extends Mod with Val with AbstractVal
                                            with Var with AbstractVar
                                            with Def with AbstractDef
                                            with Macro with Object
    final case class Final() extends Mod with Val with Var with Def with Macro with AliasType with Class
    final case class Sealed() extends Mod with Class with Trait
    final case class Override() extends Mod with Val with Var with Def with Macro with AliasType with AbstractType with Object
    final case class Case() extends Mod with Class with Object
    final case class Abstract() extends Mod with Class
    final case class Covariant() extends Mod with TypeTypeParam with TraitTypeParam with ClassTypeParam
    final case class Contravariant() extends Mod with TypeTypeParam with TraitTypeParam with ClassTypeParam
    // ??? @xeno-by: `abstract override' modifier only allowed for members of traits
    // also it's unclear what can be abstract override
    // final case class AbstractOverride() extends Mod
    final case class Lazy() extends Mod with Val
    final case class Doc(doc: String) extends Mod with NestedDefn

    sealed trait Term extends Source
    sealed trait Type extends Source
    sealed trait AbstractVal extends Source
    sealed trait Val extends Source
    sealed trait AbstractVar extends Source
    sealed trait Var extends Source
    sealed trait AbstractDef extends Source
    sealed trait Def extends Source
    sealed trait Macro extends Source
    sealed trait AbstractType extends Source
    sealed trait AliasType extends Source
    sealed trait PrimaryCtor extends Source
    sealed trait SecondaryCtor extends Source
    sealed trait Class extends Source
    sealed trait Trait extends Source
    sealed trait Object extends Source
    sealed trait MethodParam extends Source
    sealed trait ClassParam extends Source
    sealed trait MethodTypeParam extends Source
    sealed trait TypeTypeParam extends Source
    sealed trait TraitTypeParam extends Source
    sealed trait ClassTypeParam extends Source
    sealed trait NestedDefn extends Val with Var with Def with Macro
                            with AbstractType with AliasType
                            with PrimaryCtor with SecondaryCtor
                            with Class with Trait with Object
                            with ClassParam
    sealed trait Param extends MethodParam with ClassParam with MethodTypeParam
                       with TypeTypeParam with TraitTypeParam with ClassTypeParam
    sealed trait All extends Term with Type with NestedDefn with Param
  }
}
