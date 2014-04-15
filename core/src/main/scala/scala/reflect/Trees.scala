package scala.reflect

import Name._

sealed trait Tree {
  // ??? trivia: whitespace, comments, etc
  // http://msdn.microsoft.com/en-us/vstudio/hh500769
  // ??? history vs positions
  // ??? other stuff
  // https://docs.google.com/document/d/1CowAbpDOcyJyK8mECmEgel08lgmPcDCNnxAvcfudnf0/edit
}

object Tree {
  trait Stmt extends TopLevelStmt
  trait TopLevelStmt extends Tree

  sealed trait Term extends Arg with Stmt
  object Term {
    sealed trait Ref extends Term
    sealed trait Lit extends Term

    final case class Bool(value: scala.Bool) extends Lit
    final case class Int(value: scala.Int) extends Lit
    final case class Long(value: scala.Long) extends Lit
    final case class Float(value: scala.Float) extends Lit
    final case class Double(value: scala.Double) extends Lit
    final case class Char(value: scala.Char) extends Lit
    final case class String(value: Predef.String) extends Lit
    final case class Symbol(value: scala.Symbol) extends Lit
    final case class Null() extends Lit
    final case class Unit() extends Lit
    final case class Ident(name: TermName) extends Ref
    final case class Select(qual: Term, name: TermName) extends Ref
    final case class SuperSelect(qual: Option[TypeName], supertyp: Option[TypeName], selector: TermName) extends Ref
    final case class This(qual: TypeName) extends Term
    final case class Interpolate(prefix: TermName, parts: List[Term.String], args: List[Term]) extends Term
    final case class Apply(fun: Term, args: List[Arg]) extends Term
    final case class ApplyRight(arg: Term, fun: Term) extends Term
    final case class TypeApply(fun: Term, args: List[Type]) extends Term
    final case class Assign(lhs: Term.Ref, rhs: Term) extends Term
    final case class Update(expr: Term, args: List[List[Term]]) extends Term
    final case class Return(expr: Term) extends Term
    final case class Throw(expr: Term) extends Term
    // ??? what about _*? @xeno-by: I suggest we have a separate tree for that
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
    final case class Wildcard() extends Pat
    // ??? Ident vs Bind + Type
    // ??? Ascription
    // ??? :_* @xeno-by: again, I suggest we have a separate tree for varargs
    final case class Alt(left: Pat, right: Pat) extends Pat
    final case class Tuple(elements: List[Pat]) extends Pat
    final case class Extractor(ref: Term.Ref, elements: List[Pat]) extends Pat
  }

  sealed trait Type extends Tree
  object Type {
    sealed trait Ref extends Type
    final case class Ident(name: TypeName) extends Ref
    final case class Select(qual: Term.Ref, name: TypeName) extends Ref
    final case class Project(qual: Type.Ref, name: TypeName) extends Ref
    final case class Singleton(ref: Term.Ref)  extends Type
    final case class Constant(value: Term.Lit) extends Type
    // ??? final case class This
    // ??? final case class Super
    final case class TypeApply(typ: Type, targs: List[Type]) extends Type
    final case class Compound(parents: List[Type], defns: List[Defn.Nested]) extends Type
    // ??? needs sharper type for forSome. not just any defn, but just abstract val and abstract type
    final case class Existential(typ: Type, quants: List[Defn]) extends Type
    final case class Function(params: Type, res: Type) extends Type
    final case class Tuple(elements: List[Type]) extends Type
    final case class Annotated(typ: Type, annots: Annots.Type) extends Type
  }

  sealed trait Defn extends Stmt
  object Defn {
    // AbstractVal + ConcreteVal + TODO: base classes ???
    // @xeno-by: this makes sense, because that will allow us to specify precise type for Type.Existential
    // and also will rid ourselves of Term.Empty, but that would cause pattern matching problems.
    // that would also be consistent with AbstractType vs AliasType

    final case class AbstractVal(annots: Annots.AbstractVal, pats: List[Pat], typ: Type) extends Defn

    final case class Val(annots: Annots.Val, pats: List[Pat], typ: Option[Type], rhs: Term) extends Defn

    // ??? var x = _
    final case class AbstractVar(annots: Annots.AbstractVar, pats: List[Pat], typ: Type) extends Defn

    final case class Var(annots: Annots.Var, pats: List[Pat], typ: Option[Type], rhs: Term) extends Defn

    final case class AbstractDef(annots: Annots.AbstractDef, name: TermName, tparams: List[MethodTypeParam],
                                 paramss: List[List[MethodParam]], implicits: List[MethodParam],
                                 typ: Type) extends Defn

    final case class Def(annots: Annots.Def, name: TermName, tparams: List[MethodTypeParam],
                         paramss: List[List[MethodParam]], implicits: List[MethodParam],
                         typ: Option[Type], body: Term) extends Defn

    final case class Macro(annots: Annots.Macro, name: TermName, tparams: List[MethodTypeParam],
                           paramss: List[List[MethodParam]], implicits: List[MethodParam],
                           typ: Type, body: Term) extends Defn

    final case class AbstractType(annots: Annots.AbstractType, name: TypeName, tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Defn

    final case class AliasType(annots: Annots.AliasType, name: TypeName, tparams: List[TypeTypeParam],
                               body: Type) extends Defn

    final case class PrimaryCtor(annots: Annots.PrimaryCtor, paramss: List[List[ClassParam]],
                                 implicits: List[ClassParam]) extends Defn

    final case class SecondaryCtor(annots: Annots.SecondaryCtor, paramss: List[List[MethodParam]],
                                   implicits: List[MethodParam], primaryCtorArgss: List[List[Term]]) extends Defn

    final case class Class(annots: Annots.Class, name: TypeName, tparams: List[ClassTypeParam],
                           ctor: PrimaryCtor, templ: Template) extends Defn with TopLevelStmt

    final case class Trait(annots: Annots.Trait, name: TypeName, tparams: List[TraitTypeParam],
                           templ: Template) extends Defn with TopLevelStmt

    final case class Object(annots: Annots.Object, name: TermName,
                            templ: Template) extends Defn with TopLevelStmt

    final case class Package(ref: Term.Ref, body: List[TopLevelStmt]) extends Defn with TopLevelStmt
    final case class PackageObject(name: TermName, templ: Template) extends Defn with TopLevelStmt
  }

  final case class Import(clauses: List[Import.Clause]) extends TopLevelStmt
  object Import {
    final case class Clause(ref: Term.Ref, sels: List[Selector]) extends Tree

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
  final case class NamedArg(name: TermName, arg: Term) extends Arg

  final case class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree

  final case class Template(early: List[Defn.Val], parents: List[Parent],
                            self: Self, stats: List[Stmt]) extends Tree

  sealed trait Enumerator extends Tree
  object Enumerator {
    final case class Generator(pat: Pat, rhs: Term) extends Enumerator
    final case class ValueDefinition(pat: Pat, rhs: Term) extends Enumerator
    final case class Guard(cond: Term) extends Enumerator
  }

  final case class Self(name: TermName, typ: Option[Type]) extends Tree

  final case class Parent(name: Type.Ref, targs: List[Type], argss: List[List[Term]]) extends Tree

  final case class TypeBounds(lo: Option[Type], hi: Option[Type]) extends Tree

  final case class FunctionParam(name: TermName, typ: Option[Type]) extends Tree
  final case class MethodParam(annots: Annots.MethodParam, name: TermName, typ: Type, default: Term) extends Tree
  final case class ClassParam(annots: Annots.ClassParam, name: TermName, typ: Type, default: Term) extends Tree
  final case class MethodTypeParam(annots: Annots.MethodTypeParam, name: TypeName,
                                   tparams: List[TypeTypeParam],
                                   contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                   viewBounds: List[Type.Ref],
                                   bounds: TypeBounds) extends Tree
  final case class TypeTypeParam(annots: Annots.TypeTypeParam, name: TypeName,
                                 tparams: List[TypeTypeParam],
                                 bounds: TypeBounds) extends Tree // ??? @xeno-by: btw why not have context bounds for type type params?
  final case class TraitTypeParam(annots: Annots.TraitTypeParam, name: TypeName,
                                  tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Tree
  final case class ClassTypeParam(annots: Annots.ClassTypeParam, name: TypeName,
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
    final case class Private(within: Name) extends Mod with NestedDefn
    final case class Protected(within: Name) extends Mod with NestedDefn
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
