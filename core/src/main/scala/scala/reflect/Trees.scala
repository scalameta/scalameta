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
  // ??? @xeno-by: do we really need to have this trait
  sealed trait Ref extends Tree

  sealed trait Term extends Tree
  object Term {
    // ??? bad practice: this violates safety by construction
    // currently used in val, var, def, try, case guard
    final case class Empty() extends Term
    sealed trait Ref extends Term with Tree.Ref
    sealed trait Lit extends Term

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
    // ??? see below
    final case class Select(qual: Term.Ref, name: TermName) extends Ref
    // ??? qual and supertyp can be empty. @xeno-by: what do you mean qual can be empty?
    final case class SuperSelect(qual: TypeName, supertyp: TypeName, selector: TermName) extends Ref
    final case class This(qual: TypeName) extends Term
    final case class Apply(fun: Term, args: List[Term]) extends Term
    final case class TypeApply(fun: Term, args: List[Type]) extends Term
    final case class Assign(lhs: Term.Ref, rhs: Term) extends Term
    // ??? List[List[Term]]. @xeno-by: scalac allows multiple argument lists for update
    final case class Update(expr: Term, args: List[Term]) extends Term
    final case class Return(expr: Term) extends Term
    final case class Throw(expr: Term) extends Term
    // ??? what about _*? @xeno-by: I suggest we have a separate tree for that
    final case class Ascribe(expr: Term, typ: Type) extends Term
    final case class Annotate(expr: Term, meta: Meta) extends Term
    final case class Tuple(elements: List[Term]) extends Term
    // ??? blocks only allow Term, nested definitions and imports. how do we express that?
    final case class Block(stats: List[Tree]) extends Term
    final case class If(cond: Term, `then`: Term, `else`: Term) extends Term
    final case class Match(scrut: Term, cases: List[Case]) extends Term
    final case class Try(expr: Term, `catch`: List[Case], `finally`: Term) extends Term
    final case class Function(params: List[FunctionParam], body: Term) extends Term
    final case class PartialFunction(cases: List[Case]) extends Term
    final case class While(expr: Term, body: Term) extends Term
    final case class DoWhile(body: Term, expr: Term) extends Term
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
    // ??? bad practice: this violates safety by construction
    // currently used in vals, defs, bounds, self type, function params
    final case class Empty() extends Type
    sealed trait Ref extends Type with Tree.Ref

    final case class Ident(name: TypeName) extends Type with Ref
    final case class Select(qual: Tree.Ref, name: TypeName) extends Ref
    // ??? needs thought about Select vs Project and kind of ref
    // @xeno-by: I suggest we separate Select and Project by Term.Ref and Type.Ref
    // however Denys says this might cause problems with pattern matching

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
    final case class Annotated(typ: Type, meta: Meta)
  }

  sealed trait Defn extends Tree
  object Defn {
    sealed trait TopLevel extends Defn
    sealed trait Nested extends Defn

    // ??? AbstractVal + ConcreteVal + Val as a base class
    // @xeno-by: this makes sense, because that will allow us to specify precise type for Type.Existential
    // and also will rid ourselves of Term.Empty, but that would cause pattern matching problems.
    // that would also be consistent with AbstractType vs AliasType
    final case class Val(meta: Meta, pats: List[Pat], typ: Type, rhs: Term) extends Nested

    // ??? var x = _
    final case class Var(meta: Meta, pats: List[Pat], typ: Type, rhs: Term) extends Nested

    // ??? rename to Method?
    final case class Def(meta: Meta, name: TermName, tparams: List[MethodTypeParam],
                         paramss: List[List[MethodParam]], implicits: List[MethodParam],
                         typ: Type, body: Term) extends Nested

    final case class Macro(meta: Meta, name: TermName, tparams: List[MethodTypeParam],
                           paramss: List[List[MethodParam]], implicits: List[MethodParam],
                           typ: Type, body: Term) extends Nested

    final case class AbstractType(meta: Meta, name: TypeName, tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Nested

    final case class AliasType(meta: Meta, name: TypeName, tparams: List[TypeTypeParam],
                               body: Type) extends Nested

    final case class PrimaryCtor(meta: Meta, paramss: List[List[ClassParam]],
                                 implicits: List[ClassParam]) extends Nested

    final case class SecondaryCtor(meta: Meta, paramss: List[List[MethodParam]],
                                   implicits: List[MethodParam], primaryCtorArgss: List[List[Term]]) extends Nested

    final case class Class(meta: Meta, name: TypeName, tparams: List[ClassTypeParam],
                           ctor: PrimaryCtor, templ: Template) extends TopLevel with Nested

    final case class Trait(meta: Meta, name: TypeName, tparams: List[TraitTypeParam],
                           templ: Template) extends TopLevel with Nested

    final case class Object(meta: Meta, name: TermName,
                            templ: Template) extends TopLevel with Nested

    final case class Package(ref: Term.Ref, body: List[TopLevel]) extends TopLevel
    final case class PackageObject(name: TermName, templ: Template) extends TopLevel
  }

  final case class Import(clauses: List[Import.Clause]) extends Tree
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

  final case class Case(pat: Pat, cond: Term, body: Term) extends Tree

  // ??? sharpen the type of stats (only terms and nested defns are allowed)
  final case class Template(early: List[Defn.Val], parents: List[Parent],
                            self: Self, stats: List[Tree]) extends Tree

  sealed trait Enumerator extends Tree
  object Enumerator {
    final case class Generator(pat: Pat, rhs: Term) extends Enumerator
    final case class ValueDefinition(pat: Pat, rhs: Term) extends Enumerator
    final case class Guard(cond: Term) extends Enumerator
  }

  final case class Self(name: TermName, typ: Type) extends Tree

  final case class Parent(name: Type.Ref, targs: List[Type], argss: List[List[Term]]) extends Tree

  final case class TypeBounds(lo: Type, hi: Type) extends Tree

  final case class FunctionParam(name: TermName, typ: Type) extends Tree
  final case class MethodParam(meta: Meta, name: TermName, typ: Type, default: Term) extends Tree
  final case class ClassParam(meta: Meta, name: TermName, typ: Type, default: Term) extends Tree
  final case class MethodTypeParam(meta: Meta, name: TypeName,
                                   tparams: List[TypeTypeParam],
                                   contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                   viewBounds: List[Type.Ref],
                                   bounds: TypeBounds) extends Tree
  final case class TypeTypeParam(meta: Meta, name: TypeName,
                                 tparams: List[TypeTypeParam],
                                 bounds: TypeBounds) extends Tree // ??? @xeno-by: btw why not have context bounds for type type params?
  final case class TraitTypeParam(meta: Meta, name: TypeName,
                                  tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Tree
  final case class ClassTypeParam(meta: Meta, name: TypeName,
                                  tparams: List[TypeTypeParam],
                                  contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                  viewBounds: List[Type.Ref],
                                  bounds: TypeBounds) extends Tree

  // ??? @xeno-by: to be implemented
  trait Meta
}
