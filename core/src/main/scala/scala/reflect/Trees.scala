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
    // ??? named and default args
    final case class Apply(fun: Term, args: List[Term]) extends Term
    final case class TypeApply(fun: Term, args: List[Type]) extends Term
    final case class Assign(lhs: Term.Ref, rhs: Term) extends Term
    // ??? List[List[Term]]. @xeno-by: scalac allows multiple argument lists for update
    final case class Update(expr: Term, args: List[Term]) extends Term
    final case class Return(expr: Term) extends Term
    final case class Throw(expr: Term) extends Term
    // ??? what about _*? @xeno-by: I suggest we have a separate tree for that
    final case class Ascribe(expr: Term, typ: Type) extends Term
    final case class Annotate(expr: Term, props: Props.Term) extends Term
    final case class Tuple(elements: List[Term]) extends Term
    // ??? blocks only allow Term, nested definitions and imports. how do we express that?
    final case class Block(stats: List[Tree]) extends Term
    final case class If(cond: Term, `then`: Term, `else`: Term) extends Term
    final case class Match(scrut: Term, cases: List[Case]) extends Term
    final case class Try(expr: Term, `catch`: List[Case], `finally`: Term) extends Term
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
    final case class Annotated(typ: Type, props: Props.Type)
  }

  sealed trait Defn extends Tree
  object Defn {
    sealed trait TopLevel extends Defn
    sealed trait Nested extends Defn

    // ??? AbstractVal + ConcreteVal + Val as a base class
    // @xeno-by: this makes sense, because that will allow us to specify precise type for Type.Existential
    // and also will rid ourselves of Term.Empty, but that would cause pattern matching problems.
    // that would also be consistent with AbstractType vs AliasType
    final case class Val(props: Props.Val, pats: List[Pat], typ: Type, rhs: Term) extends Nested

    // ??? var x = _
    final case class Var(props: Props.Var, pats: List[Pat], typ: Type, rhs: Term) extends Nested

    // ??? rename to Method?
    final case class Def(props: Props.Def, name: TermName, tparams: List[MethodTypeParam],
                         paramss: List[List[MethodParam]], implicits: List[MethodParam],
                         typ: Type, body: Term) extends Nested

    final case class Macro(props: Props.Macro, name: TermName, tparams: List[MethodTypeParam],
                           paramss: List[List[MethodParam]], implicits: List[MethodParam],
                           typ: Type, body: Term) extends Nested

    final case class AbstractType(props: Props.AbstractType, name: TypeName, tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Nested

    final case class AliasType(props: Props.AliasType, name: TypeName, tparams: List[TypeTypeParam],
                               body: Type) extends Nested

    final case class PrimaryCtor(props: Props.PrimaryCtor, paramss: List[List[ClassParam]],
                                 implicits: List[ClassParam]) extends Nested

    final case class SecondaryCtor(props: Props.SecondaryCtor, paramss: List[List[MethodParam]],
                                   implicits: List[MethodParam], primaryCtorArgss: List[List[Term]]) extends Nested

    final case class Class(props: Props.Class, name: TypeName, tparams: List[ClassTypeParam],
                           ctor: PrimaryCtor, templ: Template) extends TopLevel with Nested

    final case class Trait(props: Props.Trait, name: TypeName, tparams: List[TraitTypeParam],
                           templ: Template) extends TopLevel with Nested

    final case class Object(props: Props.Object, name: TermName,
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
  final case class MethodParam(props: Props.MethodParam, name: TermName, typ: Type, default: Term) extends Tree
  final case class ClassParam(props: Props.ClassParam, name: TermName, typ: Type, default: Term) extends Tree
  final case class MethodTypeParam(props: Props.MethodTypeParam, name: TypeName,
                                   tparams: List[TypeTypeParam],
                                   contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                   viewBounds: List[Type.Ref],
                                   bounds: TypeBounds) extends Tree
  final case class TypeTypeParam(props: Props.TypeTypeParam, name: TypeName,
                                 tparams: List[TypeTypeParam],
                                 bounds: TypeBounds) extends Tree // ??? @xeno-by: btw why not have context bounds for type type params?
  final case class TraitTypeParam(props: Props.TraitTypeParam, name: TypeName,
                                  tparams: List[TypeTypeParam],
                                  bounds: TypeBounds) extends Tree
  final case class ClassTypeParam(props: Props.ClassTypeParam, name: TypeName,
                                  tparams: List[TypeTypeParam],
                                  contextBounds: List[Type.Ref], // ??? @xeno-by: what's allowed in context bounds?
                                  viewBounds: List[Type.Ref],
                                  bounds: TypeBounds) extends Tree

  final case class Props[T <: Prop](all: List[T])
  object Props {
    type Term = Props[Prop.Term]
    object Term { def apply(data: Prop.Term*) = Props(data.toList) }
    type Type = Props[Prop.Type]
    object Type { def apply(data: Prop.Type*) = Props(data.toList) }
    type Val = Props[Prop.Val]
    object Val { def apply(data: Prop.Val*) = Props(data.toList) }
    type Var = Props[Prop.Var]
    object Var { def apply(data: Prop.Var*) = Props(data.toList) }
    type Def = Props[Prop.Def]
    object Def { def apply(data: Prop.Def*) = Props(data.toList) }
    type Macro = Props[Prop.Macro]
    object Macro { def apply(data: Prop.Macro*) = Props(data.toList) }
    type AbstractType = Props[Prop.AbstractType]
    object AbstractType { def apply(data: Prop.AbstractType*) = Props(data.toList) }
    type AliasType = Props[Prop.AliasType]
    object AliasType { def apply(data: Prop.AliasType*) = Props(data.toList) }
    type PrimaryCtor = Props[Prop.PrimaryCtor]
    object PrimaryCtor { def apply(data: Prop.PrimaryCtor*) = Props(data.toList) }
    type SecondaryCtor = Props[Prop.SecondaryCtor]
    object SecondaryCtor { def apply(data: Prop.SecondaryCtor*) = Props(data.toList) }
    type Class = Props[Prop.Class]
    object Class { def apply(data: Prop.Class*) = Props(data.toList) }
    type Trait = Props[Prop.Trait]
    object Trait { def apply(data: Prop.Trait*) = Props(data.toList) }
    type Object = Props[Prop.Object]
    object Object { def apply(data: Prop.Object*) = Props(data.toList) }
    type MethodParam = Props[Prop.MethodParam]
    object MethodParam { def apply(data: Prop.MethodParam*) = Props(data.toList) }
    type ClassParam = Props[Prop.ClassParam]
    object ClassParam { def apply(data: Prop.ClassParam*) = Props(data.toList) }
    type MethodTypeParam = Props[Prop.MethodTypeParam]
    object MethodTypeParam { def apply(data: Prop.MethodTypeParam*) = Props(data.toList) }
    type TypeTypeParam = Props[Prop.TypeTypeParam]
    object TypeTypeParam { def apply(data: Prop.TypeTypeParam*) = Props(data.toList) }
    type TraitTypeParam = Props[Prop.TraitTypeParam]
    object TraitTypeParam { def apply(data: Prop.TraitTypeParam*) = Props(data.toList) }
    type ClassTypeParam = Props[Prop.ClassTypeParam]
    object ClassTypeParam { def apply(data: Prop.ClassTypeParam*) = Props(data.toList) }
  }

  trait Prop
  object Prop {
    final case class Annot(name: Type.Ref, targs: List[Type], argss: List[List[Term]])
                     extends Tree with All
    final case class Private(within: Name) extends Prop with NestedDefn
    final case class PrivateThis() extends Prop with NestedDefn
    final case class Protected(within: Name) extends Prop with NestedDefn
    final case class ProtectedThis() extends Prop with NestedDefn
    final case class Implicit() extends Prop with Val with Var with Def with Macro with Object
    final case class Final() extends Prop with Val with Var with Def with Macro with AliasType with Class
    final case class Sealed() extends Prop with Class with Trait
    final case class Override() extends Prop with Val with Var with Def with Macro with AliasType with AbstractType with Object
    final case class Case() extends Prop with Class with Object
    // ??? @xeno-by: oopsie daisy, what do we do about AbstractType and Trait?!
    // final case class Abstract() extends Prop
    final case class Covariant() extends Prop with TypeTypeParam with TraitTypeParam with ClassTypeParam
    final case class Contravariant() extends Prop with TypeTypeParam with TraitTypeParam with ClassTypeParam
    // ??? @xeno-by: `abstract override' modifier only allowed for members of traits
    // also it's unclear what can be abstract override
    // final case class AbstractOverride() extends Prop
    final case class Lazy() extends Prop with Val
    final case class Synthetic() extends Prop with NestedDefn with Param
    final case class Artifact() extends Prop with NestedDefn with Param
    final case class Doc(doc: String) extends Prop with NestedDefn

    sealed trait Term extends Prop
    sealed trait Type extends Prop
    sealed trait Val extends Prop
    sealed trait Var extends Prop
    sealed trait Def extends Prop
    sealed trait Macro extends Prop
    sealed trait AbstractType extends Prop
    sealed trait AliasType extends Prop
    sealed trait PrimaryCtor extends Prop
    sealed trait SecondaryCtor extends Prop
    sealed trait Class extends Prop
    sealed trait Trait extends Prop
    sealed trait Object extends Prop
    sealed trait MethodParam extends Prop
    sealed trait ClassParam extends Prop
    sealed trait MethodTypeParam extends Prop
    sealed trait TypeTypeParam extends Prop
    sealed trait TraitTypeParam extends Prop
    sealed trait ClassTypeParam extends Prop
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
