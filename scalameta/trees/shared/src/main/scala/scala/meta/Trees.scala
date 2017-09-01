package scala.meta

import org.scalameta.invariants._
import scala.meta.classifiers._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.prettyprinters._
import scala.meta.internal.trees._

@root trait Tree extends InternalTree {
  def parent: Option[Tree]
  def children: List[Tree]

  def pos: Position
  def tokens(implicit dialect: Dialect): Tokens

  final override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  final override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  final override def hashCode: Int = System.identityHashCode(this)
  final override def toString = scala.meta.internal.prettyprinters.TreeToString(this)
}

object Tree extends InternalTreeXtensions {
  implicit def classifiable[T <: Tree]: Classifiable[T] = null
  implicit def showStructure[T <: Tree]: Structure[T] = scala.meta.internal.prettyprinters.TreeStructure.apply[T]
  implicit def showSyntax[T <: Tree](implicit dialect: Dialect): Syntax[T] = scala.meta.internal.prettyprinters.TreeSyntax.apply[T](dialect)
}

@branch trait Ref extends Tree
@branch trait Stat extends Tree

@branch trait Name extends Ref { def value: String }
object Name {
  def apply(value: String): Name = if (value == "") Name.Anonymous() else Name.Indeterminate(value)
  def unapply(name: Name): Option[String] = Some(name.value)
  @ast class Anonymous() extends Name {
    def value = ""
    checkParent(ParentChecks.NameAnonymous)
  }
  @ast class Indeterminate(value: Predef.String @nonEmpty) extends Name
}

@branch trait Lit extends Term with Pat with Type {
  def value: Any
}
object Lit {
  def unapply(arg: Lit): Option[Any] = Some(arg.value)
  @ast class Null() extends Lit { def value: Any = null }
  @ast class Int(value: scala.Int) extends Lit
  // NOTE: Lit.Double/Float are strings to work the same across JS/JVM. Example:
  // 1.4f.toString == "1.399999976158142" // in JS
  // 1.4f.toString == "1.4"               // in JVM
  // See https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
  @ast class Double(format: scala.Predef.String) extends Lit { val value = format.toDouble }
  object Double { def apply(double: scala.Double): Double = Lit.Double(double.toString)  }
  @ast class Float(format: scala.Predef.String) extends Lit { val value = format.toFloat }
  object Float { def apply(float: scala.Float): Float = Lit.Float(float.toString)  }
  @ast class Byte(value: scala.Byte) extends Lit
  @ast class Short(value: scala.Short) extends Lit
  @ast class Char(value: scala.Char) extends Lit
  @ast class Long(value: scala.Long) extends Lit
  @ast class Boolean(value: scala.Boolean) extends Lit
  @ast class Unit() extends Lit { def value: Any = () }
  @ast class String(value: scala.Predef.String) extends Lit
  @ast class Symbol(value: scala.Symbol) extends Lit
}

@branch trait Term extends Stat
object Term {
  @branch trait Ref extends Term with scala.meta.Ref
  @ast class This(qual: scala.meta.Name) extends Term.Ref
  @ast class Super(thisp: scala.meta.Name, superp: scala.meta.Name) extends Term.Ref
  @ast class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Term.Ref with Pat
  @ast class Select(qual: Term, name: Term.Name) extends Term.Ref with Pat
  @ast class Interpolate(prefix: Name, parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Apply(fun: Term, args: List[Term]) extends Term
  @ast class ApplyType(fun: Term, targs: List[Type] @nonEmpty) extends Term
  @ast class ApplyInfix(lhs: Term, op: Name, targs: List[Type], args: List[Term]) extends Term
  @ast class ApplyUnary(op: Name, arg: Term) extends Term.Ref {
    checkFields(op.isUnaryOp)
  }
  @ast class Assign(lhs: Term, rhs: Term) extends Term {
    checkFields(lhs.is[Term.Quasi] || lhs.is[Term.Ref] || lhs.is[Term.Apply])
    checkParent(ParentChecks.TermAssign)
  }
  @ast class Return(expr: Term) extends Term
  @ast class Throw(expr: Term) extends Term
  @ast class Ascribe(expr: Term, tpe: Type) extends Term
  @ast class Annotate(expr: Term, annots: List[Mod.Annot] @nonEmpty) extends Term
  @ast class Tuple(args: List[Term] @nonEmpty) extends Term {
    // tuple must have more than one element
    // however, this element may be Quasi with "hidden" list of elements inside
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Term.Quasi]))
  }
  @ast class Block(stats: List[Stat]) extends Term {
    checkFields(stats.forall(_.isBlockStat))
  }
  @ast class If(cond: Term, thenp: Term, elsep: Term) extends Term
  @ast class Match(expr: Term, cases: List[Case] @nonEmpty) extends Term
  @ast class Try(expr: Term, catchp: List[Case], finallyp: Option[Term]) extends Term
  @ast class TryWithHandler(expr: Term, catchp: Term, finallyp: Option[Term]) extends Term
  @ast class Function(params: List[Term.Param], body: Term) extends Term {
    checkFields(params.forall(param => param.is[Term.Param.Quasi] || (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)))
    checkFields(params.exists(_.is[Term.Param.Quasi]) || params.exists(_.mods.exists(_.is[Mod.Implicit])) ==> (params.length == 1))
  }
  @ast class PartialFunction(cases: List[Case] @nonEmpty) extends Term
  @ast class While(expr: Term, body: Term) extends Term
  @ast class Do(body: Term, expr: Term) extends Term
  @ast class For(enums: List[Enumerator] @nonEmpty, body: Term) extends Term {
    checkFields(enums.head.is[Enumerator.Generator] || enums.head.is[Enumerator.Quasi])
  }
  @ast class ForYield(enums: List[Enumerator] @nonEmpty, body: Term) extends Term
  @ast class New(init: Init) extends Term
  @ast class NewAnonymous(templ: Template) extends Term
  @ast class Placeholder() extends Term
  @ast class Eta(expr: Term) extends Term
  @ast class Repeated(expr: Term) extends Term {
    checkParent(ParentChecks.TermRepeated)
  }
  @ast class Param(mods: List[Mod], name: meta.Name, decltpe: Option[Type], default: Option[Term]) extends Member
  def fresh(): Term.Name = fresh("fresh")
  def fresh(prefix: String): Term.Name = Term.Name(prefix + Fresh.nextId())
}

@branch trait Type extends Tree
object Type {
  @branch trait Ref extends Type with scala.meta.Ref
  @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Type.Ref
  @ast class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref {
    checkFields(qual.isPath || qual.is[Term.Super] || qual.is[Term.Ref.Quasi])
  }
  @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
  @ast class Singleton(ref: Term.Ref) extends Type.Ref {
    checkFields(ref.isPath || ref.is[Term.Super])
  }
  @ast class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
  @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
  @ast class Function(params: List[Type], res: Type) extends Type
  @ast class ImplicitFunction(params: List[Type], res: Type) extends Type
  @ast class Tuple(args: List[Type] @nonEmpty) extends Type {
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type.Quasi]))
  }
  @ast class With(lhs: Type, rhs: Type) extends Type
  @ast class And(lhs: Type, rhs: Type) extends Type
  @ast class Or(lhs: Type, rhs: Type) extends Type
  @ast class Refine(tpe: Option[Type], stats: List[Stat]) extends Type {
    checkFields(stats.forall(_.isRefineStat))
  }
  @ast class Existential(tpe: Type, stats: List[Stat] @nonEmpty) extends Type {
    checkFields(stats.forall(_.isExistentialStat))
  }
  @ast class Annotate(tpe: Type, annots: List[Mod.Annot] @nonEmpty) extends Type
  @ast class Lambda(tparams: List[Type.Param], tpe: Type) extends Type {
    checkParent(ParentChecks.TypeLambda)
  }
  @ast class Method(paramss: List[List[Term.Param]], tpe: Type) extends Type {
    checkParent(ParentChecks.TypeMethod)
  }
  @ast class Placeholder(bounds: Bounds) extends Type
  @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
  @ast class ByName(tpe: Type) extends Type {
    checkParent(ParentChecks.TypeByName)
  }
  @ast class Repeated(tpe: Type) extends Type {
    checkParent(ParentChecks.TypeRepeated)
  }
  @ast class Var(name: Name) extends Type with Member.Type {
    checkFields(name.value(0).isLower)
    checkParent(ParentChecks.TypeVar)
  }
  @ast class Param(mods: List[Mod],
                   name: meta.Name,
                   tparams: List[Type.Param],
                   tbounds: Type.Bounds,
                   vbounds: List[Type],
                   cbounds: List[Type]) extends Member
  def fresh(): Type.Name = fresh("fresh")
  def fresh(prefix: String): Type.Name = Type.Name(prefix + Fresh.nextId())
}

@branch trait Pat extends Tree
object Pat {
  @ast class Var(name: scala.meta.Term.Name) extends Pat with Member.Term {
    // NOTE: can't do this check here because of things like `val X = 2`
    // checkFields(name.value(0).isLower)
    checkParent(ParentChecks.PatVar)
  }
  @ast class Wildcard() extends Pat
  @ast class SeqWildcard() extends Pat {
    checkParent(ParentChecks.PatSeqWildcard)
  }
  @ast class Bind(lhs: Pat, rhs: Pat) extends Pat {
    checkFields(lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
  }
  @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
  @ast class Tuple(args: List[Pat] @nonEmpty) extends Pat {
    checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Quasi]))
  }
  @ast class Extract(fun: Term, args: List[Pat]) extends Pat {
    checkFields(fun.isExtractor)
  }
  @ast class ExtractInfix(lhs: Pat, op: Term.Name, rhs: List[Pat]) extends Pat
  @ast class Interpolate(prefix: Term.Name, parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
    checkFields(parts.length == args.length + 1)
  }
  @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
    checkFields(lhs.is[Pat.Wildcard] || lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
    checkFields(!rhs.is[Type.Var] && !rhs.is[Type.Placeholder])
  }
  def fresh(): Pat.Var = Pat.Var(Term.fresh())
  def fresh(prefix: String): Pat.Var = Pat.Var(Term.fresh(prefix))
}

@branch trait Member extends Tree {
  def name: Name
}
object Member {
  @branch trait Term extends Member {
    def name: scala.meta.Term.Name
  }
  @branch trait Type extends Member {
    def name: scala.meta.Type.Name
  }
}

@branch trait Decl extends Stat
object Decl {
  @ast class Val(mods: List[Mod],
                 pats: List[Pat] @nonEmpty,
                 decltpe: scala.meta.Type) extends Decl
  @ast class Var(mods: List[Mod],
                 pats: List[Pat] @nonEmpty,
                 decltpe: scala.meta.Type) extends Decl
  @ast class Def(mods: List[Mod],
                 name: Term.Name,
                 tparams: List[scala.meta.Type.Param],
                 paramss: List[List[Term.Param]],
                 decltpe: scala.meta.Type) extends Decl with Member.Term
  @ast class Type(mods: List[Mod],
                  name: scala.meta.Type.Name,
                  tparams: List[scala.meta.Type.Param],
                  bounds: scala.meta.Type.Bounds) extends Decl with Member.Type
}

@branch trait Defn extends Stat
object Defn {
  @ast class Val(mods: List[Mod],
                 pats: List[Pat] @nonEmpty,
                 decltpe: Option[scala.meta.Type],
                 rhs: Term) extends Defn {
    checkFields(pats.forall(!_.is[Term.Name]))
  }
  @ast class Var(mods: List[Mod],
                 pats: List[Pat] @nonEmpty,
                 decltpe: Option[scala.meta.Type],
                 rhs: Option[Term]) extends Defn {
    checkFields(pats.forall(!_.is[Term.Name]))
    checkFields(decltpe.nonEmpty || rhs.nonEmpty)
    checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat.Var]))
  }
  @ast class Def(mods: List[Mod],
                 name: Term.Name,
                 tparams: List[scala.meta.Type.Param],
                 paramss: List[List[Term.Param]],
                 decltpe: Option[scala.meta.Type],
                 body: Term) extends Defn with Member.Term
  @ast class Macro(mods: List[Mod],
                   name: Term.Name,
                   tparams: List[scala.meta.Type.Param],
                   paramss: List[List[Term.Param]],
                   decltpe: Option[scala.meta.Type],
                   body: Term) extends Defn with Member.Term
  @ast class Type(mods: List[Mod],
                  name: scala.meta.Type.Name,
                  tparams: List[scala.meta.Type.Param],
                  body: scala.meta.Type) extends Defn with Member.Type
  @ast class Class(mods: List[Mod],
                   name: scala.meta.Type.Name,
                   tparams: List[scala.meta.Type.Param],
                   ctor: Ctor.Primary,
                   templ: Template) extends Defn with Member.Type
  @ast class Trait(mods: List[Mod],
                   name: scala.meta.Type.Name,
                   tparams: List[scala.meta.Type.Param],
                   ctor: Ctor.Primary,
                   templ: Template) extends Defn with Member.Type {
    checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
  }
  @ast class Object(mods: List[Mod],
                    name: Term.Name,
                    templ: Template) extends Defn with Member.Term {
    checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
  }
}

@ast class Pkg(ref: Term.Ref, stats: List[Stat])
     extends Member.Term with Stat {
  checkFields(ref.isQualId)
  checkFields(stats.forall(_.isTopLevelStat))
  def name: Term.Name = ref match {
    case name: Term.Name => name
    case Term.Select(_, name: Term.Name) => name
  }
}
object Pkg {
  @ast class Object(mods: List[Mod], name: Term.Name, templ: Template)
       extends Member.Term with Stat {
    checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
  }
}

// NOTE: The names of Ctor.Primary and Ctor.Secondary here is always Name.Anonymous.
// While seemingly useless, this name is crucial to one of the key principles behind the semantic API:
// "every definition and every reference should carry a name".
@branch trait Ctor extends Tree with Member
object Ctor {
  @ast class Primary(mods: List[Mod],
                     name: Name,
                     paramss: List[List[Term.Param]]) extends Ctor
  @ast class Secondary(mods: List[Mod],
                       name: Name,
                       paramss: List[List[Term.Param]] @nonEmpty,
                       init: Init,
                       stats: List[Stat]) extends Ctor with Stat {
    checkFields(stats.forall(_.isBlockStat))
  }
}

// NOTE: The name here is always Name.Anonymous.
// See comments to Ctor.Primary and Ctor.Secondary for justification.
@ast class Init(tpe: Type, name: Name, argss: List[List[Term]]) extends Ref {
  checkFields(tpe.isConstructable)
  checkParent(ParentChecks.Init)
}

@ast class Self(name: Name, decltpe: Option[Type]) extends Member

@ast class Template(early: List[Stat],
                    inits: List[Init],
                    self: Self,
                    stats: List[Stat]) extends Tree {
  checkFields(early.forall(_.isEarlyStat && inits.nonEmpty))
  checkFields(stats.forall(_.isTemplateStat))
}

@branch trait Mod extends Tree
object Mod {
  @ast class Annot(init: Init) extends Mod {
    @deprecated("Use init instead", "1.9.0")
    def body = init
  }
  @ast class Private(within: Ref) extends Mod {
    checkFields(within.isWithin)
  }
  @ast class Protected(within: Ref) extends Mod {
    checkFields(within.isWithin)
  }
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
  @ast class Inline() extends Mod
}

@branch trait Enumerator extends Tree
object Enumerator {
  @ast class Generator(pat: Pat, rhs: Term) extends Enumerator
  @ast class Val(pat: Pat, rhs: Term) extends Enumerator
  @ast class Guard(cond: Term) extends Enumerator
}

@ast class Import(importers: List[Importer] @nonEmpty) extends Stat

@ast class Importer(ref: Term.Ref, importees: List[Importee] @nonEmpty) extends Tree {
  checkFields(ref.isStableId)
}

@branch trait Importee extends Tree with Ref
object Importee {
  @ast class Wildcard() extends Importee
  @ast class Name(name: scala.meta.Name) extends Importee {
    checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
  }
  @ast class Rename(name: scala.meta.Name, rename: scala.meta.Name) extends Importee {
    checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
    checkFields(rename.is[scala.meta.Name.Quasi] || rename.is[scala.meta.Name.Indeterminate])
  }
  @ast class Unimport(name: scala.meta.Name) extends Importee {
    checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
  }
}

@ast class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree

@ast class Source(stats: List[Stat]) extends Tree {
  // NOTE: This validation has been removed to allow dialects with top-level terms.
  // Ideally, we should push the validation into a dialect-specific prettyprinter when #220 is fixed.
  // checkFields(stats.forall(_.isTopLevelStat))
}

package internal.trees {
  // NOTE: Quasi is a base trait for a whole bunch of classes.
  // Every root, branch and ast trait/class among scala.meta trees (except for quasis themselves)
  // has a corresponding quasi, e.g. Term.Quasi or Type.Quasi.
  //
  // Here's how quasis represent unquotes
  // (XXX below depends on the position where the unquote occurs, e.g. q"$x" will result in Term.Quasi):
  //   * $x => XXX.Quasi(0, XXX.Name("x"))
  //   * ..$xs => XXX.Quasi(1, XXX.Quasi(0, XXX.Name("xs"))
  //   * ...$xss => XXX.Quasi(2, XXX.Quasi(0, XXX.Name("xss"))
  //   * ..{$fs($args)} => Complex ellipses aren't supported yet
  @branch trait Quasi extends Tree {
    def rank: Int
    def tree: Tree
    def pt: Class[_]
    def become[T <: Quasi : AstInfo]: T
  }

  @registry object All
}
