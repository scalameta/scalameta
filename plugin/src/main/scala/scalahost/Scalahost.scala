package scala.reflect.internal.hosts
package scalacompiler
package scalahost

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic._
import scala.reflect.macros.contexts.{Context => ScalaContext}
import scala.reflect.semantic.{MacroContext => PalladiumContext}

class Scalahost(val scalaContext: ScalaContext) extends PalladiumContext {
  def syntaxProfile: SyntaxProfile = ???
  def semanticProfile: SemanticProfile = ???

  // NOTE: def stats(scope: Scope): Seq[Tree] is implicit in signatures of Template and Pkg
  def members(scope: Scope): Seq[Member] = ???
  def members(scope: Scope, name: Name): Seq[Member] = ???
  def ctors(scope: Scope): Seq[Ctor] = ???

  def defn(term: Term.Ref): Seq[Member.Term] = ???
  def defn(tpe: Type.Ref): Member = ???
  def overrides(member: Member.Term): Seq[Member.Term] = ???
  def overrides(member: Member.Type): Seq[Member.Type] = ???

  def <:<(tpe1: Type, tpe2: Type): Boolean = ???
  def weak_<:<(tpe1: Type, tpe2: Type): Boolean = ???
  def supertypes(tpe: Type): Seq[Type] = ???
  def linearization(tpes: Seq[Type]): Seq[Type] = ???
  def subclasses(tpe: Type): Seq[Member.Template] = ???
  def self(tpe: Type): Aux.Self = ???
  def lub(tpes: Seq[Type]): Type = ???
  def glb(tpes: Seq[Type]): Type = ???
  def widen(tpe: Type): Type = ???
  def dealias(tpe: Type): Type = ???
  def erasure(tpe: Type): Type = ???

  def attrs(tree: Tree): Seq[Attribute] = ???

  def application: Tree = ???
  def warning(msg: String): Unit = ???
  def error(msg: String): Unit = ???
  def abort(msg: String): Nothing = ???
  def resources: Seq[String] = ???
  def resourceAsBytes(url: String): Array[Byte] = ???

  def toPalladium(scalaTree: scalaContext.global.Tree): Tree = {
    import scalaContext.global
    implicit val palladiumContext: PalladiumContext = this
    scalaTree match {
      case global.Apply(fun, args) =>
        Term.Apply(toPalladium(fun).asInstanceOf[Term], args.map(arg => toPalladium(arg).asInstanceOf[Term]))
      case s: global.Select if s.symbol.isStatic =>
        if (s.symbol.isTerm) Term.Name(s.symbol.fullName.toString)(isBackquoted = false).withScratchpad(s.symbol)
        else if (s.symbol.isModuleClass) Term.Name(s.symbol.fullName.toString)(isBackquoted = false).withScratchpad(s.symbol.sourceModule)
        else Type.Name(s.symbol.fullName.toString)(isBackquoted = false).withScratchpad(s.symbol)
      case global.Literal(global.Constant(s: String)) =>
        Lit.String(s)
      case _ =>
        sys.error(s"""
          |unsupported tree:
          |${global.showCode(scalaTree)}
          |${global.showRaw(scalaTree)}
        """.trim.stripMargin)
    }
  }
  def fromPalladium(palladiumTree: Tree): scalaContext.global.Tree = {
    import scalaContext.global
    implicit val palladiumContext: PalladiumContext = this
    palladiumTree match {
      case Lit.String(value) => global.Literal(global.Constant(value))
      case _ =>
        sys.error(s"""
          |unsupported tree:
          |${palladiumTree.showCode}
          |${palladiumTree.showRaw}
        """.trim.stripMargin)
    }
  }
}