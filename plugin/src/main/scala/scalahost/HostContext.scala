package scala.reflect.internal.hosts
package scalacompiler
package scalahost

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.semantic.{HostContext => PalladiumHostContext}

class HostContext[G <: ScalaGlobal](val g: G) extends PalladiumHostContext {
  implicit val palladiumContext: PalladiumHostContext = this

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

  def toPalladium(scalaTree: g.Tree): Tree = {
    scalaTree match {
      case g.Apply(fun, args) =>
        Term.Apply(toPalladium(fun).asInstanceOf[Term], args.map(arg => toPalladium(arg).asInstanceOf[Term]))
      case s: g.Select if s.symbol.isStatic =>
        if (s.symbol.isTerm) Term.Name(s.symbol.fullName.toString)(isBackquoted = false).withScratchpad(s.symbol)
        else if (s.symbol.isModuleClass) Term.Name(s.symbol.fullName.toString)(isBackquoted = false).withScratchpad(s.symbol.sourceModule)
        else Type.Name(s.symbol.fullName.toString)(isBackquoted = false).withScratchpad(s.symbol)
      case g.Literal(g.Constant(s: String)) =>
        Lit.String(s)
      case _ =>
        sys.error(s"""
          |unsupported tree:
          |${g.showCode(scalaTree)}
          |${g.showRaw(scalaTree)}
        """.trim.stripMargin)
    }
  }

  def fromPalladium(palladiumTree: Tree): g.Tree = {
    palladiumTree match {
      case Lit.String(value) => g.Literal(g.Constant(value))
      case _ =>
        sys.error(s"""
          |unsupported tree:
          |${palladiumTree.showCode}
          |${palladiumTree.showRaw}
        """.trim.stripMargin)
    }
  }
}