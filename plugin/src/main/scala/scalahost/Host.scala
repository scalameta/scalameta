package scala.meta
package internal.hosts.scalacompiler
package scalahost

import scala.{Seq => _}
import scala.collection.immutable.Seq

import scala.meta.semantic._
import scala.meta.syntactic.show._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Host => PalladiumHost}

class Host[G <: ScalaGlobal](val g: G) extends PalladiumHost {
  import g.Quasiquote
  implicit val palladiumHost: PalladiumHost = this

  def defns(ref: Ref): Seq[Tree] = ???
  def attrs(tree: Tree): Seq[Attr] = ???
  def owner(tree: Tree): Scope = ???
  def members(scope: Scope): Seq[Tree] = ???
  def members(scope: Scope, name: Name): Seq[Tree] = ???
  def <:<(tpe1: Type, tpe2: Type): Boolean = ???
  def lub(tpes: Seq[Type]): Type = ???
  def glb(tpes: Seq[Type]): Type = ???
  def superclasses(member: Member.Template): Seq[Member.Template] = ???
  def subclasses(member: Member.Template): Seq[Member.Template] = ???
  def overridden(member: Member): Seq[Member] = ???
  def overriding(member: Member): Seq[Member] = ???
  def erasure(tpe: Type): Type = ???

  def toPalladium(scalaTree: g.Tree): Tree = {
    scalaTree match {
      case g.Apply(fun, args) =>
        Term.Apply(toPalladium(fun).asInstanceOf[Term], args.map(arg => toPalladium(arg).asInstanceOf[Term]))
      case s: g.Select if s.symbol.isStatic =>
        if (s.symbol.isTerm) Term.Name(s.symbol.fullName.toString).appendScratchpad(s.symbol)
        else if (s.symbol.isModuleClass) Term.Name(s.symbol.fullName.toString).appendScratchpad(s.symbol.sourceModule)
        else Type.Name(s.symbol.fullName.toString).appendScratchpad(s.symbol)
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
          |${palladiumTree.show[Code]}
          |${palladiumTree.show[Raw]}
        """.trim.stripMargin)
    }
  }
}