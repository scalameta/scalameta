package org.scalameta.ast

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.unreachable
import org.scalameta.ast.internal.Ast
import org.scalameta.adt.{LiftableMacros => AdtLiftableMacros}
import org.scalameta.ast.{Reflection => AstReflection}

trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAst[T <: Ast]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

class LiftableMacros(override val c: Context) extends AdtLiftableMacros(c) with AstReflection {
  import c.universe._
  lazy val QuasiClass = c.mirror.staticClass("scala.meta.internal.ast.Quasi")
  override def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiClass.toType))
    Some(QuasiClass.asBranch +: nonQuasis)
  }
  override def customMatcher(adt: Adt, defName: TermName, localName: TermName): Option[DefDef] = {
    // TODO: it should be possible to customize liftable codegen by providing implicit instances on the outside
    // we can't just do `inferImplicitValue(adt.tpe)`, because that'll lead to a stack overflow
    // we need to do something pickling-like, but I just don't have time to implement that right now
    if (adt.tpe <:< QuasiClass.toType) {
      Some(q"def $defName($localName: ${adt.tpe}): ${c.prefix}.u.Tree = liftQuasi.apply($localName)")
    } else if (adt.tpe <:< c.mirror.staticClass("scala.meta.internal.ast.Name").toType) {
      Some(q"def $defName($localName: ${adt.tpe}): ${c.prefix}.u.Tree = liftName.apply($localName)")
    } else {
      None
    }
  }
}