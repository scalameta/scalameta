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
  lazy val NameClass = c.mirror.staticClass("scala.meta.internal.ast.Name")
  lazy val DefnValClass = c.mirror.staticModule("scala.meta.internal.ast.Defn").info.member(TypeName("Val")).asClass
  lazy val DefnVarClass = c.mirror.staticModule("scala.meta.internal.ast.Defn").info.member(TypeName("Var")).asClass
  lazy val PatTypedClass = c.mirror.staticModule("scala.meta.internal.ast.Pat").info.member(TypeName("Typed")).asClass
  override def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiClass.toType))
    Some(QuasiClass.asBranch +: nonQuasis)
  }
  override def customMatcher(adt: Adt, defName: TermName, localName: TermName): Option[DefDef] = {
    // TODO: it should be possible to customize liftable codegen by providing implicit instances on the outside
    // we can't just do `inferImplicitValue(adt.tpe)`, because that'll lead to a stack overflow
    // we need to do something pickling-like, but I just don't have time to implement that right now
    def redirectTo(methodName: String) = q"def $defName($localName: ${adt.tpe}): ${c.prefix}.u.Tree = ${TermName(methodName)}.apply($localName)"
    if (adt.tpe <:< QuasiClass.toType) Some(redirectTo("liftQuasi"))
    else if (adt.tpe <:< NameClass.toType) Some(redirectTo("liftName"))
    else if (adt.tpe <:< DefnValClass.toType) Some(redirectTo("liftDefnVal"))
    else if (adt.tpe <:< DefnVarClass.toType) Some(redirectTo("liftDefnVar"))
    else if (adt.tpe <:< PatTypedClass.toType) Some(redirectTo("liftPatTyped"))
    else None
  }
}