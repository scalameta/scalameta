package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.unreachable
import org.scalameta.adt.{LiftableMacros => AdtLiftableMacros}
import scala.meta.internal.tokens.Metadata.Token

trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeToken[T <: Token]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

class LiftableMacros(override val c: Context) extends AdtLiftableMacros(c) {
  import c.universe._
  lazy val UnquoteClass = c.mirror.staticModule("scala.meta.tokens.Token").asModule.info.member(TypeName("Unquote")).asClass
  override def customMatcher(adt: Adt, defName: TermName, localName: TermName): Option[DefDef] = {
    // TODO: see comments to ast.LiftableMacros
    def redirectTo(methodName: String) = q"def $defName($localName: ${adt.tpe}): ${c.prefix}.u.Tree = ${TermName(methodName)}.apply($localName)"
    if (adt.tpe <:< UnquoteClass.toType) Some(redirectTo("liftUnquote"))
    else None
  }
}