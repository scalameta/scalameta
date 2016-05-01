package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import org.scalameta.internal.MacroHelpers
import scala.meta.internal.ast.Metadata.Ast

@implicitNotFound(msg = "${T} is not an ast class and can't be used here.")
trait AstInfo[T <: Ast] extends ClassTag[T] {
  def runtimeClass: Class[T]
  def quasi(rank: Int, tree: Tree): T
}
object AstInfo {
  implicit def materialize[T <: Ast]: AstInfo[T] = macro AstInfoMacros.materialize[T]
}

class AstInfoMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  import c.internal._

  def materialize[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val QuasiSymbol = rootMirror.staticClass("scala.meta.internal.ast.Quasi")
    val TreeSymbol = rootMirror.staticClass("scala.meta.Tree")
    val QuasiFactory = {
      if (T.tpe <:< QuasiSymbol.toType) q"${T.tpe.typeSymbol.companion}"
      else if (T.tpe <:< TreeSymbol.toType) q"${T.tpe.typeSymbol.companion}.Quasi"
      else c.abort(c.enclosingPosition, s"${T.tpe} is not an ast class and can't be used here.")
    }
    q"""
      new $AstInfoClass[$T] {
        def runtimeClass: $ClassClass[$T] = implicitly[$ClassTagClass[$T]].runtimeClass.asInstanceOf[$ClassClass[$T]]
        def quasi(rank: $IntClass, tree: $TreeSymbol): $T = $QuasiFactory.apply(rank, tree)
      }
    """
  }
}