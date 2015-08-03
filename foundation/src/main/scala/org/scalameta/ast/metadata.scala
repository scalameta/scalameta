package org.scalameta.ast

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import java.lang.Class

@implicitNotFound(msg = "${T} is not an ast class and can't be used here.")
trait AstMetadata[T] extends ClassTag[T] {
  def runtimeClass: Class[T]
  def quasi(rank: Int, tree: Any): T
}
object AstMetadata {
  implicit def materialize[T]: AstMetadata[T] = macro AstMetadataMacros.materialize[T]
}

class AstMetadataMacros(val c: Context) {
  import c.universe._
  import c.internal._
  lazy val TreeClass = rootMirror.staticClass("scala.meta.Tree")
  lazy val QuasiClass = rootMirror.staticClass("scala.meta.internal.ast.Quasi")
  def materialize[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    if (T.tpe <:< QuasiClass.toType) {
      q"""
        new _root_.org.scalameta.ast.AstMetadata[$T] {
          def runtimeClass: _root_.java.lang.Class[$T] = implicitly[_root_.scala.reflect.ClassTag[$T]].runtimeClass.asInstanceOf[_root_.java.lang.Class[$T]]
          def quasi(rank: Int, tree: Any): $T = ${T.tpe.typeSymbol.companion}.apply(rank, tree)
        }
      """
    } else if (T.tpe <:< TreeClass.toType) {
      q"""
        new _root_.org.scalameta.ast.AstMetadata[$T] {
          def runtimeClass: _root_.java.lang.Class[$T] = implicitly[_root_.scala.reflect.ClassTag[$T]].runtimeClass.asInstanceOf[_root_.java.lang.Class[$T]]
          def quasi(rank: Int, tree: Any): $T = ${T.tpe.typeSymbol.companion}.Quasi.apply(rank, tree)
        }
      """
    } else {
      c.abort(c.enclosingPosition, s"${T.tpe} is not an ast class and can't be used here.")
    }
  }
}