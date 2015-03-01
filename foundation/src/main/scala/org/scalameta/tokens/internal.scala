package org.scalameta.tokens

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

package object internal {
  trait Token extends org.scalameta.adt.Internal.Adt
  class root extends scala.annotation.StaticAnnotation
  class branch extends scala.annotation.StaticAnnotation
  class tokenClass extends scala.annotation.StaticAnnotation
  class tokenCompanion extends scala.annotation.StaticAnnotation

  def staticDynamicCheck[T]: Unit = macro Macros.staticDynamicCheck[T]

  class Macros(val c: Context) {
    import c.universe._
    lazy val StaticTokenClass = rootMirror.staticModule("scala.meta.syntactic.Token").info.decl(TypeName("Static")).asClass
    lazy val DynamicTokenClass = rootMirror.staticModule("scala.meta.syntactic.Token").info.decl(TypeName("Dynamic")).asClass
    def staticDynamicCheck[T](implicit T: c.WeakTypeTag[T]) = {
      if ((T.tpe <:< StaticTokenClass.toType) && (T.tpe <:< DynamicTokenClass.toType)) c.abort(c.enclosingPosition, s"${T.tpe} is classified as both static and dynamic token")
      if (!(T.tpe <:< StaticTokenClass.toType) && !(T.tpe <:< DynamicTokenClass.toType)) c.abort(c.enclosingPosition, s"${T.tpe} is classified as neither static nor dynamic token")
      q"()"
    }
  }
}
