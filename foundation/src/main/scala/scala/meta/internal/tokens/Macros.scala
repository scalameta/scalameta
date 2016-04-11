package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object TokenTyperMacros {
  def staticDynamicCheck[T]: Unit = macro TokenTyperMacrosBundle.staticDynamicCheck[T]
}

class TokenTyperMacrosBundle(val c: Context) {
  import c.universe._

  lazy val TokenModule = rootMirror.staticModule("scala.meta.tokens.Token")
  lazy val StaticTokenClass = TokenModule.info.decl(TypeName("Static")).asClass
  lazy val DynamicTokenClass = TokenModule.info.decl(TypeName("Dynamic")).asClass

  def staticDynamicCheck[T](implicit T: c.WeakTypeTag[T]) = {
    if ((T.tpe <:< StaticTokenClass.toType) && (T.tpe <:< DynamicTokenClass.toType)) c.abort(c.enclosingPosition, s"${T.tpe} is classified as both static and dynamic token")
    if (!(T.tpe <:< StaticTokenClass.toType) && !(T.tpe <:< DynamicTokenClass.toType)) c.abort(c.enclosingPosition, s"${T.tpe} is classified as neither static nor dynamic token")
    q"()"
  }
}
