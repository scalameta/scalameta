package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import java.lang.Class
import scala.meta.tokens.Token
import scala.meta.classifiers._
import org.scalameta.internal.MacroHelpers

@implicitNotFound(msg = "${T} is not a token class and can't be used here.")
trait TokenInfo[T <: Token] extends ClassTag[T] with Classifier[Token, T] {
  def name: String
  def runtimeClass: Class[T]
  def apply(token: Token): Boolean = token != null && runtimeClass.isAssignableFrom(token.getClass)
}
object TokenInfo {
  implicit def materialize[T <: Token]: TokenInfo[T] = macro TokenInfoMacros.materialize[T]
}

class TokenInfoMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  import c.internal._

  lazy val TokenClass = rootMirror.staticClass("scala.meta.tokens.Token")
  lazy val TokenMarkerClass = rootMirror.staticModule("scala.meta.internal.tokens.Metadata").info.decl(TypeName("tokenClass")).asClass
  lazy val TokenInfoClass = hygienicRef[scala.meta.internal.tokens.TokenInfo[_]]

  def materialize[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val tokenMarkerAnn = T.tpe.typeSymbol.annotations.map(_.tree).find(_.tpe.typeSymbol == TokenMarkerClass)
    if ((T.tpe <:< TokenClass.toType) && tokenMarkerAnn.nonEmpty) {
      val q"new $_(${name: String})" = tokenMarkerAnn.get
      q"""
        new _root_.scala.meta.internal.tokens.TokenInfo[$T] {
          def name: _root_.scala.Predef.String = $name
          def runtimeClass: _root_.java.lang.Class[$T] = implicitly[_root_.scala.reflect.ClassTag[$T]].runtimeClass.asInstanceOf[_root_.java.lang.Class[$T]]
        }
      """
    } else {
      c.abort(c.enclosingPosition, s"${T.tpe} is not a token class and can't be used here.")
    }
  }
}