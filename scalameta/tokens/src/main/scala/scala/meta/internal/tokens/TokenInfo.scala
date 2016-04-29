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
    if ((T.tpe <:< TokenClass.toType) && T.tpe.typeSymbol.annotations.exists(_.tree.tpe.typeSymbol == TokenMarkerClass)) {
      val nameBody = {
        val ctor = T.tpe.typeSymbol.info.decls.collect{case m: MethodSymbol if m.isPrimaryConstructor => m}.head
        val argss = ctor.paramLists.map(_.map(p => {
          if (p.name == TermName("input")) q"""_root_.scala.meta.inputs.Input.String("")"""
          else if (p.name == TermName("dialect")) q"""_root_.scala.meta.dialects.Scala211"""
          else if (p.name == TermName("start")) q"0"
          else if (p.name == TermName("end")) q"-1"
          else if (p.name == TermName("value") && p.info =:= typeOf[String]) q""" "" """
          else if (p.name == TermName("value")) gen.mkZero(p.info)
          else if (p.name == TermName("rank")) q"0"
          else if (p.name == TermName("tree")) q"null"
          else c.abort(c.enclosingPosition, s"unsupported parameter: $p")
        }))
        q"new $T(...$argss).name"
      }
      q"""
        new _root_.scala.meta.internal.tokens.TokenInfo[$T] {
          def name: _root_.scala.Predef.String = $nameBody
          def runtimeClass: _root_.java.lang.Class[$T] = implicitly[_root_.scala.reflect.ClassTag[$T]].runtimeClass.asInstanceOf[_root_.java.lang.Class[$T]]
        }
      """
    } else {
      c.abort(c.enclosingPosition, s"${T.tpe} is not a token class and can't be used here.")
    }
  }
}