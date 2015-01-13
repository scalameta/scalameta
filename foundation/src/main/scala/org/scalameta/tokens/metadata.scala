package org.scalameta.tokens

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import java.lang.Class

@implicitNotFound(msg = "${T} is not a token class and can't be used here.")
trait TokenMetadata[T] extends ClassTag[T] {
  def name: String
  def runtimeClass: Class[T]
}
object TokenMetadata {
  implicit def materialize[T]: TokenMetadata[T] = macro TokenMetadataMacros.materialize[T]
}

class TokenMetadataMacros(val c: Context) {
  import c.universe._
  import c.internal._
  lazy val TokenClass = rootMirror.staticClass("scala.meta.syntactic.tokenizers.Token")
  lazy val TokenMarkerClass = rootMirror.staticModule("org.scalameta.tokens.internal.package").info.decl(TypeName("tokenClass")).asClass
  def materialize[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    if ((T.tpe <:< TokenClass.toType) && T.tpe.typeSymbol.annotations.exists(_.tree.tpe.typeSymbol == TokenMarkerClass)) {
      val nameBody = {
        val ctor = T.tpe.typeSymbol.info.decls.collect{case m: MethodSymbol if m.isPrimaryConstructor => m}.head
        val argss = ctor.paramLists.map(_.map(p => {
          if (p.info =:= typeOf[String]) Literal(Constant(""))
          else if (p.info.typeSymbol.fullName == "scala.meta.Origin") q"_root_.scala.meta.Origin.None"
          else gen.mkZero(p.info)
        }))
        q"new $T(...$argss).name"
      }
      q"""
        new _root_.org.scalameta.tokens.TokenMetadata[$T] {
          def name: _root_.scala.Predef.String = $nameBody
          def runtimeClass: _root_.java.lang.Class[$T] = implicitly[_root_.scala.reflect.ClassTag[$T]].runtimeClass.asInstanceOf[_root_.java.lang.Class[$T]]
        }
      """
    } else {
      c.abort(c.enclosingPosition, s"${T.tpe} is not a token class and can't be used here.")
    }
  }
}