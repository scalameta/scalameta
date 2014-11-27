package org.scalameta.syntactic

import scala.reflect.macros.blackbox.Context

class TypeString(val c: Context) {
  import c.universe._
  import rootMirror.staticClass

  lazy val TypeStringClass = staticClass("scala.meta.syntactic.parsers.Tok.TypeString")
  lazy val KeywordTpe = staticClass("scala.meta.syntactic.parsers.Tok.Keyword").toType

  def materialize[T: WeakTypeTag] = {
    val T = weakTypeOf[T]
    q"""
      new $TypeStringClass[$T] {
        val value = ${T.typeSymbol.name.decodedName.toString}
      }
    """
  }
}
