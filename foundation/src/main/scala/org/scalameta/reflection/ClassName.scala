package org.scalameta.reflection

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

trait ClassName[T]
object ClassName {
  implicit def materialize[T]: ClassName[T] = macro ClassNameMacros.materialize[T]
}

class ClassNameMacros(val c: Context) {
  import c.universe._
  def materialize[T](implicit T: WeakTypeTag[T]) = q"""
    new _root_.org.scalameta.reflection.ClassName[$T] {
      override def toString = ${T.tpe.typeSymbol.name.decodedName.toString}
    }
  """
}
