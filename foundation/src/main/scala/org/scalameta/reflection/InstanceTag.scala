package org.scalameta
package reflection

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.ClassTag
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "${T} is not a trivially instantiable class or an object.")
trait InstanceTag[T] extends ClassTag[T] {
  def instantiate: T
}

object InstanceTag {
  implicit def materialize[T]: InstanceTag[T] = macro InstanceMetadataMacros.materialize[T]
}

class InstanceMetadataMacros(val c: Context) {
  import c.universe._
  import c.internal._
  def materialize[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    def fail() = c.abort(c.enclosingPosition, T.tpe + " is not a trivially instantiable class or an object")
    val body = {
      val TypeRef(pre, sym: ClassSymbol, Nil) = T.tpe
      if (sym.isModuleClass) {
        gen.mkAttributedRef(pre, sym.module.asModule)
      } else if (sym.isClass) {
        def isTrivialCtor(m: MethodSymbol) = m.isConstructor && m.paramLists.flatten.isEmpty
        val trivialCtors = T.tpe.decls.collect{case m: MethodSymbol if isTrivialCtor(m) => m}
        trivialCtors match { case List(_) => q"new ${gen.mkAttributedRef(pre, sym)}"; case _ => fail() }
      } else {
        fail()
      }
    }
    q"""
      new _root_.org.scalameta.reflection.InstanceTag[$T] {
        override def runtimeClass: _root_.java.lang.Class[$T] = _root_.scala.reflect.classTag[$T].runtimeClass.asInstanceOf[_root_.java.lang.Class[$T]]
        override def instantiate: $T = $body
      }
    """
  }
}