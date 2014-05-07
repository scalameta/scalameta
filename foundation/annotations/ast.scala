package org.scalareflect.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstMacros.impl
}

class AstMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val q"${mods @ Modifiers(flags, privateWithin, anns)} class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val leaf = q"new _root_.org.scalareflect.adt.leaf"
      val hasSourceContext = q"new _root_.org.scalareflect.annotations.contextful[_root_.scala.reflect.core.SourceContext]"
      val footprint = q"new _root_.org.scalareflect.annotations.internal.ast"
      val anns1 = anns ++ List(leaf, hasSourceContext, footprint)
      val src = q"def src = implicitly[_root_.scala.reflect.core.SourceContext]"
      val stats1 = stats ++ List(src)
      q"${Modifiers(flags, privateWithin, anns1)} class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
    }
    val expanded = annottees match {
      case (cdef: ClassDef) :: rest => transform(cdef) :: rest
      case (mdef: ModuleDef) :: rest => mdef :: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes and objects can be @ast")
    }
    q"{ ..$expanded; () }"
  }
}