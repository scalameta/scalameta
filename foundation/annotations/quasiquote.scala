package org.scalareflect.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class quasiquote[T](qname: scala.Symbol) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro QuasiquoteMacros.impl
}

class QuasiquoteMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: c.Tree*): c.Tree = {
    val q"new $_[$qtype](scala.Symbol(${qname: String})).macroTransform(..$_)" = c.macroApplication
    def transform(cdef: ClassDef): ClassDef = {
      val q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      if (stats.nonEmpty) c.abort(cdef.pos, "@quasiquote classes must have empty bodies")
      val qmodule = q"""
        object ${TermName(qname)} {
          def apply[T](args: T*): $qtype = macro ???
          def unapply(scrutinee: Any): Any = macro ???
        }
      """
      val stats1 = qmodule +: stats
      q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if mods.hasFlag(IMPLICIT) => transform(cdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only implicit classes can be @quasiquote")
    }
    q"{ ..$expanded; () }"
  }
}

