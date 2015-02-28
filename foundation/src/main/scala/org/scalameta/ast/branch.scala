package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchMacros.impl
}

class BranchMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      // NOTE: turned off because we can't have @ast hierarchy sealed anymore
      // hopefully, in the future we'll find a way to restore sealedness
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@branch traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = {
        if (name.toString == "Defn") flags
        else (flags.asInstanceOf[Long] & ~(INTERFACE.asInstanceOf[Long])).asInstanceOf[FlagSet]
      }
      val AdtInternal = q"_root_.org.scalameta.adt.Internal"
      val AstInternal = q"_root_.org.scalameta.ast.internal"
      val thisType = q"type ThisType <: ${cdef.name}"
      val hierarchyCheck = q"$AstInternal.hierarchyCheck[${cdef.name}]"
      val register = q"$AstInternal.register[${cdef.name}]"
      val stats1 = {
        if (name.toString == "Defn") stats
        else stats :+ thisType :+ hierarchyCheck :+ register
      }
      val anns1 = q"new $AdtInternal.branch" +: q"new $AstInternal.branch" +: anns
      ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents, self, stats1))
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @branch")
    }
    q"{ ..$expanded; () }"
  }
}