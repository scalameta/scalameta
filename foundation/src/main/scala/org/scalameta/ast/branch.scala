package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchMacros.impl
}

class BranchMacros(val c: Context) {
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  val AstInternal = q"_root_.org.scalameta.ast.internal"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def is(abbrev: String) = c.internal.enclosingOwner.fullName.toString + "." + cdef.name.toString == "scala.meta.internal.ast." + abbrev
      def isQuasi = cdef.name.toString == "Quasi"
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      // NOTE: sealedness is turned off because we can't have @ast hierarchy sealed anymore
      // hopefully, in the future we'll find a way to restore sealedness
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@branch traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags // TODO: flags | SEALED
      stats1 += q"type ThisType <: $name"
      mstats1 += q"$AstInternal.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $AdtInternal.branch" :+ q"new $AstInternal.branch"

      if (!isQuasi) {
        val qmods = Modifiers(NoFlags, TypeName("meta"), List(q"new _root_.org.scalameta.ast.ast"))
        val qname = TypeName("Quasi")
        val qparents = tq"$name" +: tq"_root_.scala.meta.internal.ast.Quasi" +: parents.map({
          case Ident(name) => Select(Ident(name.toTermName), TypeName("Quasi"))
          case Select(qual, name) => Select(Select(qual, name.toTermName), TypeName("Quasi"))
          case unsupported => c.abort(unsupported.pos, "implementation restriction: unsupported parent")
        })
        def quasigetter(name: String) = {
          val unsupportedUnquotingPosition = "unsupported unquoting position"
          val unsupportedSplicingPosition = "unsupported splicing position"
          val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
          val impl = q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
          q"override def ${TermName(name)}: _root_.scala.Nothing = $impl"
        }
        var qstats = List(q"def pt: _root_.java.lang.Class[_] = _root_.org.scalameta.runtime.arrayClass(_root_.scala.Predef.classOf[$name], this.rank)")
        if (is("Name") || is("Term.Param.Name") || is("Type.Param.Name")) qstats ++= List("denot", "sigma", "value").map(quasigetter)
        mstats1 += q"$qmods class $qname(tree: _root_.scala.Any, rank: _root_.scala.Int) extends ..$qparents { ..$qstats }"
      }

      val cdef1 = ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents, self, stats1.toList))
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef: ModuleDef) :: rest if mods.hasFlag(TRAIT) => transform(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @branch")
    }
    q"{ ..$expanded; () }"
  }
}