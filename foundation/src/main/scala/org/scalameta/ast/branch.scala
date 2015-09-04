package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.ast.{Reflection => AstReflection}

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchMacros.impl
}

class BranchMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  val AstInternal = q"_root_.org.scalameta.ast.internal"
  val Semantic = q"_root_.scala.meta.semantic"
  val SemanticInternal = q"_root_.scala.meta.internal.semantic"
  val FlagsPackage = q"_root_.scala.meta.internal.flags.`package`"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def is(abbrev: String) = c.internal.enclosingOwner.fullName.toString + "." + cdef.name.toString == "scala.meta.internal.ast." + abbrev
      def isQuasi = cdef.name.toString == "Quasi"
      def isName = is("Name") || is("Term.Param.Name") || is("Type.Param.Name")
      def isTerm = is("Term") || is("Lit") || is("Term.Ref") || is("Ctor.Ref") || is("Ctor.Call")
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
        def quasigetter(mods: Modifiers, name: String) = {
          val unsupportedUnquotingPosition = "unsupported unquoting position"
          val unsupportedSplicingPosition = "unsupported splicing position"
          val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
          val impl = q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
          val Modifiers(flags, privateWithin, anns) = mods
          val mods1 = Modifiers(flags | OVERRIDE, privateWithin, anns)
          q"$mods1 def ${TermName(name)}: _root_.scala.Nothing = $impl"
        }
        def quasisetter(mods: Modifiers, name: String, params: ValDef*) = {
          val DefDef(mods1, termName, tparams, _, tpt, rhs) = quasigetter(mods, name)
          DefDef(mods1, termName, tparams, List(params.toList), tpt, rhs)
        }
        var qstats = List(q"def pt: _root_.java.lang.Class[_] = _root_.org.scalameta.runtime.arrayClass(_root_.scala.Predef.classOf[$name], this.rank)")
        if (isName) {
          qstats :+= quasigetter(NoMods, "value")
          qstats :+= quasigetter(PrivateMeta, "env")
          qstats :+= quasigetter(PrivateMeta, "denot")
          qstats :+= quasisetter(PrivateMeta, "withEnv", q"val env: $Semantic.Environment")
          qstats :+= quasisetter(PrivateMeta, "withAttrs", q"val denot: $SemanticInternal.Denotation")
        }
        if (isTerm) {
          qstats :+= quasigetter(PrivateMeta, "env")
          qstats :+= quasigetter(PrivateMeta, "typing")
          qstats :+= quasigetter(PrivateMeta, "expansion")
          qstats :+= quasisetter(PrivateMeta, "withEnv", q"val env: $Semantic.Environment")
          qstats :+= quasisetter(PrivateMeta, "withAttrs", q"val typingLike: $SemanticInternal.TypingLike")
          qstats :+= quasisetter(PrivateMeta, "withExpansion", q"val expansionLike: $SemanticInternal.ExpansionLike")
        }
        qstats :+= q"protected def privateEnv: $Semantic.Environment = null"
        qstats :+= q"protected def privateDenot: $SemanticInternal.Denotation = null"
        qstats :+= q"protected def privateTyping: $SemanticInternal.Typing = null"
        qstats :+= q"protected def privateExpansion: $SemanticInternal.Expansion = null"
        mstats1 += q"$qmods class $qname(rank: _root_.scala.Int, tree: _root_.scala.Any) extends ..$qparents { ..$qstats }"
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