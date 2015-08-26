package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RootMacros.impl
}

class RootMacros(val c: Context) {
  import c.universe._
  import Flag._
  lazy val Tree = tq"_root_.scala.meta.Tree"
  lazy val Datum = tq"_root_.scala.Any"
  lazy val Data = tq"_root_.scala.collection.immutable.Seq[$Datum]"
  lazy val Flags = tq"_root_.scala.meta.internal.flags.`package`.Flags"
  lazy val Tokens = tq"_root_.scala.meta.Tokens"
  lazy val Environment = tq"_root_.scala.meta.semantic.Environment"
  lazy val Denotation = tq"_root_.scala.meta.internal.semantic.Denotation"
  lazy val Typing = tq"_root_.scala.meta.internal.semantic.Typing"
  lazy val Expansion = tq"_root_.scala.meta.internal.semantic.Expansion"
  lazy val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  lazy val AstInternal = q"_root_.org.scalameta.ast.internal"
  lazy val SemanticInternal = q"_root_.scala.meta.internal.semantic"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"${mods @ Modifiers(flags, privateWithin, anns)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      // NOTE: sealedness is turned off because we can't have @ast hierarchy sealed anymore
      // hopefully, in the future we'll find a way to restore sealedness
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@root traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@root traits cannot be final")
      val flags1 = flags // TODO: flags | SEALED
      val needsThisType = stats.collect{ case TypeDef(_, TypeName("ThisType"), _, _) => () }.isEmpty
      if (needsThisType) stats1 += q"type ThisType <: $name"
      stats1 += q"def internalTag: _root_.scala.Int"
      mstats1 += q"$AstInternal.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $AdtInternal.root" :+ q"new $AstInternal.root"
      val parents1 = parents :+ tq"$AstInternal.Ast" :+ tq"_root_.scala.Product" :+ tq"_root_.scala.Serializable"

      // TODO: think of better ways to abstract this away from the public API
      // See comments in ast.scala to see the idea behind internalCopy.
      val q"..$boilerplate" = q"""
        private[meta] def flags: $Flags
        private[meta] def withFlags(flags: $Flags): ThisType

        // NOTE: these are internal APIs that are meant to be used only in the implementation of the scala.meta framework
        // host implementors should not utilize these APIs
        protected def internalFlags: $Flags
        protected def internalPrototype: ThisType
        protected def internalParent: $Tree
        protected def internalTokens: $Tokens
        protected def internalEnv: $Environment
        protected def internalDenot: $Denotation
        protected def internalTyping: $Typing
        protected def internalExpansion: $Expansion

        // TODO: turn the prototype argument of internalCopy into ThisType
        // if done naively, this isn't going to compile for prototypes of @branch traits as ThisType there is abstract
        private[meta] def internalCopy(
          flags: $Flags = _root_.scala.meta.internal.flags.`package`.ZERO,
          prototype: $Tree = this,
          parent: $Tree = internalParent,
          tokens: $Tokens = internalTokens,
          env: $Environment = internalEnv,
          denot: $Denotation = internalDenot,
          typing: $Typing = internalTyping,
          expansion: $Expansion = internalExpansion): ThisType
      """
      stats1 ++= boilerplate

      val qmods = Modifiers(NoFlags, TypeName("meta"), List(q"new _root_.org.scalameta.ast.ast"))
      val qname = TypeName("Quasi")
      val qparents = List(tq"_root_.scala.meta.internal.ast.Quasi")
      var qstats = List(q"def pt: _root_.java.lang.Class[_] = _root_.org.scalameta.runtime.arrayClass(_root_.scala.Predef.classOf[$name], this.rank)")
      qstats :+= q"protected def internalEnv: $Environment = null"
      qstats :+= q"protected def internalDenot: $Denotation = null"
      qstats :+= q"protected def internalTyping: $Typing = null"
      qstats :+= q"protected def internalExpansion: $Expansion = null"
      mstats1 += q"$qmods class $qname(rank: _root_.scala.Int, tree: _root_.scala.Any) extends ..$qparents { ..$qstats }"

      val cdef1 = q"${Modifiers(flags1, privateWithin, anns1)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef: ModuleDef) :: rest if mods.hasFlag(TRAIT) => transform(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }
}