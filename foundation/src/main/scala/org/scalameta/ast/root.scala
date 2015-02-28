package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RootMacros.impl
}

class RootMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val q"${mods @ Modifiers(flags, privateWithin, anns)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      // NOTE: turned off because we can't have @ast hierarchy sealed anymore
      // hopefully, in the future we'll find a way to restore sealedness
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@root traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@root traits cannot be final")
      val flags1 = (flags.asInstanceOf[Long] & ~(INTERFACE.asInstanceOf[Long])).asInstanceOf[FlagSet]
      // TODO: think of better ways to abstract this away from the public API
      val Tree = tq"_root_.scala.meta.Tree"
      val Datum = tq"_root_.scala.Any"
      val Data = tq"_root_.scala.collection.immutable.Seq[$Datum]"
      val Origin = tq"_root_.scala.meta.Origin"
      val AdtInternal = q"_root_.org.scalameta.adt.Internal"
      val AstInternal = q"_root_.org.scalameta.ast.internal"
      val thisType = if (stats.collect{ case TypeDef(_, TypeName("ThisType"), _, _) => () }.isEmpty) q"type ThisType <: ${cdef.name}" else q"()"
      val tag = q"def internalTag: _root_.scala.Int"
      val hierarchyCheck = q"$AstInternal.hierarchyCheck[${cdef.name}]"
      val q"..$boilerplate" = q"""
        // NOTE: these are internal APIs designed to be used only by hosts
        // TODO: these APIs will most likely change in the future
        // because we would like to make sure that trees are guaranteed to be immutable
        private[meta] def scratchpad: $Data = internalScratchpad
        private[meta] def appendScratchpad(datum: $Datum): ThisType = internalCopy(scratchpad = internalScratchpad :+ datum)
        private[meta] def withScratchpad(data: $Data): ThisType = internalCopy(scratchpad = data)
        private[meta] def mapScratchpad(f: $Data => $Data): ThisType = internalCopy(scratchpad = f(internalScratchpad))

        // NOTE: these are internal APIs that are meant to be used only in the implementation of the framework
        // host implementors should not utilize these APIs
        // TODO: turn the prototype argument of internalCopy into ThisType
        // if done naively, this isn't going to compile for prototypes of @branch traits as ThisType there is abstract
        protected def internalPrototype: ThisType
        protected def internalParent: $Tree
        protected def internalScratchpad: $Data
        protected def internalOrigin: $Origin
        private[meta] def internalCopy(prototype: $Tree = internalPrototype, parent: $Tree = internalParent, scratchpad: $Data = internalScratchpad, origin: $Origin = internalOrigin): ThisType
      """
      val stats1 = (stats ++ boilerplate) :+ thisType :+ tag :+ hierarchyCheck
      val anns1 = q"new $AdtInternal.root" +: q"new $AstInternal.root" +: anns
      val parents1 = parents :+ tq"$AstInternal.Ast"
      q"${Modifiers(flags1, privateWithin, anns1)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }
}