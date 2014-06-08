package org.scalareflect.ast

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
      val q"${Modifiers(flags, privateWithin, anns)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      // TODO: think of better ways to abstract this away from the public API
      val Host = tq"_root_.scala.reflect.semantic.Host"
      val Tree = tq"_root_.scala.reflect.core.Tree"
      val Origin = tq"_root_.scala.reflect.core.Origin"
      val SeqAny = tq"_root_.scala.collection.immutable.Seq[_root_.scala.Any]"
      val Scratchpads = tq"_root_.scala.Predef.Map[$Host, $SeqAny]"
      val q"..$boilerplate" = q"""
        // NOTE: these are internal APIs designed to be used only by hosts
        // TODO: these APIs will most likely change in the future
        // because we would like to make sure that trees are guaranteed to be immutable
        private[reflect] def scratchpad(implicit h: $Host): $SeqAny = internalScratchpads.getOrElse(h, _root_.scala.Nil);
        private[reflect] def appendScratchpad(datum: _root_.scala.Any)(implicit h: $Host): ThisType = internalCopy(scratchpads = internalScratchpads + (h -> (internalScratchpads.getOrElse(h, Nil) :+ datum)))
        private[reflect] def withScratchpad(scratchpad: $SeqAny)(implicit h: $Host): ThisType = internalCopy(scratchpads = internalScratchpads + (h -> scratchpad))
        private[reflect] def mapScratchpad(f: $SeqAny => $SeqAny)(implicit h: $Host): ThisType = internalCopy(scratchpads = internalScratchpads + (h -> f(internalScratchpads.getOrElse(h, Nil))))

        // NOTE: these are internal APIs that are meant to be used only in the implementation of the framework
        // host implementors should not utilize these APIs
        // TODO: turn the prototype argument of internalCopy into ThisType
        // if done naively, this isn't going to compile for prototypes of @branch traits as ThisType there is abstract
        protected def internalPrototype: ThisType
        protected def internalParent: $Tree
        protected def internalScratchpads: $Scratchpads
        private[core] def internalCopy(prototype: $Tree = internalPrototype, parent: $Tree = internalParent, scratchpads: $Scratchpads = internalScratchpads, origin: $Origin = origin): ThisType
      """
      val stats1 = stats ++ boilerplate
      val anns1 = q"new _root_.org.scalareflect.adt.root" +: anns
      q"${Modifiers(flags, privateWithin, anns1)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }
}