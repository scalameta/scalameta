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
  lazy val Origin = tq"_root_.scala.meta.Origin"
  lazy val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  lazy val AstInternal = q"_root_.org.scalameta.ast.internal"
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
      val parents1 = parents :+ tq"$AstInternal.Ast"

      // TODO: think of better ways to abstract this away from the public API
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
      stats1 ++= boilerplate
            
      val qmods = Modifiers(NoFlags, TypeName("meta"), List(q"new _root_.org.scalameta.ast.ast"))
      val qname = TypeName("Quasi")
      val qparents = List(tq"_root_.scala.meta.internal.ast.Quasi")
      val qstats = List(q"def pt: _root_.java.lang.Class[_] = _root_.org.scalameta.runtime.arrayClass(_root_.scala.Predef.classOf[$name], this.rank)")
      mstats1 += q"$qmods class $qname(tree: _root_.scala.Any, rank: _root_.scala.Int) extends ..$qparents { ..$qstats }"

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