package org.scalameta.contexts

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class context(translateExceptions: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ContextMacros.impl
}

class ContextMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: Tree*): Tree = {
    val args = c.macroApplication match {
      case q"new $_(..$args).macroTransform(..$_)" => args
      case q"new $_().macroTransform(..$_)" => Nil
    }
    val translateExceptions = args.collect{ case q"translateExceptions = true" => true }.nonEmpty
    def transform(cdef: ClassDef): ClassDef = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val stats1 = stats.map {
        case stat @ DefDef(mods, name, tparams, vparamss, tpt, body) if translateExceptions =>
          val body1 = q"""
            try $body
            catch {
              case ex: _root_.scala.meta.ScalametaException => throw ex
              case ex: _root_.scala.meta.ScalametaError => throw ex
              case other: _root_.scala.Exception => throw new _root_.scala.meta.SemanticException(other.getMessage, other)
            }
          """
          treeCopy.DefDef(stat, mods, name, tparams, vparamss, tpt, body1)
        case other =>
          other
      }
      q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
    }
    val expanded = annottees match {
      case (cdef: ClassDef) :: rest if !cdef.mods.hasFlag(TRAIT) => transform(cdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes can be @context")
    }
    q"{ ..$expanded; () }"
  }
}
