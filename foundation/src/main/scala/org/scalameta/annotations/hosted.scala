package org.scalameta.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class hosted(macroApi: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro HostedMacros.impl
}

class HostedMacros(val c: Context) {
  import c.universe._
  def impl(annottees: c.Tree*): c.Tree = {
    val args = c.macroApplication match {
      case q"new $_(..$args).macroTransform(..$_)" => args
      case q"new $_().macroTransform(..$_)" => Nil
    }
    val macroApi = args.collect{ case q"macroApi = true" => true }.nonEmpty
    val exnTpe = tq"_root_.scala.meta.MetaException"
    val contextTpe = if (macroApi) tq"_root_.scala.meta.macros.Context" else tq"_root_.scala.meta.semantic.Context"
    def transform(ddef: DefDef): DefDef = {
      val DefDef(mods, name, tparams, vparamss, tpt, body) = ddef
      val contextful = q"new _root_.org.scalameta.annotations.contextful[$contextTpe]"
      val mods1 = Modifiers(mods.flags, mods.privateWithin, mods.annotations ++ List(contextful))
      DefDef(mods1, name, tparams, vparamss, tpt, body)
    }
    val expanded = annottees match {
      case (ddef: DefDef) :: rest => transform(ddef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only methods can be @hosted")
    }
    q"{ ..$expanded; () }"
  }
}
