package org.scalameta.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class hosted extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro HostedMacros.impl
}

class HostedMacros(val c: Context) {
  import c.universe._
  def impl(annottees: c.Tree*): c.Tree = {
    val args = c.macroApplication match {
      case q"new $_(..$args).macroTransform(..$_)" => args
      case q"new $_().macroTransform(..$_)" => Nil
    }
    val contextTpt = {
      def enclosingPackage(sym: Symbol): Symbol = {
        if (sym == NoSymbol) sym
        else if (sym.isPackage || sym.isPackageClass) sym
        else enclosingPackage(sym.owner)
      }
      val apiSym = enclosingPackage(c.internal.enclosingOwner)
      val apiRef = apiSym.fullName.split('.').foldLeft(q"_root_": Tree)((acc, part) => q"$acc.${TermName(part)}")
      tq"$apiRef.Context"
    }
    def transform(ddef: DefDef): DefDef = {
      val DefDef(mods, name, tparams, vparamss, tpt, body) = ddef
      val contextful = q"new _root_.org.scalameta.annotations.contextful[$contextTpt]"
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
