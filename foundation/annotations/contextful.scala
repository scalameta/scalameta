package org.scalareflect.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class contextful[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ContextfulMacros.impl
}

class ContextfulMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: Tree*): Tree = {
    val q"new $_[$t]().macroTransform(..$_)" = c.macroApplication
    def mkContextParameter(): ValDef = {
      val prefix = if (t.toString.contains("SourceContext")) "src" else "c"
      val name = c.freshName(TermName(prefix))
      q"implicit val $name: $t"
    }
    def transformMods(mods: Modifiers): Modifiers = {
      val footprint = q"new _root_.org.scalareflect.annotations.internal.contextful[$t]"
      Modifiers(mods.flags, mods.privateWithin, mods.annotations ++ List(footprint))
    }
    def transformCdef(cdef: ClassDef): ClassDef = {
      val q"$mods class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ..$iparams) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val iparams1 = iparams :+ mkContextParameter()
      val mods1 = transformMods(mods)
      q"$mods1 class $tpname[..$tparams] $ctorMods(...$paramss)(implicit ..$iparams1) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
    }
    def transformDdef(ddef: DefDef): DefDef = {
      val q"$mods def $name[..$tparams](...$paramss)(implicit ..$iparams): $tpt = $body" = ddef
      val iparams1 = iparams :+ mkContextParameter()
      val mods1 = transformMods(mods)
      q"$mods1 def $name[..$tparams](...$paramss)(implicit ..$iparams1): $tpt = $body"
    }
    val expanded = annottees match {
      case (cdef: ClassDef) :: rest if !cdef.mods.hasFlag(TRAIT) => transformCdef(cdef) :: rest
      case (ddef: DefDef) :: rest => transformDdef(ddef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes and methods can be @contextful")
    }
    q"{ ..$expanded; () }"
  }
}