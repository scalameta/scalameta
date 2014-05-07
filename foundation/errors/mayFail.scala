package org.scalareflect.errors

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class mayFail[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MayFailMacros.mayFail
}

class MayFailMacros(val c: Context) {
  import c.universe._
  def mayFail(annottees: c.Tree*): c.Tree = {
    val exnTpe = c.macroApplication match {
      case q"new $_().macroTransform(..$_)" => tq"_root_.scala.reflect.core.ReflectionException"
      case q"new $_[$t]().macroTransform(..$_)" => t
    }
    def transform(ddef: DefDef): DefDef = {
      val q"$mods def $name[..$tparams](...$paramss)(implicit ..$implparamss): $tpt = $body" = ddef
      if (tpt.isEmpty) c.abort(ddef.pos, "@mayFail methods must explicitly specify return type")
      val pname = c.freshName(TermName("eh"))
      val implparamss1 = implparamss :+ q"val $pname: _root_.org.scalareflect.errors.ErrorHandler"
      val tpt1 = tq"$pname.Result[$tpt, $exnTpe]"
      val body1 = if (body.nonEmpty) q"""
        import _root_.org.scalareflect.errors.{succeed, fail}
        import $pname.MonadicOps
        import $pname.MonadicSeqOps
        $body
      """ else body
      q"$mods def $name[..$tparams](...$paramss)(implicit ..$implparamss1): $tpt1 = $body1"
    }
    val expanded = annottees match {
      case (ddef: DefDef) :: rest => transform(ddef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only methods can be @mayFail")
    }
    q"{ ..$expanded; () }"
  }
}
