package org.scalareflect
package convert

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class converter extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ConverterMacros.converter
}

class ConverterMacros(val c: Context) {
  import c.universe._

  def converter(annottees: Tree*): Tree = {
    def transform(ddef: DefDef): DefDef = {
      val q"$mods def $name[..$tparams](...$paramss): $tpt = $body" = ddef
      q"$mods def $name[..$tparams](...$paramss): $tpt = $body"
    }
    val expanded = annottees match {
      case (ddef: DefDef) :: rest => transform(ddef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only methods can be @converter")
    }
    q"{ ..$expanded; () }"
  }
}
