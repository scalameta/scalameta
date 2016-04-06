package scala.meta
package internal
package tokens

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
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val Adt = q"_root_.org.scalameta.adt"
      val TokenInternal = q"_root_.scala.meta.internal.tokens.internal"
      val anns1 = q"new $TokenInternal.root" +: q"new $Adt.root" +: anns
      val parents1 = parents :+ tq"$TokenInternal.Token" :+ tq"_root_.scala.Product" :+ tq"_root_.scala.Serializable"
      ClassDef(Modifiers(flags, privateWithin, anns1), name, tparams, Template(parents1, self, stats))
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }
}