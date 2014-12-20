package org.scalameta.tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.show.escape

class token extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TokenMacros.impl
}

class TokenMacros(val c: Context) {
  import c.universe._
  import Flag._
  val Adt = q"_root_.org.scalameta.adt"
  val TokenInternal = q"_root_.org.scalameta.tokens.internal"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val parents1 = ListBuffer[Tree]() ++ parents
      val mstats1 = ListBuffer[Tree]() ++ mstats
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)

      // step 1: generate boilerplate required by the @adt infrastructure
      anns1 += q"new $Adt.leaf"
      anns1 += q"new $TokenInternal.tokenClass"
      manns1 += q"new $TokenInternal.tokenCompanion"

      // step 2: generate implementation of `def name: String` and `def end: String` for static tokens
      val isStaticToken = !paramss.flatten.exists(_.name.toString == "end")
      val needsName = isStaticToken && !stats.exists{ case DefDef(_, TermName("name"), _, _, _, _) => true; case _ => false }
      val needsEnd = isStaticToken && !stats.exists{ case DefDef(_, TermName("end"), _, _, _, _) => true; case _ => false }
      val hasCustomCode = isStaticToken && stats.exists{ case DefDef(_, TermName("code"), _, _, _, _) => true; case _ => false }
      var code = name.decodedName.toString
      if (code == "_ ") code = "_" // NOTE: can't call a class `_`, so have to use `_ `
      if (code == "class ") code = "class" // TODO: wat?
      if (code == "package ") code = "package" // TODO: wat?
      if (needsName) stats1 += q"def name: _root_.scala.Predef.String = ${escape(code)}"
      if (needsEnd) {
        val codeRef = if (hasCustomCode) q"this.code.length" else q"${code.length}"
        stats1 += q"def end: _root_.scala.Int = this.start + $codeRef - 1"
      }

      // step 3: ensure that the token is correctly classified as either static or dynamic
      stats1 += q"$TokenInternal.staticDynamicCheck[$name]"

      val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef: ClassDef) :: (mdef: ModuleDef) :: rest => transform(cdef, mdef) ++ rest
      case (cdef: ClassDef) :: rest => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only classes can be @token")
    }
    q"{ ..$expanded; () }"
  }
}