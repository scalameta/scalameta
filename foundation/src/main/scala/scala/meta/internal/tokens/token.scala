package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

class token extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TokenMacros.impl
}

class TokenMacros(val c: Context) {
  import c.universe._
  import Flag._
  val Adt = q"_root_.org.scalameta.adt"
      val TokenInternal = q"_root_.scala.meta.internal.tokens.internal"
  val Invariants = q"_root_.org.scalameta.invariants.`package`"
  val Unsupported = tq"_root_.scala.`package`.UnsupportedOperationException"
  val Content = tq"_root_.scala.meta.inputs.Content"
  val Dialect = tq"_root_.scala.meta.Dialect"
  val Token = tq"_root_.scala.meta.tokens.Token"
  val Prototype = tq"_root_.scala.meta.tokens.Token.Prototype"
  val None = q"_root_.scala.meta.tokens.Token.Prototype.None"
  val Some = q"_root_.scala.meta.tokens.Token.Prototype.Some"
  val Require = q"_root_.org.scalameta.invariants.`package`.require"
  val Debug = q"_root_.org.scalameta.invariants.`package`.debug"
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
      anns1 += q"new $Adt.leaf(toString = false)"
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
      if (needsName) {
        // TODO: deduplicate with scala.meta.internal.prettyprinters.escape
        // NOTE: can be removed altogether once tokens are renamed
        val codepage = Map("\t" -> "\\t", "\b" -> "\\b", "\n" -> "\\n", "\r" -> "\\r", "\f" -> "\\f", "\\" -> "\\\\")
        val name = code.flatMap(c => codepage.getOrElse(c.toString, c.toString))
        stats1 += q"def name: _root_.scala.Predef.String = $name"
      }
      if (needsEnd) {
        val codeRef = if (hasCustomCode) q"this.code.length" else q"${code.length}"
        stats1 += q"def end: _root_.scala.Int = this.start + $codeRef"
      }

      // step 3: ensure that the token is correctly classified as either static or dynamic
      stats1 += q"$TokenInternal.staticDynamicCheck[$name]"

      // step 4: generate implementation of `def adjust`
      val needsAdjust = !stats.exists{ case DefDef(_, TermName("adjust"), _, _, _, _) => true; case _ => false }
      if (needsAdjust) {
        val paramContent = q"val content: $Content = this.content"
        val paramDialect = q"val dialect: $Dialect = this.dialect"
        val paramStart = q"val start: _root_.scala.Int = this.start"
        val paramEnd = q"val end: _root_.scala.Int = this.end"
        val paramDelta = q"val delta: _root_.scala.Int = 0"
        val adjustResult = {
          if (code == "BOF" || code == "EOF") q"this.copy(content = content, dialect = dialect)"
          else if (isStaticToken) q"this.copy(content = content, dialect = dialect, start = start)"
          else q"this.copy(content = content, dialect = dialect, start = start, end = end)"
        }
        val adjustError = {
          if (code == "BOF" || code == "EOF") q""" "position-changing adjust on Token." + this.name """
          else if (isStaticToken) q""" "end-changing adjust on Tokens." + this.name """
          else q""" "fatal error in the token infrastructure" """
        }
        val body = q"""
          (start != this.start || end != this.end, delta != 0) match {
            case (false, false) =>
              this.copy(content = content, dialect = dialect)
            case (true, false) =>
              val result = $adjustResult
              if (result.start != start || result.end != end) {
                var message = $adjustError
                message += (": expected " + result.start + ".." + result.end)
                message += (", actual " + start + ".." + end)
                throw new $Unsupported(message)
              }
              result
            case (false, true) =>
              this.adjust(content = content, dialect = dialect, start = this.start + delta, end = this.end + delta)
            case (true, true) =>
              throw new _root_.scala.`package`.UnsupportedOperationException("you can specify either start/end or delta, but not both")
          }
        """
        stats1 += q"def adjust($paramContent, $paramDialect, $paramStart, $paramEnd, $paramDelta): $Token = $body"
      }

      // step 5: generate the boilerplate fields
      var paramss1 = (q"val content: $Content" +: q"val dialect: $Dialect" +: paramss.head) +: paramss.tail
      val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
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