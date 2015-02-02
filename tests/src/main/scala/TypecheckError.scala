package org.scalameta
package tests

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.ParseException
import scala.reflect.macros.TypecheckException

object typecheckError {
  def apply(code: String): String = macro impl
  def impl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._
    val s_code = code match {
      case Literal(Constant(s_code: String)) => s_code
      case _ => c.abort(c.enclosingPosition, "this macro only works with literal strings")
    }
    val tree = {
      try c.parse(s_code)
      catch { case ex: ParseException => c.abort(c.enclosingPosition, "this code fails to parse") }
    }
    try { c.typecheck(tree, silent = false); q"${""}" }
    catch { case ex: TypecheckException => q"${ex.msg}" }
  }
}