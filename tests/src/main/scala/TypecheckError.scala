package org.scalameta
package tests

import org.scalameta.adt._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.ParseException
import scala.reflect.macros.TypecheckException
import scala.reflect.internal.util.Position

@root trait Style
object Style {
  @leaf object WithPositions extends Style
  @leaf implicit object WithoutPositions extends Style
}

object typecheckError {
  def apply(code: String)(implicit style: Style): String = macro impl
  def impl(c: Context)(code: c.Tree)(style: c.Tree): c.Tree = {
    import c.universe.{Position => _, _}
    val s_code = code match {
      case Literal(Constant(s_code: String)) => s_code
      case _ => c.abort(c.enclosingPosition, "this macro only works with literal strings")
    }
    val tree = {
      try c.parse(s_code)
      catch { case ex: ParseException => c.abort(c.enclosingPosition, "this code fails to parse") }
    }
    def format(ex: TypecheckException) = {
      if (style.tpe.typeSymbol == symbolOf[Style.WithoutPositions.type]) {
        ex.msg
      } else if (style.tpe.typeSymbol == symbolOf[Style.WithPositions.type]) {
        Position.formatMessage(ex.pos.asInstanceOf[Position], ex.msg, shortenFile = true)
      } else {
        c.abort(c.enclosingPosition, s"unsupported style: ${style.tpe}")
      }
    }
    try { c.typecheck(tree, silent = false); q"${""}" }
    catch { case ex: TypecheckException => q"${format(ex)}" }
  }
}