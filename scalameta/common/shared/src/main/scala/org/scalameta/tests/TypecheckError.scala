package org.scalameta
package tests

import org.scalameta.adt._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.ParseException
import scala.reflect.macros.TypecheckException
import scala.reflect.internal.util.Position

object typecheckError {
  // Typechecks the enclosed code at compile time
  // and expands into a string literal that contains an error message.
  // Returns an empty string if there's no typecheck error.
  //
  // The options parameter determines whether to print just the error message
  // or also the original code with a familiar-looking caret pointing to the error.
  // Here's an example from the quasiquote suite:
  //
  //   assert(typecheckError("""
  //     import scala.meta.quasiquotes._
  //     import scala.meta.dialects.Scala211
  //     val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
  //   """) === """
  //     |<macro>:4: not found: value X
  //     |      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
  //     |                        ^
  //   """.trim.stripMargin)
  def apply(code: String)(implicit options: Options): String = macro impl

  def impl(c: Context)(code: c.Tree)(options: c.Tree): c.Tree = {
    import c.universe.{Position => _, _}
    val s_code = code match {
      case Literal(Constant(s_code: String)) => s_code
      case _ => c.abort(c.enclosingPosition, "this macro only works with literal strings")
    }
    val tree = {
      try c.parse(s_code.replace("QQQ", "\"\"\""))
      catch { case ex: ParseException => c.abort(c.enclosingPosition, "this code fails to parse") }
    }
    def format(ex: TypecheckException) = {
      if (options.tpe.typeSymbol == symbolOf[Options.WithoutPositions.type]) {
        ex.msg
      } else if (options.tpe.typeSymbol == symbolOf[Options.WithPositions.type]) {
        Position.formatMessage(ex.pos.asInstanceOf[Position], ex.msg, shortenFile = true)
      } else {
        c.abort(c.enclosingPosition, s"unsupported option: ${options.tpe}")
      }
    }
    try { c.typecheck(tree, silent = false); q"${""}" }
    catch { case ex: TypecheckException => q"${format(ex)}" }
  }

  trait Options
  trait LowPriorityOptions {
    implicit object WithPositions extends Options
  }
  object Options extends LowPriorityOptions {
    implicit object WithoutPositions extends Options
  }
}