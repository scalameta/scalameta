package scala.meta.internal.eval
package interpreter

import scala.meta._
import syntactic.show._

object Interpreter {
  def eval(term: Term): Any = {
    term match {
      case _ => sys.error(s"""
        |unsupported tree:
        |${term.show[Code]}
        |${term.show[Raw]}
      """.trim.stripMargin)
    }
  }
}