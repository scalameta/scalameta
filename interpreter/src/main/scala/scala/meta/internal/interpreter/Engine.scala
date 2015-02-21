package scala.meta.internal.eval
package interpreter

import scala.meta._
import scala.meta.dialects.Scala211

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