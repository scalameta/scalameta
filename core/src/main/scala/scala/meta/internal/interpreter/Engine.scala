package scala.meta.internal
package interpreter

import scala.meta._
import scala.meta.dialects.Scala211

import scala.collection.immutable.ListMap
import scala.meta.internal.interpreter.environment._

object Interpreter {

  def eval(term: Term): Any = {
    eval(term, Env(List(ListMap[Name, Object]()), ListMap[Name, Object]()))
  }

  def eval(term: Term, env: Env): Any = {
    term match {
      case _ => sys.error(s"""
        |unsupported tree:
        |${term.show[Code]}
        |${term.show[Raw]}
      """.trim.stripMargin)
    }
  }

}
