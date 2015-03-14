package scala.meta

import scala.meta.semantic.Context
import scala.meta.internal.interpreter.Interpreter

package object eval {
  implicit class EvalOps(val term: Term) extends AnyVal {
    def eval(env: Map[Term.Name, Any] = Map())(implicit c: Context): Any = Interpreter.eval(term, env)
  }
}
