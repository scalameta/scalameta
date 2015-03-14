package scala.meta.internal

import scala.meta._
import scala.meta.internal.interpreter._

package object eval {
  implicit class EvalOps(val term: Term) extends AnyVal {
    def eval(env: Map[Term.Name, Any])(implicit c: semantic.Context): Any = Interpreter.eval(term, env)
  }
}
