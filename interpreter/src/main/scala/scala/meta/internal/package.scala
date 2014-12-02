package scala.meta.internal

import scala.meta._
import scala.meta.internal.eval.interpreter._

package object eval {
  implicit class EvalOps(val term: Term) extends AnyVal {
    def eval: Any = `package`.this.eval(term)
  }
  def eval(term: Term): Any = Interpreter.eval(term)
}
