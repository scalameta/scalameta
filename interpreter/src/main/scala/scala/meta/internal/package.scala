package scala.meta.internal

import scala.meta._
import scala.meta.internal.interpreter._

package object eval {
  implicit class EvalOps(val term: Term) extends AnyVal {
    def eval(implicit c: semantic.Context): Any = `package`.this.eval(term)
  }

  def eval(term: Term)(implicit c: semantic.Context): Any = Interpreter.eval(term)

  def evalFunc(term: Tree, argss: Seq[Any]*)(implicit c: semantic.Context): Any =
      Interpreter.evalFunc(term, argss.toSeq:_*)
}
