package scala.reflect
package semantic

import org.scalareflect.errors._
import scala.reflect.core.ReflectionException

package object errors {
  implicit val throwExceptions = handlers.throwExceptions
  implicit val returnTries = handlers.returnTries

  def succeed[S](x: S)(implicit eh: ErrorHandler): eh.Success[S] = eh.succeed(x)
  def fail(message: String)(implicit eh: ErrorHandler): eh.Failure[ReflectionException] = eh.fail(ReflectionException(message))
  def fail(ex: ReflectionException)(implicit eh: ErrorHandler): eh.Failure[ReflectionException] = eh.fail(ex)

  def wrapHosted = new Wrap[Host]
  class Wrap[H <: Host] {
    def apply[T](f: H => T)(implicit h: H, eh: ErrorHandler) = {
      try eh.succeed(f(h))
      catch {
        case ex: ReflectionException => eh.fail(ex)
        case ex: Exception => eh.fail(ReflectionException(ex.toString))
      }
    }
  }
}
