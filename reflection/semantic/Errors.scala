package scala.reflect
package semantic

import org.scalareflect.errors._
import scala.reflect.core.ReflectionException

package object errors {
  implicit val throwExceptions = handlers.throwExceptions
  implicit val returnTries = handlers.returnTries

  def succeed[S](x: S)(implicit eh: ErrorHandler): eh.Success[S] = eh.succeed(x)
  def fail[E <: Exception](x: E)(implicit eh: ErrorHandler): eh.Failure[E] = eh.fail(x)

  def wrapHosted = new Wrap[HostContext]
  def wrapMacrohosted = new Wrap[MacroContext]
  class Wrap[C <: HostContext] {
    def apply[T](f: C => T)(implicit c: C, eh: ErrorHandler) = {
      try eh.succeed(f(c))
      catch {
        case ex: ReflectionException => eh.fail(ex)
        case ex: Exception => eh.fail(ReflectionException(ex.toString))
      }
    }
  }
}
