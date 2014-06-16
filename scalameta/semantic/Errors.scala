package scala.meta
package semantic

import org.scalameta.errors._

package object errors {
  implicit val throwExceptions = handlers.throwExceptions
  implicit val returnTries = handlers.returnTries

  def succeed[S](x: S)(implicit eh: ErrorHandler): eh.Success[S] = eh.succeed(x)
  def fail(message: String)(implicit eh: ErrorHandler): eh.Failure[MetaException] = eh.fail(MetaException(message))
  def fail(ex: MetaException)(implicit eh: ErrorHandler): eh.Failure[MetaException] = eh.fail(ex)

  def wrapHosted = new Wrap[Host]
  def wrapMacrohosted = new Wrap[MacroHost]
  class Wrap[H <: Host] {
    def apply[T](f: H => T)(implicit h: H, eh: ErrorHandler) = {
      try eh.succeed(f(h))
      catch {
        case ex: MetaException => eh.fail(ex)
        case ex: Exception => eh.fail(MetaException(ex.toString))
      }
    }
  }
}
