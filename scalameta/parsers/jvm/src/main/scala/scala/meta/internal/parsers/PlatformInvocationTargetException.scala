package scala.meta
package internal
package parsers

import java.lang.reflect.InvocationTargetException
import Absolutize._

object PlatformInvocationTargetException {
  def unapply(e: InvocationTargetException): Option[Throwable] =
    Option(new InvocationTargetException(e.getTargetException.absolutize, e.getMessage))
}
