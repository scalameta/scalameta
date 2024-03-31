package scala.meta
package internal
package parsers

import java.lang.reflect.InvocationTargetException

object PlatformInvocationTargetException {
  import Absolutize._

  def unapply(e: InvocationTargetException): Option[Throwable] =
    Option(new InvocationTargetException(e.getTargetException.absolutize, e.getMessage))
}
