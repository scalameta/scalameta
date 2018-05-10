package scala.meta
package internal
package parsers

object PlatformInvocationTargetException {
  // java.lang.reflect.InvocationTargetException does not exist on Scala.js
  def unapply(e: Throwable): Option[Throwable] = None
}
