package scala.reflect
package semantic

import org.scalareflect.annotations._

object c {
  @hosted(macroApi = true) def warning(msg: String): Unit = delegate
  @hosted(macroApi = true) def error(msg: String): Unit = delegate
  @hosted(macroApi = true) def abort(msg: String): Nothing = delegate
}
