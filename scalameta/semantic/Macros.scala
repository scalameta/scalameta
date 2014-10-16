package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.annotations._

package object macros {
  @hosted(macroApi = true) def warning(msg: String): Unit = delegate
  @hosted(macroApi = true) def error(msg: String): Unit = delegate
  @hosted(macroApi = true) def abort(msg: String): Nothing = delegate
  @hosted(macroApi = true) def resources: Seq[String] = delegate
  @hosted(macroApi = true) def resource(url: String): Array[Byte] = delegate
}