package scala.meta
package macros

import org.scalameta.adt._
import org.scalameta.annotations._

private[meta] trait Api {
  @hosted(macroApi = true) def warning(msg: String): Unit = ???
  @hosted(macroApi = true) def error(msg: String): Unit = ???
  @hosted(macroApi = true) def abort(msg: String): Nothing = ???
  @hosted(macroApi = true) def resources: Seq[String] = ???
  @hosted(macroApi = true) def resource(url: String): Array[Byte] = ???
}