package scala.reflect
package semantic

import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

object c {
  @hosted(macroApi = true) def application: Tree = delegate
  @hosted(macroApi = true) def warning(msg: String): Unit = delegate
  @hosted(macroApi = true) def error(msg: String): Unit = delegate
  @hosted(macroApi = true) def abort(msg: String): Nothing = delegate
  @hosted(macroApi = true) def resources: Seq[String] = delegate
  @hosted(macroApi = true) def resourceAsBytes(url: String): Array[Byte] = delegate
  @hosted(macroApi = true) def resourceAsUtf8(url: String): String = resourceAsBytes(url).map(bytes => new String(bytes))
}
