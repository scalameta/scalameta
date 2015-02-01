package scala.meta
package macros

import org.scalameta.annotations._

@opaque
trait Context extends scala.meta.semantic.Context {
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def resources: Map[String, Array[Byte]]
}
