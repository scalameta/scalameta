package scala.meta
package macros

trait Context extends scala.meta.semantic.Context {
  def warning(msg: String): Unit
  def error(msg: String): Unit
  def abort(msg: String): Nothing
  def resources: Map[String, Array[Byte]]
}
