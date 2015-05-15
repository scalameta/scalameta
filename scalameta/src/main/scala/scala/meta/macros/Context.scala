package scala.meta
package macros

import org.scalameta.annotations._
import scala.annotation._

@opaque
@implicitNotFound("this method requires an implicit scala.meta.macros.Context")
trait Context extends scala.meta.semantic.Context {
  def resources: Map[String, Array[Byte]]
}
