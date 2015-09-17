package scala.meta
package syntactic

import org.scalameta.convert._
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to parse into ${T}")
trait Parse[T] {
  def apply(input: Input)(implicit dialect: Dialect): T
}
