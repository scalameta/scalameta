package scala.meta
package parsers
package common

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.inputs._

@implicitNotFound(msg = "don't know how to parse into ${T}")
trait Parse[T] {
  def apply(input: Input)(implicit dialect: Dialect): T
}
