package scala.meta
package syntactic

import org.scalameta.convert._
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to parse ${T} (if you're sure that ${T} is parseable, double-check that you've imported a dialect, e.g. scala.meta.dialects.Scala211)")
trait Parse[T] extends Convert[Input, T]
object Parse {
  def apply[T](f: Input => T): Parse[T] = new Parse[T] {
    def apply(input: Input): T = f(input)
  }
}
