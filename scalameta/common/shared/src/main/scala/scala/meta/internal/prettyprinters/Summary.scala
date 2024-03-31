package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{sequence => _, _}
import scala.annotation.implicitNotFound

import org.scalameta.internal.ScalaCompat.EOL

@implicitNotFound(msg = "don't know how to show[Summary] for ${T}")
trait Summary[T] extends Show[T]
object Summary {
  def apply[T](f: T => Show.Result): Summary[T] = new Summary[T] {
    def apply(input: T) = f(input)
  }

  implicit def summary[T: Syntax]: Summary[T] = Summary { x =>
    var result = x.syntax.replace(EOL, " ")
    if (result.length > 60) result = result.take(60) + "..."
    Str(result)
  }
}
