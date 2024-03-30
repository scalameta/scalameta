package scala.meta
package internal
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.prettyprinters._

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Summary] for ${T}")
trait Summary[T] extends Show[T]
object Summary {
//  import Show.{sequence => _, _}

  def apply[T](f: T => Show.Result): Summary[T] = new Summary[T] {
    def apply(input: T) = f(input)
  }

  implicit def summary[T: Syntax]: Summary[T] = Summary { x =>
    var result = x.syntax.replace(EOL, " ")
    if (result.length > 60) result = result.take(60) + "..."
    Show.Str(result)
  }
}
