package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => _, repeat => r, indent => i, newline => n, _ }
import scala.compat.Platform.EOL
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Summary] for ${T}")
trait Summary[T] extends Show[T]
object Summary {
  def apply[T](f: T => Show.Result): Summary[T] = new Summary[T] { def apply(input: T) = f(input) }

  implicit def summary[T: Syntax]: Summary[T] = Summary { x =>
    var result = x.show[Syntax].replace(EOL, " ")
    if (result.length > 60) result = result.take(60) + "..."
    Str(result)
  }
}
