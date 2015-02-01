package scala.meta
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.compat.Platform.EOL

trait Summary[T] extends Show[T]
object Summary {
  def apply[T](f: T => Show.Result): Summary[T] = new Summary[T] { def apply(input: T) = f(input) }

  implicit def summary[T: Code]: Summary[T] = Summary { x =>
    var result = x.show[Code].replace(EOL, " ")
    if (result.length > 60) result = result.take(60) + "..."
    s(result)
  }
}
