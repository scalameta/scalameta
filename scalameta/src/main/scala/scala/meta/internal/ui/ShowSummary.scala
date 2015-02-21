package scala.meta
package internal
package ui

import org.scalameta.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.compat.Platform.EOL
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Summary] for ${T} (if you're prettyprinting a tree, be sure to import a dialect, e.g. scala.meta.dialects.Scala211)")
private[meta] trait Summary[T] extends Show[T]
private[meta] object Summary {
  def apply[T](f: T => Show.Result): Summary[T] = new Summary[T] { def apply(input: T) = f(input) }

  implicit def summary[T: Code]: Summary[T] = Summary { x =>
    var result = x.show[Code].replace(EOL, " ")
    if (result.length > 60) result = result.take(60) + "..."
    s(result)
  }
}
