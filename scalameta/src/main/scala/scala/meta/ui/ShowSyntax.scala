package scala.meta
package ui

import scala.annotation.implicitNotFound
import org.scalameta.show._
import Show.{sequence => s, repeat => r, indent => i, newline => n, meta => m, adorn => a, function => fn}

@implicitNotFound(msg = "don't know how to show[Syntax] for ${T} (if you're prettyprinting a tree, be sure to import a dialect, e.g. scala.meta.dialects.Scala211)")
trait Syntax[T] extends Show[T]
object Syntax {
  def apply[T](f: T => Show.Result): Syntax[T] = new Syntax[T] { def apply(input: T) = f(input) }
  implicit def syntaxTree[T <: Tree](implicit dialect: Dialect): Syntax[T] = Syntax { (x: Tree) => s(x.tokens.map(_.show[Syntax]).mkString) }
  implicit def syntaxToken[T <: Token]: Syntax[T] = Syntax { x => s(x.code) }
}
