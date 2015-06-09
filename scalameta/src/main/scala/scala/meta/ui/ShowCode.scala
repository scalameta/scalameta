package scala.meta
package ui

import org.scalameta.show._
import Show.{sequence => s, repeat => r, indent => i, newline => n, meta => m, adorn => a, function => fn}

@implicitNotFound(msg = "don't know how to show[Code] for ${T} (if you're prettyprinting a tree, be sure to import a dialect, e.g. scala.meta.dialects.Scala211)")
trait Code[T] extends Show[T]
object Code {
  def apply[T](f: T => Show.Result): Code[T] = new Code[T] { def apply(input: T) = f(input) }
  implicit def codeTree[T <: api.Tree](implicit dialect: Dialect, style: Style): Code[T] = Code { (x: api.Tree) => s(x.tokens.map(_.show[Code]).mkString) }
  implicit def codeToken[T <: Token]: Code[T] = Code { x => s(x.code) }
}
