package scala.meta
package internal
package ui

import org.scalameta.show._
import org.scalameta.unreachable
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.compat.Platform.EOL
import scala.annotation.implicitNotFound
import scala.collection.mutable.StringBuilder
import scala.Console._

@implicitNotFound(msg = "don't know how to show[Tokens] for ${T} (if you're prettyprinting a tree, be sure to import a dialect, e.g. scala.meta.dialects.Scala211)")
private[meta] trait Tokens[T] extends Show[T]
private[meta] object Tokens {
  def apply[T](f: T => Show.Result): Tokens[T] = new Tokens[T] { def apply(input: T) = f(input) }

  implicit def tokensTree[T <: Tree : Code]: Tokens[T] = Tokens { x =>
    def text(x: Tree) = {
      x.origin match {
        case Origin.None => s("<Origin.None>")
        case Origin.Parsed(_, _, _, _) => s(x.origin.tokens.map(_.code).mkString)
        case Origin.Transformed(_) => s("<Origin.Transformed>")
      }
    }
    def colored(x: Tree): Show.Result = {
      implicit class XtensionString(s: String) { def colored(color: String) = color + s + RESET }
      def loop(x: Any, color: String): Show.Result = x match {
        case el: String => s(enquote(el, DoubleQuotes).colored(color))
        case el: Tree => colored(el)
        case el: Nil.type => s("Nil".colored(color))
        case el: ::[_] => s("List(".colored(color), r(el.map(el => loop(el, color)), ", ".colored(color)), ")".colored(color))
        case el: None.type => s("None".colored(color))
        case el: Some[_] => s("Some(".colored(color), loop(el.get, color), ")".colored(color))
        case el => s(el.toString.colored(color))
      }
      def color(x: Tree): String = x.origin match {
        case Origin.None => RED
        case Origin.Parsed(_, _, _, _) => GREEN
        case Origin.Transformed(tree) => color(tree)
      }
      s((x.productPrefix + "(").colored(color(x)), r(x.productIterator.toList.map(el => loop(el, color(x))), ", ".colored(color(x))), ")".colored(color(x)))
    }
    s(text(x), EOL, colored(x))
  }
}
