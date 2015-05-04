package scala.meta
package ui

import org.scalameta.show._
import org.scalameta.adt._
import org.scalameta.unreachable
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.compat.Platform.EOL
import scala.annotation.implicitNotFound
import scala.collection.mutable.StringBuilder
import scala.Console._

@root trait PositionStyle
object PositionStyle {
  @leaf object BlackAndWhite extends PositionStyle
  @leaf object Colorful extends PositionStyle
  implicit val default: PositionStyle = BlackAndWhite
}

@implicitNotFound(msg = "don't know how to show[Positions] for ${T} (if you're prettyprinting a tree, be sure to import a dialect, e.g. scala.meta.dialects.Scala211)")
trait Positions[T] extends Show[T]
object Positions {
  def apply[T](f: T => Show.Result): Positions[T] = new Positions[T] { def apply(input: T) = f(input) }
  implicit val Colorful: PositionStyle = PositionStyle.Colorful

  implicit def positionsTree[T <: Tree : Code](implicit style: PositionStyle): Positions[T] = Positions { x =>
    def loopTree(x: Tree): Show.Result = {
      implicit class XtensionString(s: String) {
        def colored(color: String) = if (style == PositionStyle.Colorful) (color + s + RESET) else s
      }
      def loopField(x: Any, color: String): Show.Result = x match {
        case el: String => s(enquote(el, DoubleQuotes).colored(color))
        case el: Tree => loopTree(el)
        case el: Nil.type => s("Nil".colored(color))
        case el: ::[_] => s("List(".colored(color), r(el.map(el => loopField(el, color)), ", ".colored(color)), ")".colored(color))
        case el: None.type => s("None".colored(color))
        case el: Some[_] => s("Some(".colored(color), loopField(el.get, color), ")".colored(color))
        case el => s(el.toString.colored(color))
      }
      def position(x: Tree): String = x.origin match {
        case origin: Origin.Parsed => s"[${origin.start.offset}..${origin.end.offset}]"
        case _ => ""
      }
      def color(x: Tree): String = x.origin match {
        case _: Origin.Parsed => GREEN
        case _: Origin.Synthetic => RED
      }
      val prefix = (x.productPrefix + position(x) + "(").colored(color(x))
      val fields = r(x.productIterator.toList.map(el => loopField(el, color(x))), ", ".colored(color(x)))
      val suffix = ")".colored(color(x))
      s(prefix, fields, suffix)
    }
    loopTree(x)
  }
}
