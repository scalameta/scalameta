package scala.meta
package internal
package prettyprinters

import org.scalameta.adt._
import scala.annotation.implicitNotFound
import scala.Console._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r }

@root trait PositionStyle
object PositionStyle {
  @leaf object BlackAndWhite extends PositionStyle
  @leaf object Colorful extends PositionStyle
  implicit val default: PositionStyle = BlackAndWhite
}

@root trait SliceStyle
object SliceStyle {
  @leaf object Hide extends SliceStyle
  @leaf object Show extends SliceStyle
  implicit val default: SliceStyle = Hide
}

@implicitNotFound(msg = "don't know how to show[Positions] for ${T}")
trait Positions[T] extends Show[T]
object Positions {
  def apply[T](f: T => Show.Result): Positions[T] = new Positions[T] { def apply(input: T) = f(input) }

  implicit def positionsTree[T <: Tree : Syntax](implicit positionStyle: PositionStyle, sliceStyle: SliceStyle): Positions[T] = Positions { x =>
    def loopTree(x: Tree): Show.Result = {
      implicit class XtensionString(s: String) {
        def colored(color: String) = if (positionStyle == PositionStyle.Colorful) (color + s + RESET) else s
        def sliced(slice: => String) = if (sliceStyle == SliceStyle.Show) (s + "<" + slice + ">") else s
      }
      def loopField(x: Any, color: String): Show.Result = x match {
        case el: String => s(enquote(el, DoubleQuotes).colored(color))
        case el: Tree => loopTree(el)
        case el: Nil.type => s("Nil".colored(color))
        case el @ List(List()) => s("List(List())".colored(color))
        case el: ::[_] => s("List(".colored(color), r(el.map(el => loopField(el, color)), ", ".colored(color)), ")".colored(color))
        case el: None.type => s("None".colored(color))
        case el: Some[_] => s("Some(".colored(color), loopField(el.get, color), ")".colored(color))
        case el => s(el.toString.colored(color))
      }
      def pos(x: Tree): String = {
        if (x.pos != Position.None) s"{${x.pos.start}..${x.pos.end}}".sliced(x.toString)
        else ""
      }
      def color(x: Tree): String = {
        if (x.pos != Position.None) GREEN else RED
      }
      val prefix = (x.productPrefix + pos(x) + "(").colored(color(x))
      val fields = r(x.productIterator.toList.map(el => loopField(el, color(x))), ", ".colored(color(x)))
      val suffix = ")".colored(color(x))
      s(prefix, fields, suffix)
    }
    loopTree(x)
  }
}
