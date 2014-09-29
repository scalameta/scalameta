package scala.meta
package syntactic.show

import org.scalameta.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import internal._

trait Raw[T] extends Show[T]
object Raw {
  def apply[T](f: T => Show.Result): Raw[T] = new Raw[T] { def apply(input: T) = f(input) }

  implicit def rawTree[T <: Tree]: Raw[T] = Raw(x => s(x.productPrefix, "(", x match {
    case x: Lit.String => s(enquote(x.value, DoubleQuotes))
    case x: Lit => s(x.show[Code])
    case x => r(x.productIterator.map({
      case el: String => enquote(el, DoubleQuotes)
      case el => el.toString
    }).toList, ", ")
  }, ")"))
}
