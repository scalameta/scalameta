package scala.meta.syntactic
package show

import org.scalameta.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.syntactic.show.internal._
import scala.meta.syntactic.ast._
import scala.{meta => api}

trait Raw[T] extends Show[T]
object Raw {
  def apply[T](f: T => Show.Result): Raw[T] = new Raw[T] { def apply(input: T) = f(input) }

  implicit def rawTree[T <: api.Tree]: Raw[T] = Raw(x => s(x.productPrefix, "(", x match {
    case x: Lit.String => s(enquote(x.value, DoubleQuotes))
    case x: Lit => s(x.show[Code])
    case x => r(x.productIterator.map({
      case el: String => enquote(el, DoubleQuotes)
      case el => el.toString
    }).toList, ", ")
  }, ")"))
}
