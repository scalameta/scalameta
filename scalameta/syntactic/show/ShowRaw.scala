package scala.meta
package syntactic.show

import org.scalameta.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Raw[T] extends Show[T]
object Raw {
  def apply[T](f: T => Show.Result): Raw[T] = new Raw[T] { def apply(input: T) = f(input) }

  implicit def rawTree[T <: Tree]: Raw[T] = Raw(x => {
    s(x.productPrefix, "(", r(x.productIterator.map({
      case null => "null"
      case s: String => "\"" + s.replace("\n", "\\n").replace("\"", "\\\"") + "\"" // TODO: escape all the special characters as necessary
      case x => x.toString
    }).toList, ", "), ")")
  })
}
