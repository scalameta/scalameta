package scala.reflect
package syntactic.show

import core._
import org.scalareflect.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Raw[T] extends Show[T]
object Raw {
  def apply[T](f: T => Show.Result): Raw[T] = new Raw[T] { def apply(input: T) = f(input) }

  implicit def rawTree[T <: Tree]: Raw[T] = Raw(x => {
    s(x.productPrefix, "(", r(x.productIterator.map(v => if (v != null) v.toString else "null").toList, ", "), ")")
  })
}
