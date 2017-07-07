package scala.meta
package prettyprinters

import scala.annotation.implicitNotFound
import Show.{ sequence => _, repeat => r, _ }

@implicitNotFound(msg = "don't know how to show[Structure] for ${T}")
trait Structure[T] extends Show[T]
object Structure {
  def apply[T](f: T => Show.Result): Structure[T] = new Structure[T] { def apply(input: T) = f(input) }

  implicit def structureList[T: Structure]: Structure[List[T]] = Structure { xs =>
    Sequence(Str("List("), r(xs.map(x => implicitly[Structure[T]].apply(x)), ", "), Str(")"))
  }

  implicit def structureOption[T: Structure]: Structure[Option[T]] = Structure {
    case scala.Some(x) => Sequence(Str("Some("), implicitly[Structure[T]].apply(x), Str(")"))
    case scala.None => Str("None")
  }
}
