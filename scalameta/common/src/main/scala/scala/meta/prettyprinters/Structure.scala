package scala.meta
package prettyprinters

import scala.{Seq => _}
import scala.collection.immutable.Seq
import Show.{ sequence => _, repeat => r, indent => i, newline => n, _ }
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Structure] for ${T}")
trait Structure[T] extends Show[T]
object Structure {
  def apply[T](f: T => Show.Result): Structure[T] = new Structure[T] { def apply(input: T) = f(input) }

  implicit def structureSeq[T: Structure]: Structure[Seq[T]] = Structure { xs =>
    Sequence(Str("Seq("), r(xs.map(x => implicitly[Structure[T]].apply(x)), ", "), Str(")"))
  }

  implicit def structureOption[T: Structure]: Structure[Option[T]] = Structure {
    case Some(x) => Sequence(Str("Some("), implicitly[Structure[T]].apply(x), Str(")"))
    case None => Str("None")
  }
}
