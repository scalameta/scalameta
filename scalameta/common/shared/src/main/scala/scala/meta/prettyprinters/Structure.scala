package scala.meta
package prettyprinters

import scala.annotation.implicitNotFound
import org.scalameta.collections._
import Show.{ sequence => _, repeat => r, indent => i, newline => n, _ }

@implicitNotFound(msg = "don't know how to show[Structure] for ${T}")
trait Structure[T] extends Show[T]
object Structure {
  def apply[T](f: T => Show.Result): Structure[T] = new Structure[T] { def apply(input: T) = f(input) }

  implicit def structureList[T: Structure](implicit options: Options): Structure[List[T]] = Structure { xs =>
    val contents = if (options.isLazy && xs.isLazy) Str("...") else r(xs.map(x => implicitly[Structure[T]].apply(x)), ", ")
    Sequence(Str("List("), contents, Str(")"))
  }

  implicit def structureOption[T: Structure](implicit options: Options): Structure[Option[T]] = Structure {
    case scala.Some(x) => Sequence(Str("Some("), implicitly[Structure[T]].apply(x), Str(")"))
    case scala.None => Str("None")
  }
}
