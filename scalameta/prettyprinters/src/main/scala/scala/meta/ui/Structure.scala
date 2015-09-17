package scala.meta
package ui

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Structure] for ${T}")
trait Structure[T] extends Show[T]
object Structure {
  def apply[T](f: T => Show.Result): Structure[T] = new Structure[T] { def apply(input: T) = f(input) }

  implicit def structureSeq[T: Structure]: Structure[Seq[T]] = Structure { xs =>
    s("Seq(", r(xs.map(x => implicitly[Structure[T]].apply(x)), ", "), ")")
  }
}
