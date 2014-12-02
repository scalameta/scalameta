package scala.meta.syntactic

import org.scalameta.show._
import scala.language.higherKinds

package object show {
  implicit class ShowOps[T](val x: T) extends AnyVal {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}
