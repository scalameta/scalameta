package scala.reflect
package syntactic

import org.scalareflect.show._
import scala.reflect.core._
import scala.language.higherKinds

package object show {
  implicit class ShowOps[T](val x: T) extends AnyVal {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}
