package scala.meta
package ui

import scala.language.higherKinds
import org.scalameta.show._

trait Api {
  // NOTE: I wish there was a way to avoid duplication and ambiguities wrt org.scalameta.show
  implicit class ShowOps[T](val x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}