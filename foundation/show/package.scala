package org.scalareflect

import scala.language.higherKinds

package object show {
  implicit class ShowOps[T](val x: T) extends AnyVal {
    def show[Style[T] <: Show[T]](implicit style: Style[T]): String = style(x).toString
  }
}
