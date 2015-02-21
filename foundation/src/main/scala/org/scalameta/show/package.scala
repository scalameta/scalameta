package org.scalameta

import scala.language.higherKinds

package object show {
  implicit class XtensionShow[T](x: T) {
    def show[Style[T] <: Show[T]](implicit style: Style[T]): String = style(x).toString
  }
}
