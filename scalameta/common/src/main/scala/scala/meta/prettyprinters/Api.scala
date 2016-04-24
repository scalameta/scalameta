package scala.meta
package prettyprinters

import scala.language.higherKinds

private[meta] trait Api {
  implicit class XtensionShow[T](x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}

private[meta] trait Aliases {
  type Structure[T] = scala.meta.prettyprinters.Structure[T]
  type Syntax[T] = scala.meta.prettyprinters.Syntax[T]
}
