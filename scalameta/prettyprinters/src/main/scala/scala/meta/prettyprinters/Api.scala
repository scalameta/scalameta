package scala.meta
package prettyprinters

import scala.language.higherKinds

private[meta] trait Api {
  implicit class XtensionShow[T](x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}

private[meta] trait Aliases {
  @deprecated("use show[Structure] instead", "0.0.3")
  type Raw[T] = scala.meta.prettyprinters.Structure[T]
  type Structure[T] = scala.meta.prettyprinters.Structure[T]

  @deprecated("use show[Syntax] instead", "0.0.3")
  type Code[T] = scala.meta.prettyprinters.Syntax[T]
  type Syntax[T] = scala.meta.prettyprinters.Syntax[T]
}
