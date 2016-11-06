package scala.meta
package prettyprinters

import scala.language.higherKinds

private[meta] trait Api {
  implicit class XtensionShow[T](x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
  implicit class XtensionSyntax[T](x: T)(implicit style: Syntax[T]) {
    def syntax: String = style(x).toString
  }
  implicit class XtensionStructure[T](x: T)(implicit style: Structure[T]) {
    def structure: String = style(x).toString
  }
}

private[meta] trait Aliases {
  type Structure[T] = scala.meta.prettyprinters.Structure[T]
  val Structure = scala.meta.prettyprinters.Structure

  type Syntax[T] = scala.meta.prettyprinters.Syntax[T]
  val Syntax = scala.meta.prettyprinters.Syntax
}
