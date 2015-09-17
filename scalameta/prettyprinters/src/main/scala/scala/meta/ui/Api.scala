package scala.meta
package ui

import scala.language.higherKinds
import org.scalameta.show._

private[meta] trait PrettyprintApi {
  // NOTE: I wish there was a way to avoid duplication and ambiguities wrt org.scalameta.show
  implicit class XtensionShow[T](x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }

  @deprecated("use show[Structure] instead", "0.0.3") type Raw[T] = scala.meta.ui.Structure[T]
  type Structure[T] = scala.meta.ui.Structure[T]

  @deprecated("use show[Syntax] instead", "0.0.3") type Code[T] = scala.meta.ui.Syntax[T]
  type Syntax[T] = scala.meta.ui.Syntax[T]

  type Semantics[T] = scala.meta.ui.Semantics[T]
}