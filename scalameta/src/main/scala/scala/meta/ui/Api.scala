package scala.meta
package ui

import scala.language.higherKinds
import org.scalameta.show._

private[meta] trait Api {
  type Exception = scala.meta.ui.Exception
  val Exception = scala.meta.ui.Exception

  type Code[T] = scala.meta.ui.Code[T]
  type Raw[T] = scala.meta.ui.Raw[T]
  type Semantics[T] = scala.meta.ui.Semantics[T]

  // NOTE: I wish there was a way to avoid duplication and ambiguities wrt org.scalameta.show
  implicit class ShowOps[T](x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}