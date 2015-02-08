package scala.meta
package ui

import scala.language.higherKinds
import org.scalameta.show._

trait Api {
  type Exception = scala.meta.ui.Exception
  val Exception = scala.meta.ui.Exception

  type Code[T] = scala.meta.ui.Code[T]
  val Code = scala.meta.ui.Code

  type Raw[T] = scala.meta.ui.Raw[T]
  val Raw = scala.meta.ui.Raw

  type Semantics[T] = scala.meta.ui.Semantics[T]
  val Semantics = scala.meta.ui.Semantics

  // NOTE: I wish there was a way to avoid duplication and ambiguities wrt org.scalameta.show
  implicit class ShowOps[T](val x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }
}