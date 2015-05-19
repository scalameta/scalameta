package scala.meta
package ui

import scala.language.higherKinds
import org.scalameta.show._

private[meta] trait Api {
  type Code[T] = scala.meta.ui.Code[T]
  type Raw[T] = scala.meta.ui.Raw[T]
  type Semantics[T] = scala.meta.ui.Semantics[T]
  type Positions[T] = scala.meta.ui.Positions[T]

  // NOTE: I wish there was a way to avoid duplication and ambiguities wrt org.scalameta.show
  implicit class XtensionShow[T](x: T) {
    def show[Style[X] <: Show[X]](implicit style: Style[T]): String = style(x).toString
  }

  def abort(msg: String): Nothing = throw new AbortException(msg)
  def abort(pos: Position, msg: String): Nothing = throw new AbortException(pos, msg)
}