package org.scalameta
package default

import scala.language.implicitConversions
import org.scalameta.adt._

sealed trait Param[+T] {
  def isEmpty: Boolean = toOption.isEmpty
  def nonEmpty: Boolean = toOption.nonEmpty
  def get: T = toOption.get
  def getOrElse[U >: T](default: U): U = toOption.getOrElse(default)
  def toOption: Option[T]
}

object Param {
  final case class Explicit[+T](value: T) extends Param[T] { def toOption = Some(value) }
  final case object Default extends Param[Nothing] { def toOption = None }
  implicit def valueToExplicit[T](value: T): Explicit[T] = new Explicit[T](value)
}