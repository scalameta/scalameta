package scala.meta
package internal
package quasiquotes

import scala.{Seq => _}
import scala.collection.immutable.Seq

object Flatten {
  def unapply[T](x: Option[Seq[T]]): Option[Seq[T]] = x match {
    case Some(xs) => Some(xs)
    case None => Some(Nil)
  }
}