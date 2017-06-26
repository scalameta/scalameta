package scala.meta
package internal
package quasiquotes

object Flatten {
  def unapply[T](x: Option[List[T]]): Option[List[T]] = x match {
    case Some(xs) => Some(xs)
    case None => Some(Nil)
  }
}