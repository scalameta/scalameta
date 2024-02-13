package scala.meta
package parsers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._

trait Parsed[+T] extends Product with Serializable with Metadata.Adt with Equals {

  def fold[A](fe: Parsed.Error => A, ft: T => A): A = this match {
    case x: Parsed.Success[_] => ft(x.tree)
    case e: Parsed.Error => fe(e)
  }

  def get: T = fold(e => throw e.details, identity)
  def orElse[U >: T](alt: => Parsed[U]): Parsed[U] = fold(_ => alt, _ => this)
  def getOrElse[U >: T](alt: => U): U = fold(_ => alt, identity)

  def toOption: Option[T] = fold(_ => None, t => Some(t))
  def toEither: Either[Parsed.Error, T] = fold(e => Left(e), t => Right(t))

}

object Parsed {
  case class Success[+T](tree: T) extends Parsed[T] {
    override def toString = tree.toString
  }
  case class Error(pos: Position, message: String, details: Exception) extends Parsed[Nothing] {
    override def toString = details.toString
  }
}

case class ParseException(pos: Position, shortMessage: String)
    extends InputException(pos, shortMessage)
