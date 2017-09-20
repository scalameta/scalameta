package scala.meta
package parsers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._

@root trait Parsed[+T] {

  def fold[A](fe: Parsed.Error => A, ft: T => A): A = this match {
    case Parsed.Success(t) => ft(t)
    case e: Parsed.Error => fe(e)
  }

  def get: T = fold(e => throw e.details, identity)
  def orElse[U >: T](alt: => Parsed[U]): Parsed[U] = fold(_ => alt, _ => this)
  def getOrElse[U >: T](alt: => U): U = fold(_ => alt, identity)

  def toOption: Option[T] = fold(_ => None, t => Some(t))
  def toEither: Either[Parsed.Error, T] = fold(e => Left(e), t => Right(t))

}

object Parsed {
  @leaf class Success[+T](tree: T) extends Parsed[T] {
    override def toString = tree.toString
  }
  @leaf class Error(pos: Position, message: String, details: Exception) extends Parsed[Nothing] {
    override def toString = details.toString
  }
}

@data class ParseException(pos: Position, shortMessage: String)
extends Exception(pos.formatMessage("error", shortMessage)) {
  def fullMessage = getMessage
  override def toString = fullMessage
}
