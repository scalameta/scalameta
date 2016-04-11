package scala.meta
package parsers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._

@root trait Parsed[+T] {
  def get: T = this match {
    case Parsed.Success(tree) => tree
    case Parsed.Error(_, _, details) => throw details
  }
  def orElse[U >: T](alt: => Parsed[U]): Parsed[U] = this match {
    case Parsed.Success(_) => this
    case _ => alt
  }
  def getOrElse[U >: T](alt: => U): U = this match {
    case Parsed.Success(tree) => tree
    case _ => alt
  }
}

object Parsed {
  @leaf class Success[+T](tree: T) extends Parsed[T]
  @leaf class Error(pos: Position, message: String, details: ParseException) extends Parsed[Nothing] {
    override def toString = s"Error($details)"
  }
}

@data class ParseException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") {
  override def toString = super.toString
}
