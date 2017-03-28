package scala.meta
package parsers

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._
import scala.compat.Platform.EOL

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
