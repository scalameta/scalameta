package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._

@root trait Completed[+T] {
  def get: T = this match {
    case Completed.Success(tree) => tree
    case Completed.Error(details) => throw details
  }
  def orElse[U >: T](alt: => Completed[U]): Completed[U] = this match {
    case Completed.Success(_) => this
    case _ => alt
  }
  def getOrElse[U >: T](alt: => U): U = this match {
    case Completed.Success(tree) => tree
    case _ => alt
  }
}

object Completed {
  @leaf class Success[+T](tree: T) extends Completed[T] {
    override def toString = tree.toString
  }
  @leaf class Error(details: Exception) extends Completed[Nothing] {
    override def toString = details.toString
  }
}

@data class SemanticException(pos: Position, shortMessage: String)
extends Exception(pos.formatMessage("error", shortMessage)) {
  def fullMessage = getMessage
  override def toString = fullMessage
}
