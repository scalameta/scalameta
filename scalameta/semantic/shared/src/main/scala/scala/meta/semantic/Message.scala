package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.data._
import org.scalameta.unreachable
import scala.meta.inputs._
import scala.meta.internal.inputs._

@data class Message(position: Position, severity: Severity, message: String) {
  override def toString = syntax
  def syntax = s"[${severity.toString.toLowerCase}] ${position.syntax}: $message"
  def structure = s"""Message(${position.structure}, Severity.$severity, "$message")"""
}

@root trait Severity {
  import Severity._
  def id: Int = this match {
    case Info => 0
    case Warning => 1
    case Error => 2
  }
}
object Severity {
  def fromId(id: Int): Severity = id match {
    case 0 => Info
    case 1 => Warning
    case 2 => Error
    case _ => unreachable
  }

  @leaf object Info extends Severity
  @leaf object Warning extends Severity
  @leaf object Error extends Severity
}
