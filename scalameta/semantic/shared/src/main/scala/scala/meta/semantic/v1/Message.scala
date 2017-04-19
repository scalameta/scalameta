package scala.meta
package semantic
package v1

import org.scalameta.adt._
import org.scalameta.data._
import org.scalameta.unreachable

@data class Message(location: Location, severity: Severity, message: String) {
  override def toString = syntax
  def syntax = s"[${severity.toString.toLowerCase}] ${location.syntax}: $message"
  def structure = s"""Message(${location.structure}, Severity.$severity, "$message")"""
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
