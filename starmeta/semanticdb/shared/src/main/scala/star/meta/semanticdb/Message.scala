package star.meta
package semanticdb

import star.meta.inputs._
import star.meta.internal.inputs._

final case class Message(position: Position, severity: Severity, message: String) {
  def syntax = s"[${severity.toString.toLowerCase}] ${position.syntax}: $message"
  def structure = s"""Message(${position.structure}, Severity.$severity, "$message")"""
  override def toString = syntax
}

sealed trait Severity
object Severity {
  case object Info extends Severity
  case object Warning extends Severity
  case object Error extends Severity
}
