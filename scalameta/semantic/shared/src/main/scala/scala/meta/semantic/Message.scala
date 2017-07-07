package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._

@data class Message(position: Position, severity: Severity, message: String) {
  def syntax = s"[${severity.toString.toLowerCase}] ${position.syntax}: $message"
  def structure = s"""Message(${position.structure}, Severity.$severity, "$message")"""
  override def toString = syntax
}

@root trait Severity
object Severity {
  @leaf object Info extends Severity
  @leaf object Warning extends Severity
  @leaf object Error extends Severity
}
