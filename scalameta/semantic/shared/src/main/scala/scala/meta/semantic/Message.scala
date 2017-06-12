package scala.meta
package semantic

import org.scalameta.adt._
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._
import scala.meta.semantic.Message.Kind

@data
class Message(position: Position, severity: Severity, message: String, kind: Kind = Kind.None) {
  def syntax = s"[${severity.toString.toLowerCase}] ${position.syntax}: $message"
  def structure = s"""Message(${position.structure}, Severity.$severity, "$message")"""
  override def toString = syntax
}

object Message {
  @root
  trait Kind {
    def syntax = if (this.isNone) "" else this.toString
    def structure = s"Kind.${super.toString}"
    def isNone = this == Kind.None
    def isUnusedImport = this == Kind.UnusedImport
  }
  object Kind {
    @leaf object None extends Kind
    @leaf object UnusedImport extends Kind
    @leaf object AdaptedArg extends Kind
  }
}

@root trait Severity
object Severity {
  @leaf object Info extends Severity
  @leaf object Warning extends Severity
  @leaf object Error extends Severity
}
