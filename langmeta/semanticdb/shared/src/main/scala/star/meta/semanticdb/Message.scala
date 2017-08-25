package lang.meta
package semanticdb

import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class Message(position: Position, severity: Severity, text: String) {
  def syntax = s"[${position.start}..${position.end}): ${severity.syntax} $text"
  def structure = s"""Message(${position.structure}, ${severity.structure}, "$text")"""
  override def toString = syntax
}
