package lang.meta
package semanticdb

import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class Message(pos: Position, severity: Severity, text: String) {
  def syntax = s"[${pos.start}..${pos.end}): ${severity.syntax} $text"
  def structure = s"""Message(${pos.structure}, ${severity.structure}, "$text")"""
  override def toString = syntax
}
