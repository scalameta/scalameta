package org.langmeta.semanticdb

import org.langmeta.inputs._
import org.langmeta.internal.inputs._

final case class Message(position: Position, severity: Severity, text: String) {
  def syntax = s"[${position.start}..${position.end}): ${severity.syntax} $text"
  def structure = s"""Message(${position.structure}, ${severity.structure}, "$text")"""
  override def toString = syntax
}
