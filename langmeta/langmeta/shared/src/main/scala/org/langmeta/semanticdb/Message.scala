package org.langmeta.semanticdb

import org.langmeta.inputs._
import org.langmeta.internal.inputs._
import org.langmeta.internal.semanticdb.DeprecationMessage

@deprecated(DeprecationMessage, "3.8.0")
final case class Message(position: Position, severity: Severity, text: String) {
  def syntax = s"[${position.start}..${position.end}): ${severity.syntax} $text"
  def structure = s"""Message(${position.structure}, ${severity.structure}, "$text")"""
  override def toString = syntax
}
