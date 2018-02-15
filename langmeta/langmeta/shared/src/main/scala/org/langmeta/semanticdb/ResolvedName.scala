package org.langmeta.semanticdb

import scala.compat.Platform.EOL
import org.langmeta.inputs._
import org.langmeta.internal.inputs._

final case class ResolvedName(position: Position, symbol: Symbol, isDefinition: Boolean) {
  def syntax: String = {
    val text = if (position.text.nonEmpty) position.text else ""
    val binder = if (isDefinition) "<=" else "=>"
    s"[${position.start}..${position.end}): $text $binder ${symbol.syntax}"
  }
  def structure = s"""ResolvedName(${position.structure}, ${symbol.structure}, $isDefinition)"""
  override def toString = syntax
}

object ResolvedName {
  def syntax(names: List[ResolvedName]): String = {
    if (names.isEmpty) ""
    else names.map(name => "  " + name.syntax).mkString(EOL, EOL, "")
  }
}
