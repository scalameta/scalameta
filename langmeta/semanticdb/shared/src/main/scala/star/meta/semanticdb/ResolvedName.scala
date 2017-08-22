package lang.meta
package semanticdb

import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class ResolvedName(pos: Position, symbol: Symbol, isBinder: Boolean) {
  def syntax = {
    val text = if (pos.text.nonEmpty) pos.text else "ε"
    val binder = if (isBinder) "<=" else "=>"
    s"[${pos.start}..${pos.end}): $text $binder ${symbol.syntax}"
  }
  def structure = s"""ResolvedName(${pos.structure}, ${symbol.structure}, $isBinder)"""
  override def toString = syntax
}
