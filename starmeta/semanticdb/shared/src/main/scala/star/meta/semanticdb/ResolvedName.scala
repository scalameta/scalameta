package star.meta
package semanticdb

import star.meta.inputs._
import star.meta.internal.inputs._

final case class ResolvedName(pos: Position, sym: Symbol) {
  def syntax = {
    val text = if (pos.text.nonEmpty) pos.text else "ε"
    s"[${pos.start}..${pos.end}): $text => ${sym.syntax}"
  }
  def structure = s"""ResolvedName(${pos.structure}, ${sym.structure})"""
  override def toString = syntax
}
