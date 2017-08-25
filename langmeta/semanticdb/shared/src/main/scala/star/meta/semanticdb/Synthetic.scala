package lang.meta
package semanticdb

import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class Synthetic(position: Position, text: String, names: List[ResolvedName]) {
  def input = Input.Synthetic(text, position.input, position.start, position.end)
  def syntax = {
    val s_names = ResolvedName.syntax(names)
    s"[${position.start}..${position.end}): $text$s_names"
  }
  def structure = s"""Synthetic(${position.structure}, "$text", List(${names.map(_.structure).mkString(", ")}))"""
  override def toString = syntax
}
