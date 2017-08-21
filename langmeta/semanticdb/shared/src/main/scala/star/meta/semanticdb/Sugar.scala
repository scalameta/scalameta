package lang.meta
package semanticdb

import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class Sugar(pos: Position, text: String, names: List[ResolvedName]) {
  def input = Input.Sugar(text, pos.input, pos.start, pos.end)
  def syntax = {
    val s_names = ResolvedName.syntax(names)
    s"[${pos.start}..${pos.end}): $text$s_names"
  }
  def structure = s"""Sugar(${pos.structure}, "$text", List(${names.map(_.structure).mkString(", ")}))"""
  override def toString = syntax
}
