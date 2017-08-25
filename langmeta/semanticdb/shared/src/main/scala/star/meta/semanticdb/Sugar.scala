package lang.meta
package semanticdb

import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class Sugar(position: Position, text: String, names: List[ResolvedName]) {
  def input = Input.Sugar(text, position.input, position.start, position.end)
  def syntax = {
    val s_names = ResolvedName.syntax(names)
    s"[${position.start}..${position.end}): $text$s_names"
  }
  def structure = s"""Sugar(${position.structure}, "$text", List(${names.map(_.structure).mkString(", ")}))"""
  override def toString = syntax
}
