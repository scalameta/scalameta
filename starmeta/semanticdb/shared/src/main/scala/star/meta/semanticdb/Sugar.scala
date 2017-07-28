package star.meta
package semanticdb

import scala.compat.Platform.EOL
import star.meta.inputs._
import star.meta.internal.inputs._

final case class Sugar(pos: Position, text: String, names: List[ResolvedName]) {
  def input = Input.Sugar(text, pos.input, pos.start, pos.end)
  def syntax = {
    val s_names = if (names.isEmpty) "" else names.map(name => "  " + name.syntax).mkString(EOL, EOL, "")
    s"[${pos.start}..${pos.end}): $text$s_names"
  }
  def structure = s"""Sugar(${pos.structure}, "$text", List(${names.map(_.structure).mkString(", ")}))"""
  override def toString = syntax
}
