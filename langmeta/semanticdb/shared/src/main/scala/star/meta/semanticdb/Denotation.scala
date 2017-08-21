package lang.meta
package semanticdb

final case class Denotation(flags: Long, name: String, info: String, resolvedNames: List[ResolvedName]) extends HasFlags {
  def syntax: String = {
    val s_info = if (info != "") ": " + info else ""
    val s_names = ResolvedName.syntax(resolvedNames)
    s"$flagSyntax $name" + s_info + s_names
  }
  def structure = s"""Denotation($flagStructure, "$name", "$info")"""
  override def toString = syntax
}
