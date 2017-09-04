package org.langmeta
package semanticdb

final case class Denotation(flags: Long, name: String, signature: String, names: List[ResolvedName]) extends HasFlags {
  def syntax: String = {
    val s_info = if (signature != "") ": " + signature else ""
    val s_names = ResolvedName.syntax(names)
    // TODO(olafur) use more advances escaping.
    val s_name = if (name.contains(" ")) s"`$name`" else name
    s"$flagSyntax $s_name" + s_info + s_names
  }
  def structure = s"""Denotation($flagStructure, "$name", "$signature")"""
  override def toString = syntax
}
