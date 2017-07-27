package star.meta
package semanticdb

final case class Denotation(flags: Long, name: String, info: String) extends HasFlags {
  def syntax = s"$flagSyntax $name" + (if (info != "") ": " + info else "")
  def structure = s"""Denotation($flagStructure, "$name", "$info")"""
  override def toString = syntax
}
