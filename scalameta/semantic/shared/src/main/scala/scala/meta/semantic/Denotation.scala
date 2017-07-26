package scala.meta
package semantic

import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._

@data class Denotation(flags: Long, name: String, info: String, position: Position) extends HasFlags {
  def syntax = s"$flagSyntax $name ${position.syntax}" + (if (info != "") ": " + info else "")
  def structure = s"""Denotation($flagStructure, "$name", "$info", "$position")"""
  override def toString = syntax
}
