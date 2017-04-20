package scala.meta
package semantic

import org.scalameta.data._
import scala.meta.io._
import scala.meta.prettyprinters._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Anchor.end can point to the last character plus one.
@data class Anchor(path: RelativePath, start: Int, end: Int) {
  override def toString = syntax
  def syntax = s"$path@$start..$end"
  def structure = s"""Anchor("$path", $start, $end)"""
}
object Anchor {
  def apply(path: RelativePath, start: Int, end: Int): Anchor = new Anchor(path, start, end)
}
