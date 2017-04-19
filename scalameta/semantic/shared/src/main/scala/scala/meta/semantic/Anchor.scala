package scala.meta
package semantic

import org.scalameta.data._
import scala.meta.prettyprinters._
import scala.meta.io.AbsolutePath

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Anchor.end can point to the last character plus one.
@data class Anchor(path: AbsolutePath, start: Int, end: Int) {
  override def toString = syntax
  def syntax = s"${path.absolute}@$start..$end"
  def structure = s"""Anchor(${path.absolute}, $start, $end)"""
}
object Anchor {
  def apply(path: AbsolutePath, start: Int, end: Int): Anchor = new Anchor(path, start, end)
  def apply(path: String, start: Int, end: Int): Anchor = apply(AbsolutePath.fromAbsoluteOrRelative(path), start, end)
  def apply(file: java.io.File, start: Int, end: Int): Anchor = apply(AbsolutePath(file), start, end)
}
