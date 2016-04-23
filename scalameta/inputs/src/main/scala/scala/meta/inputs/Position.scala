package scala.meta
package inputs

import org.scalameta.adt._
import org.scalameta.invariants._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
@root trait Position {
  def content: Content
  def start: Point
  def point: Point
  def end: Point
}

object Position {
  @leaf class Range(content: Content, start: Point, point: Point, end: Point) extends Position {
    if (!((start.offset <= end.offset) && (start.offset <= point.offset) && (point.offset <= end.offset))) {
      throw new IllegalArgumentException(s"$rangeString is not a valid range")
    }
    private def rangeString = s"[${start.offset}..${point.offset}..${end.offset})"
    override def toString = s"$rangeString in $content"
  }
}
