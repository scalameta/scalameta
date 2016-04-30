package scala.meta
package inputs

import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.common._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
@root trait Position extends Optional {
  def input: Input
  def start: Point
  def point: Point
  def end: Point
}

object Position {
  @none object None extends Position {
    def input = Input.None
    def start = Point.None
    def point = Point.None
    def end = Point.None
    override def toString = "Position.None"
  }

  @leaf class Range(input: Input @nonEmpty, start: Point @nonEmpty, point: Point @nonEmpty, end: Point @nonEmpty) extends Position {
    if (!((start.offset <= end.offset) && (start.offset <= point.offset) && (point.offset <= end.offset))) {
      throw new IllegalArgumentException(s"$rangeString is not a valid range")
    }
    private def rangeString = s"[${start.offset}..${point.offset}..${end.offset})"
    override def toString = s"$rangeString in $input"
  }
  object Range {
    def apply(input: Input, start: Point, point: Point, end: Point): Position = {
      new Range(input, start, point, end)
    }
    def apply(input: Input, start: Int, point: Int, end: Int): Position = {
      new Range(input, Point.Offset(input, start), Point.Offset(input, point), Point.Offset(input, end))
    }
  }
}
