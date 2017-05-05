package scala.meta
package inputs

import org.scalameta.adt.{Liftables => AdtLiftables}
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.common._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
@root trait Position extends Optional {
  def input: Input
  def start: Point
  def end: Point
  def text: String
}

object Position {
  @none object None extends Position {
    def input = Input.None
    def start = Point.None
    def end = Point.None
    def text = ""
    override def toString = "Position.None"
  }

  @leaf class Range(input: Input @nonEmpty, start: Point @nonEmpty, end: Point @nonEmpty) extends Position {
    if (!(start.offset <= end.offset)) throw new IllegalArgumentException(s"$this is not a valid range")
    override def text = new String(input.chars, start.offset, end.offset - start.offset)
    override def toString = s"[${start.offset}..${end.offset}) in $input"
  }
  object Range {
    def apply(input: Input, start: Point, end: Point): Position = {
      new Range(input, start, end)
    }
    def apply(input: Input, start: Int, end: Int): Position = {
      new Range(input, Point.Offset(input, start), Point.Offset(input, end))
    }
  }
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait PositionLiftables extends AdtLiftables with InputLiftables with PointLiftables {
  lazy implicit val liftablePosition: u.Liftable[Position] = materializeAdt[Position]
}
