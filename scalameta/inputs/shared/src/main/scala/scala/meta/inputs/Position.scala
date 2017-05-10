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
  def syntax: String
  def structure: String
}

object Position {
  @none object None extends Position {
    def input = Input.None
    def start = Point.None
    def end = Point.None
    def text = ""
    def syntax = "<none>"
    def structure = "Position.None"
    override def toString = structure
  }

  @leaf class Range(input: Input @nonEmpty, start: Point @nonEmpty, end: Point @nonEmpty) extends Position {
    if (!(start.offset <= end.offset)) throw new IllegalArgumentException(s"$this is not a valid range")
    override def text = new String(input.chars, start.offset, end.offset - start.offset)
    def syntax = s"[${start.offset}..${end.offset})"
    def structure = s"Position.Range(${input.structure}, ${start.offset}, ${end.offset})"
    override def toString = structure
  }
  object Range {
    def apply(input: Input, start: Point, end: Point): Position = {
      new Range(input, start, end)
    }
    def apply(input: Input, start: Int, end: Int): Position = {
      new Range(input, Point.Offset(input, start), Point.Offset(input, end))
    }
    // Custom unapply so that most users won't have to match against RangeWithPoint.
    def unapply(position: Position): Option[(Input, Point, Point)] = position match {
      case r: Range => Some((r.input, r.start, r.end))
      case RangeWithPoint(input, start, _, end) => Some((input, start, end))
      case _ => scala.None
    }
  }

  // the `point` field is inherited from scala-reflect's Position. In scala-reflect,
  // `point` is the offset where the caret ^ should appear. Offset positions don't have
  // start/end, only point. In scala-reflect RangePosition, the point is often matched
  // with end and is therefore redundant. The commit
  // https://github.com/scalameta/scalameta/commit/7a193ed47638f6a6b3216814866ae95cc7b57344
  // removed Position.Range.point from scalameta. However, it turns out that in some
  // cases the `point` field is necessary even for range positions because: start != point != end.
  // On such case is "unused import" warnings, related discussion: https://github.com/scalameta/scalameta/issues/839
  // Instead of adding a third field `point` to Range, we use `RangeWithPoint` to
  // encode these (hopefully) rare cases where start != point != end.
  // Ideally, most scalameta users should have to worry about RangeWithPoint as long
  // as they construct Position.Range and use Position.Range.unapply.
  @leaf class RangeWithPoint(input: Input @nonEmpty, start: Point @nonEmpty, point: Point @nonEmpty, end: Point @nonEmpty) extends Position {
    if (!(start.offset < point.offset && point.offset < end.offset)) throw new IllegalArgumentException(s"$this is not a valid range")
    override def text = new String(input.chars, start.offset, end.offset - start.offset)
    def syntax = s"[${start.offset}..${point.offset}..${end.offset})"
    def structure = s"Position.RangeWithPoint(${input.structure}, ${start.offset}, ${point.offset}, ${end.offset})"
    override def toString = structure
  }
  object RangeWithPoint {
    def apply(input: Input, start: Point, point: Point, end: Point): Position = {
      new RangeWithPoint(input, start, point, end)
    }
    def apply(input: Input, start: Int, point: Int, end: Int): Position = {
      new RangeWithPoint(input, Point.Offset(input, start), Point.Offset(input, point), Point.Offset(input, end))
    }
  }
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait PositionLiftables extends AdtLiftables with InputLiftables with PointLiftables {
  lazy implicit val liftablePosition: u.Liftable[Position] = materializeAdt[Position]
}
