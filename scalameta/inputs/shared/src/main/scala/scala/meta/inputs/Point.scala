package scala.meta
package inputs

import org.scalameta.adt.{Liftables => AdtLiftables}
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.common._

// NOTE: All numbers here are zero-based, namely:
// * Offset 0 is the first character in the input.
// * Line 0 is the first line in the input.
// * Column 0 is the first character in the line.
// -1 is the sentinel value used in Point.None.
@root trait Point extends Optional {
  def input: Input
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @none object None extends Point {
    def input = Input.None
    def offset = -1
    def line = -1
    def column = -1
    override def toString = "Point.None"
  }

  @leaf class Offset(input: Input @nonEmpty, offset: Int) extends Point {
    def line: Int = input.offsetToLine(offset)
    def column: Int = offset - input.lineToOffset(line)
    override def toString = s"$offset in $input"
  }
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait PointLiftables extends AdtLiftables with InputLiftables {
  lazy implicit val liftablePoint: u.Liftable[Point] = materializeAdt[Point]
}
