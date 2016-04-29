package scala.meta
package inputs

import org.scalameta.adt._
import org.scalameta.invariants._

// NOTE: All numbers here are zero-based, namely:
// * Offset 0 is the first character in the input.
// * Line 0 is the first line in the input.
// * Column 0 is the first character in the line.
@root trait Point {
  def input: Input
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @leaf class Offset(input: Input, offset: Int) extends Point {
    if (!(0 <= offset && offset <= input.chars.length)) {
      val message = s"$offset is not a valid offset, allowed [0..${input.chars.length}]"
      throw new IllegalArgumentException(message)
    }
    def line: Int = input.offsetToLine(offset)
    def column: Int = offset - input.lineToOffset(line)
    override def toString = s"$offset in $input"
  }
}
