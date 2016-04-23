package scala.meta
package inputs

import org.scalameta.adt._
import org.scalameta.invariants._

// NOTE: All numbers here are zero-based, namely:
// * Offset 0 is the first character in the content.
// * Line 0 is the first line in the content.
// * Column 0 is the first character in the line.
@root trait Point {
  def content: Content
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @leaf class Offset(content: Content, offset: Int) extends Point {
    if (!(0 <= offset && offset <= content.chars.length)) {
      val message = s"$offset is not a valid offset, allowed [0..${content.chars.length}]"
      throw new IllegalArgumentException(message)
    }
    def line: Int = content.offsetToLine(offset)
    def column: Int = offset - content.lineToOffset(line)
    override def toString = s"$offset in $content"
  }
}
