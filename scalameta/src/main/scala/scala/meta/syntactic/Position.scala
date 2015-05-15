package scala.meta
package syntactic

import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Position {
  def input: Input
  def start: Point
  def end: Point
}

object Position {
  @leaf object None extends Position {
    def input = Input.None
    def start = Point.None
    def end = Point.None
  }
  @leaf class Range(content: Content, start: Point, end: Point) extends Position {
    def input = content
  }
}

@root trait Point {
  def input: Input
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @leaf object None extends Point {
    def input = Input.None
    def offset = -1
    def line = -1
    def column = -1
  }
  @leaf class Offset(content: Content, offset: Int) extends Point {
    def input = content
    private lazy val (eolCount, eolPos) = {
      var i = 0
      var eolCount = 0
      var eolPos = -1
      while (i < Math.min(offset + 1, content.chars.length)) {
        if (content.chars(i) == '\n') {
          eolCount += 1
          eolPos = i
        }
        i += 1
      }
      (eolCount, eolPos)
    }
    def line: Int = eolCount
    def column: Int = offset - eolPos + 1
  }
}
