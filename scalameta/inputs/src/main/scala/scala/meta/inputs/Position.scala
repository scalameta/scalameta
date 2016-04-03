package scala.meta
package inputs

import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Position {
  def content: Content
  def start: Point
  def point: Point
  def end: Point
}

object Position {
  @leaf class Range(content: Content, start: Point, point: Point, end: Point) extends Position
  object Range {
    def apply(content: Content, start: Point, point: Point, end: Point): Position = {
      new Position.Range(content, start, point, end)
    }
  }
}

@root trait Point {
  def content: Content
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @leaf class Offset(content: Content, offset: Int) extends Point {
    private lazy val (eolCount, eolPos) = {
      var i = 0
      var eolCount = 0
      var eolPos = -1
      while (i < Math.min(offset, content.chars.length)) {
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
