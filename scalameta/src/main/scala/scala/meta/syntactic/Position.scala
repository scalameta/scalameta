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
  @leaf class Real(input: Input.Real, startToken: Token, endToken: Token) extends Position {
    lazy val start = Point.Start(startToken)
    lazy val end = Point.End(endToken)
  }
  @leaf class Virtual(input: Input.Virtual) extends Position {
    lazy val start = Point.Virtual(input)
    lazy val end = Point.Virtual(input)
  }
}

@root trait Point {
  def input: Input
  def offset: Int
  def line: Int
  def column: Int
}

object Point {
  @branch trait Real extends Point {
    def token: Token
    def offset: Int
    private lazy val (eolCount, eolPos) = {
      var i = 0
      var eolCount = 0
      var eolPos = -1
      while (i < Math.min(offset + 1, input.content.length)) {
        if (input.content(i) == '\n') {
          eolCount += 1
          eolPos = i
        }
        i += 1
      }
      (eolCount, eolPos)
    }
    def input: Input.Real = token.input.require[Input.Real]
    def line: Int = eolCount
    def column: Int = offset - eolPos + 1
  }
  @leaf class Start(token: Token) extends Real {
    def offset: Int = token.start
  }
  @leaf class End(token: Token) extends Real {
    def offset: Int = token.end
  }
  @leaf class Virtual(input: Input.Virtual) extends Point {
    def offset = -1
    def line = -1
    def column = -1
  }
}
