package scala.meta.inputs

import scala.meta.tokens.Tokens

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
//
// NOTE: All numbers here are zero-based, namely:
// * Offset 0 is the first character in the input.
// * Line 0 is the first line in the input.
// * Column 0 is the first character in the line.
// -1 is the sentinel value used in Position.None.

trait InputRange {
  def input: Input
  def start: Int
  def end: Int
  def text: String

  override def toString = {
    implicit val sb = new java.lang.StringBuilder
    Input.getInputSlice(input, start, end)
    sb.toString
  }
}

sealed trait Position extends InputRange {
  def startLine: Int
  def startColumn: Int
  def endLine: Int // exclusive
  def endColumn: Int // exclusive
  def lastLine: Int // inclusive
  def lastColumn: Int // inclusive

  final def isEmpty: Boolean = start >= end
  final def len: Int = end - start
  final def isMultiLine: Boolean = startLine < endLine
}

object Position {
  case object None extends Position {

    def input = Input.None
    def start = -1
    def startLine = -1
    def startColumn = -1
    def end = -1
    def endLine = -1
    def endColumn = -1
    def lastLine = -1
    def lastColumn = -1
    def text = ""
    override def toString = "Position.None"

  }

  final case class Range(input: Input, start: Int, end: Int) extends Position {
    private lazy val startPacked = input.offsetToLineAndColumn(start)
    private lazy val endPacked = input.offsetToLineAndColumn(end)
    def startLine: Int = Tokens.indexOf(startPacked)
    def startColumn: Int = Tokens.countOf(startPacked)
    def endLine: Int = Tokens.indexOf(endPacked)
    def endColumn: Int = Tokens.countOf(endPacked)
    lazy val text = if (isEmpty) "" else new String(input.chars, start, len)
    def lastLine: Int = if (endColumn > 0) endLine else endLine - 1
    def lastColumn: Int =
      if (endColumn > 0) endColumn - 1
      else if (endLine > 0) Tokens.countOf(input.lineToOffsetAndLength(endLine - 1)) - 1
      else 0
  }

  object Range {
    @inline
    private def getPositionOffset(packedLine: Long, lineColumn: Int): Int = Tokens
      .indexOf(packedLine) + math.min(lineColumn, Tokens.countOf(packedLine))

    def exclusive(input: Input, start: Int, end: Int): Range = new Range(input, start, end)
    def inclusive(input: Input, start: Int, end: Int): Range = new Range(input, start, end + 1)

    def apply(input: Input, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Range = {
      val begPacked = input.lineToOffsetAndLength(startLine)
      val endPacked = if (endLine == startLine) begPacked else input.lineToOffsetAndLength(endLine)
      val beg = getPositionOffset(begPacked, startColumn)
      val end = getPositionOffset(endPacked, endColumn)
      exclusive(input, beg, end)
    }
  }
}
