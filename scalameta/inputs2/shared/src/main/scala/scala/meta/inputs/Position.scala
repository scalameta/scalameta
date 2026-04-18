package scala.meta.inputs

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

  final def isEmpty: Boolean = start >= end
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
    def text = ""
    override def toString = "Position.None"
  }

  final case class Range(input: Input, start: Int, end: Int) extends Position {
    lazy val (startLine, startColumn) = input.offsetToLineAndColumn(start)
    lazy val (endLine, endColumn) = input.offsetToLineAndColumn(end)
    lazy val text = if (isEmpty) "" else new String(input.chars, start, end - start)
  }

  object Range {
    @inline
    private def getPositionOffset(lineOffset: Int, lineColumn: Int, lineLength: Int): Int =
      lineOffset + math.min(lineColumn, lineLength)

    def exclusive(input: Input, start: Int, end: Int): Range = new Range(input, start, end)
    def inclusive(input: Input, start: Int, end: Int): Range = new Range(input, start, end + 1)

    def apply(input: Input, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Range = {
      val (begLineOff, begLineLen) = input.lineToOffsetAndLength(startLine)
      val (endLineOff, endLineLen) =
        if (endLine == startLine) (begLineOff, begLineLen) else input.lineToOffsetAndLength(endLine)
      val beg = getPositionOffset(begLineOff, startColumn, begLineLen)
      val end = getPositionOffset(endLineOff, endColumn, endLineLen)
      exclusive(input, beg, end)
    }
  }
}
