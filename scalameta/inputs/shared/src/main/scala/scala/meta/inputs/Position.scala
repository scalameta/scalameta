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

  override def toString = s"[$start..$end) in $input"
}

sealed trait Position extends InputRange {
  def startLine: Int
  def startColumn: Int
  def endLine: Int
  def endColumn: Int

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
    lazy val startLine: Int = input.offsetToLine(start)
    def startColumn: Int = start - input.lineToOffset(startLine)
    lazy val endLine: Int = input.offsetToLine(end)
    def endColumn: Int = end - input.lineToOffset(endLine)
    lazy val text = if (isEmpty) "" else new String(input.chars, start, end - start)
  }

  object Range {
    @inline
    private def getPositionOffset(lineOffset: Int, lineColumn: Int, lineLength: Int): Int =
      lineOffset + math.min(lineColumn, lineLength)

    def apply(input: Input, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Range = {
      require(startLine <= endLine)

      def lineOffsetAndLength(line: Int): (Int, Int) = {
        val off = input.lineToOffset(line)
        (off, input.lineToOffset(line + 1) - off - 1)
      }

      val (endLineOff, endLineLen) = {
        val inputEndOff = input.chars.length
        val inputLastLine = input.offsetToLine(inputEndOff)
        if (inputLastLine == endLine) {
          val inputLastLineOff = input.lineToOffset(inputLastLine)
          (inputLastLineOff, inputEndOff - inputLastLineOff)
        } else lineOffsetAndLength(endLine)
      }

      val (begLineOff, begLineLen) =
        if (endLine == startLine) (endLineOff, endLineLen) else lineOffsetAndLength(startLine)

      val beg = getPositionOffset(begLineOff, startColumn, begLineLen)
      val end = getPositionOffset(endLineOff, endColumn, endLineLen)

      Position.Range(input, beg, end)
    }
  }
}
