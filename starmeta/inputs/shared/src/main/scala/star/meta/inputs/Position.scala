package star.meta
package inputs

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
//
// NOTE: All numbers here are zero-based, namely:
// * Offset 0 is the first character in the input.
// * Line 0 is the first line in the input.
// * Column 0 is the first character in the line.
// -1 is the sentinel value used in Position.None.
sealed trait Position {
  def input: Input
  def start: Int
  def startLine: Int
  def startColumn: Int
  def end: Int
  def endLine: Int
  def endColumn: Int
  def text: String
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
    def startLine: Int = input.offsetToLine(start)
    def startColumn: Int = start - input.lineToOffset(startLine)
    def endLine: Int = input.offsetToLine(end)
    def endColumn: Int = end - input.lineToOffset(endLine)
    override def text = new String(input.chars, start, end - start)
    override def toString = s"[$start..$end) in $input"
  }
}
