package scala.meta.internal

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.inputs._

package object inputs {

  implicit class XtensionInputRange(private val obj: InputRange) extends AnyVal {
    def desc: String = obj match {
      case Position.None => "<none>"
      case x =>
        val text = x.text.trim
        val excerpt =
          if (text.isEmpty) text
          else {
            val nl = text.indexOf('\n')
            val prefix = if (x.text.head <= ' ') "..." else ""
            val suffix = if (x.text.last <= ' ' || nl >= 0) "..." else ""
            val slice = if (nl < 0) text else text.substring(0, nl)
            s"$prefix$slice$suffix"
          }
        val (lsep, rsep) =
          if (text.isEmpty || text.head != ':' && text.last != ':') (':', ':') else ('<', '>')
        s"[${x.start}$lsep$excerpt$rsep${x.end})"
    }
  }

  implicit class XtensionPosition(private val pos: Position) extends AnyVal {
    def lineContent: String = {
      val input = pos.input
      val start = input.lineToOffset(pos.startLine)
      val notEof = start < input.chars.length
      val end = if (notEof) input.lineToOffset(pos.startLine + 1) else start
      new String(input.chars, start, end - start).stripLineEnd
    }
    def lineCaret: String = " " * pos.startColumn + "^"
    def formatMessage(severity: String, message: String): String =
      // WONTFIX: https://github.com/scalameta/scalameta/issues/383
      if (pos != Position.None) {
        val input = pos.input
        val header = s"${input.syntax}:${pos.startLine + 1}: $severity: $message"
        val line = lineContent
        val caret = lineCaret
        header + EOL + line + EOL + caret
      } else s"$severity: $message"

    def syntax: String = pos match {
      case Position.None => s"<none>"
      case Position.Range(input, start, end) => s"${input.syntax}@$start..$end"
    }
    def structure: String = pos match {
      case Position.None => s"Position.None"
      case Position.Range(input, start, end) => s"Position.Range(${input.structure}, $start, $end)"
    }
  }

  implicit class XtensionInput(private val input: Input) extends AnyVal {
    def syntax: String = input match {
      case Input.None => "<none>"
      case proxy: Input.Proxy => proxy.input.syntax
      case Input.File(path, _) => path.toString
      case Input.VirtualFile(path, _) => path
      case _ => "<input>"
    }
    def structure: String = input.toString

    def pos(beg: Int, end: Int): Position = Position.Range(input, beg, end)
    def pos(offset: Int): Position = pos(offset, offset)
  }

}
