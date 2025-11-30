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
      if (start < input.chars.length) {
        val end = input.lineToOffset(pos.startLine + 1)
        new String(input.chars, start, end - start).stripLineEnd
      } else ""
    }
    def formatMessage(severity: String, message: String): String = {
      // WONTFIX: https://github.com/scalameta/scalameta/issues/383
      implicit val sb = new StringBuilder
      def appendMessage = sb.append(severity).append(": ").append(message)
      if (pos != Position.None) {
        sb.append(pos.input.syntax).append(':').append(pos.startLine + 1).append(": ")
        appendMessage
        sb.append(EOL).append(lineContent).append(EOL)
        var i = 0
        while (i < pos.startColumn) {
          sb.append(' ')
          i += 1
        }
        sb.append('^')
      } else appendMessage
      sb.result()
    }

    def syntax: String = pos match {
      case Position.None => s"<none>"
      case Position.Range(input, start, end) => s"${input.syntax}@$start..$end"
    }
    def structure: String = pos match {
      case Position.None => s"Position.None"
      case Position.Range(input, start, end) => s"Position.Range(${input.structure}, $start, $end)"
    }
    def trunc(len: Int): Position = pos.input.posWithLen(pos.start, len)
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
    def posWithLen(beg: Int, len: Int): Position = pos(beg, beg + len)
  }

}
