package star.meta.internal

import scala.compat.Platform.EOL
import star.meta.inputs._

package object inputs {
  implicit class XtensionPositionFormatMessage(pos: Position) {
    def formatMessage(severity: String, message: String): String = {
      // TODO: In order to be completely compatible with scalac, we need to support Position.point.
      // On the other hand, do we really need to? Let's try without it. See #383 for discussion.
      if (pos != Position.None) {
        val input = pos.input
        val header = s"${input.syntax}:${pos.startLine + 1}: $severity: $message"
        val line = {
          val start = input.lineToOffset(pos.startLine)
          val notEof = start < input.chars.length
          val end = if (notEof) input.lineToOffset(pos.startLine + 1) else start
          new String(input.chars, start, end - start).trim
        }
        var caret = " " * pos.startColumn + "^"
        header + EOL + line + EOL + caret
      } else {
        s"$severity: $message"
      }
    }
  }

  // TODO: the extension methods below are temporary stubs that should be moved to the public API

  implicit class XtensionInputSyntaxStructure(input: Input) {
    def syntax: String = input match {
      case Input.None => "<none>"
      case Input.File(path, _) => path.toString
      case Input.VirtualFile(path, _) => path
      case _ => "<input>"
    }
    def structure: String = input.toString
  }

  implicit class XtensionPositionSyntaxStructure(pos: Position) {
    def syntax: String = pos match {
      case Position.None => s"<none>"
      case Position.Range(input, start, end) => s"${input.syntax}@$start..$end"
    }
    def structure: String = pos match {
      case Position.None => s"Position.None"
      case Position.Range(input, start, end) => s"Position.Range(${input.structure}, $start, $end)"
    }
  }
}