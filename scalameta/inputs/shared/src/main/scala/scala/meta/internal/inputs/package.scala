package scala.meta.internal

import scala.compat.Platform.EOL
import scala.meta.inputs._
import scala.meta.internal.{semanticdb => s}

package object inputs {
  implicit class XtensionPositionFormatMessage(pos: Position) {
    def formatMessage(severity: String, message: String): String = {
      // WONTFIX: https://github.com/scalameta/scalameta/issues/383
      if (pos != Position.None) {
        val input = pos.input
        val header = s"${input.syntax}:${pos.startLine + 1}: $severity: $message"
        val line = {
          val start = input.lineToOffset(pos.startLine)
          val notEof = start < input.chars.length
          val end = if (notEof) input.lineToOffset(pos.startLine + 1) else start
          new String(input.chars, start, end - start).stripLineEnd
        }
        var caret = " " * pos.startColumn + "^"
        header + EOL + line + EOL + caret
      } else {
        s"$severity: $message"
      }
    }
  }

  implicit class XtensionInputSyntaxStructure(input: Input) {
    def syntax: String = input match {
      case Input.None => "<none>"
      case Input.File(path, _) => path.toString
      case Input.VirtualFile(path, _) => path
      case _ => "<input>"
    }
    def structure: String = input.toString
  }

  implicit class XtensionPositionToRange(pos: Position) {
    def toRange: s.Range = s.Range(
      startLine = pos.startLine,
      startCharacter = pos.startColumn,
      endLine = pos.endLine,
      endCharacter = pos.endColumn
    )
  }

  implicit class XtensionRangeToPosition(range: s.Range) {
    def toPosition(input: Input): Position = Position.Range(
      input = input,
      startLine = range.startLine,
      startColumn = range.startCharacter,
      endLine = range.endLine,
      endColumn = range.endCharacter
    )
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
