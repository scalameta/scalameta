package scala.meta
package internal

import scala.compat.Platform.EOL
import scala.meta.inputs._

package object inputs {
  implicit class XtensionPositionFormatMessage(pos: Position) {
    def formatMessage(severity: String, message: String): String = {
      // TODO: In order to be completely compatible with scalac, we need to support Position.point.
      // On the other hand, do we really need to? Let's try without it. See #383 for discussion.
      pos.start.formatMessage(severity, message)
    }
  }

  implicit class XtensionPointFormatMessage(point: Point) {
    def formatMessage(severity: String, message: String): String = {
      if (point != Point.None) {
        val input = point.input
        val shortContent = input match {
          case Input.File(path, _) => path.toString
          case Input.LabeledString(label, _) => label
          case _ => "<input>"
        }
        val header = s"$shortContent:${point.line + 1}: $severity: $message"
        val line = {
          val start = input.lineToOffset(point.line)
          val end = if (start < input.chars.length) input.lineToOffset(point.line + 1) else start
          new String(input.chars, start, end - start).trim
        }
        var caret = " " * point.column + "^"
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
      case Input.File(path, _) => scala.util.Try(path.toRelative.toString).getOrElse(path.toString)
      case Input.LabeledString(label, _) => label
      case _ => "<input>"
    }
    def structure: String = input.toString
  }

  implicit class XtensionPositionSyntaxStructure(pos: Position) {
    def syntax: String = pos match {
      case Position.None => s"<none>"
      case Position.Range(input, start, end) => s"${input.syntax}@${start.syntax}..${end.syntax}"
    }
    def structure: String = pos match {
      case Position.None => s"Position.None"
      case Position.Range(input, start, end) => s"Position.Range(${input.structure}, ${start.structure}, ${end.structure})"
    }
  }

  implicit class XtensionPointSyntaxStructure(point: Point) {
    def syntax: String = point match {
      case Point.None => s"<none>"
      case Point.Offset(input, offset) => s"${input.syntax}@${offset}"
    }
    def structure: String = point match {
      case Point.None => s"Point.None"
      case Point.Offset(input, offset) => s"Point.Offset(${input.structure}, ${offset})"
    }
  }
}