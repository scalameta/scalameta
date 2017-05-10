package scala.meta
package internal

import scala.compat.Platform.EOL
import scala.meta.inputs._

package object inputs {
  implicit class XtensionPositionFormatMessage(pos: Position) {
    def formatMessage(severity: String, message: String): String = pos match {
      case Position.RangeWithPoint(_, _, point, end) =>
        point.formatMessage(severity, message)
      case _ =>
        pos.start.formatMessage(severity, message)
    }
  }

  implicit class XtensionPointFormatMessage(point: Point) {
    def formatMessage(severity: String, message: String): String = {
      if (point != Point.None) {
        val input = point.input
        val header = s"${input.syntax}:${point.line + 1}: $severity: $message"
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

  // TODO(olafur): Find a better name + home for this method, it's currently used
  // in Message in Symbol. Positions.syntax should not include the input IMO
  // because that would make it unnecessarily verbose. However, in some cases
  // it is nice to include the input.
  implicit class XtensionPositionPretty(position: Position) {
    def syntaxWithInput = s"${position.input.syntax}@${position.start.offset}..${position.end.offset}"
  }
}