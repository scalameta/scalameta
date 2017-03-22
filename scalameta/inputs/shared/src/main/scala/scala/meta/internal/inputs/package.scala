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
        val shortContent = input.location
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
}