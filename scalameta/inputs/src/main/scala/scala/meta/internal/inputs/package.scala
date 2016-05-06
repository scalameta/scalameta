package scala.meta
package internal

import scala.compat.Platform.EOL
import scala.meta.inputs._

package object inputs {
  implicit class XtensionPositionFormatMessage(pos: Position) {
    def formatMessage(severity: String, message: String): String = {
      pos.point.formatMessage(severity, message)
    }
  }

  implicit class XtensionPointFormatMessage(point: Point) {
    def formatMessage(severity: String, message: String): String = {
      val input = point.input
      val shortContent = input match { case Input.File(file, _) => file.getName; case _ => "<input>" }
      val header = s"$shortContent:${point.line + 1}: $severity: $message"
      val line = {
        val start = input.lineToOffset(point.line)
        val end = if (start < input.chars.length) input.lineToOffset(point.line + 1) else start
        new String(input.chars, start, end - start).trim
      }
      var caret = " " * point.column + "^"
      header + EOL + line + EOL + caret
    }
  }
}