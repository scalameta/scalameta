package scala.meta
package internal

import scala.compat.Platform.EOL
import scala.meta.inputs._

package object inputs {
  implicit class XtensionPositionFormatMessage(position: Position) {
    def formatMessage(severity: String, message: String): String = {
      position.point.formatMessage(severity, message)
    }
  }

  implicit class XtensionPointFormatMessage(point: Point) {
    def formatMessage(severity: String, message: String): String = {
      val content = point.content
      val shortContent = content match { case Input.File(file, _) => file.getName; case _ => "<content>" }
      val header = s"$shortContent:${point.line + 1}: $severity: $message"
      val line = {
        val start = content.lineToOffset(point.line)
        val end = if (start < content.chars.length) content.lineToOffset(point.line + 1) else start
        new String(content.chars, start, end - start).trim
      }
      var caret = " " * point.column + "^"
      header + EOL + line + EOL + caret
    }
  }
}