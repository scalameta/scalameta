package org.langmeta.internal

import scala.compat.Platform.EOL
import org.langmeta.inputs._

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
          new String(input.chars, start, end - start).stripLineEnd
        }
        var caret = " " * pos.startColumn + "^"
        header + EOL + line + EOL + caret
      } else {
        s"$severity: $message"
      }
    }
  }

  
  @deprecated("Use Input.syntax or Input.structure", "2.1.4")
  implicit class XtensionInputSyntaxStructure(input: Input) {
    def syntax: String = input.syntax
    def structure: String = input.structure
  }

  @deprecated("Use Position.syntax or Position.structure", "2.1.4")
  implicit class XtensionPositionSyntaxStructure(pos: Position) {
    def syntax: String = pos.syntax
    def structure: String = pos.structure
  }
}
