package scala.meta
package internal
package tokenizers

import Chars._
import scala.meta.inputs._
import scala.util.control.NonFatal

private[meta] case class CharArrayReader private (
    buf: Array[Char],
    dialect: Dialect,
    reporter: Reporter,
    /** the last read character */
    var ch: Char = SU,
    /** The offset one past the last read character */
    var begCharOffset: Int = -1, // included
    var endCharOffset: Int = 0, // excluded
    /** The start offset of the current line */
    var lineStartOffset: Int = 0,
    /** The start offset of the line before the current one */
    private var lastLineStartOffset: Int = 0
) {

  def this(input: Input, dialect: Dialect, reporter: Reporter) =
    this(buf = input.chars, dialect = dialect, reporter = reporter)

  import reporter._

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    nextRawChar()
    if (ch < ' ') {
      skipCR()
      potentialLineEnd()
    }
    if (ch == '"' && !dialect.allowMultilinePrograms) {
      readerError("double quotes are not allowed in single-line quasiquotes", at = begCharOffset)
    }
  }

  final def nextCommentChar(): Unit = {
    if (endCharOffset >= buf.length) {
      ch = SU
    } else {
      ch = buf(endCharOffset)
      begCharOffset = endCharOffset
      endCharOffset += 1
      checkLineEnd()
    }
  }

  /**
   * Advance one character, leaving CR;LF pairs intact. This is for use in multi-line strings, so
   * there are no "potential line ends" here.
   */
  final def nextRawChar(): Unit = {
    if (endCharOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(endCharOffset)
      ch = c
      begCharOffset = endCharOffset
      endCharOffset += 1
      if (c == '\\') potentialUnicode()
    }
  }

  def nextNonWhitespace = {
    while (ch == ' ' || ch == '\t') nextRawChar()
    ch
  }

  /** Interpret \\uxxxx escapes */
  private def potentialUnicode() = {
    def evenSlashPrefix: Boolean = {
      var p = endCharOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (endCharOffset - p) % 2 == 0
    }
    def udigit: Int = {
      if (endCharOffset >= buf.length) {
        // Since the positioning code is very insistent about throwing exceptions,
        // we have to point to start of range so our error message can be seen, since
        // we are one past EOF.  This happens with e.g. val x = \ u 1 <EOF>
        readerError("incomplete unicode escape", at = begCharOffset)
        SU
      } else {
        val d = digit2int(buf(endCharOffset), 16)
        if (d >= 0) endCharOffset += 1
        else readerError("error in unicode escape", at = endCharOffset)
        d
      }
    }

    if (endCharOffset < buf.length && buf(endCharOffset) == 'u' && evenSlashPrefix) {
      do endCharOffset += 1 while (endCharOffset < buf.length && buf(endCharOffset) == 'u')
      try {
        val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
        ch = code.toChar
      } catch {
        case NonFatal(_) =>
      }
    }
  }

  /** replace CR;LF by LF */
  private def skipCR() =
    if (ch == CR && endCharOffset < buf.length && buf(endCharOffset) == '\\') {
      val lookahead = lookaheadReader
      lookahead.endCharOffset += 1 // skip the backslash
      lookahead.potentialUnicode()
      if (lookahead.ch == LF) {
        ch = LF
        endCharOffset = lookahead.endCharOffset
      }
    }

  /** Handle line ends */
  private def potentialLineEnd(): Unit = {
    if (checkLineEnd() && !dialect.allowMultilinePrograms) {
      readerError("line breaks are not allowed in single-line quasiquotes", at = begCharOffset)
    }
  }

  private def checkLineEnd(): Boolean = {
    val ok = ch == LF || ch == FF
    if (ok) {
      lastLineStartOffset = lineStartOffset
      lineStartOffset = endCharOffset
    }
    ok
  }

  /** A new reader that takes off at the current character position */
  def lookaheadReader = copy()

  /** A mystery why CharArrayReader.nextChar() returns Unit */
  def getc() = { nextChar(); ch }

  final def wasMultiChar: Boolean = begCharOffset < endCharOffset - 1

}
