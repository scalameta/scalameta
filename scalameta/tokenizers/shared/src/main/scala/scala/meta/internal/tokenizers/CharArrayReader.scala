package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.internal.tokens.Chars._

private[meta] case class CharArrayReader private (
    buf: Array[Char],
    dialect: Dialect,
    reporter: Reporter,
    /** the last read character */
    var ch: Int = SU,
    /** The offset one past the last read character */
    var begCharOffset: Int = -1, // included
    var endCharOffset: Int = 0, // excluded
    /** The start offset of the current line */
    var lineStartOffset: Int = 0,
    /** The start offset of the line before the current one */
    private var lastLineStartOffset: Int = 0
) {

  import CharArrayReader._
  import reporter._

  def this(input: Input, dialect: Dialect, reporter: Reporter) =
    this(buf = input.chars, dialect = dialect, reporter = reporter)

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    nextRawChar()
    if (ch < ' ') {
      skipCR()
      potentialLineEnd()
    }
    if (ch == '"' && !dialect.allowMultilinePrograms)
      readerError("double quotes are not allowed in single-line quasiquotes", at = begCharOffset)
  }

  final def nextCommentChar(): Unit =
    if (endCharOffset >= buf.length) ch = SU
    else {
      ch = buf(endCharOffset)
      begCharOffset = endCharOffset
      endCharOffset += 1
      checkLineEnd()
    }

  /**
   * Advance one character, leaving CR;LF pairs intact. This is for use in multi-line strings, so
   * there are no "potential line ends" here.
   */
  final def nextRawChar(): Unit =
    if (endCharOffset >= buf.length) ch = SU
    else {
      begCharOffset = endCharOffset
      val (hi, hiEnd) = readUnicodeChar(buf, endCharOffset)
      endCharOffset = hiEnd
      if (hiEnd < buf.length && Character.isHighSurrogate(hi)) {
        val (lo, loEnd) = readUnicodeChar(buf, hiEnd)
        if (Character.isLowSurrogate(lo)) {
          ch = Character.toCodePoint(hi, lo)
          endCharOffset = loEnd
        } else ch = hi
      } else ch = hi
    }

  def nextNonWhitespace = {
    while (ch == ' ' || ch == '\t') nextRawChar()
    ch
  }

  /** replace CR;LF by LF */
  private def skipCR() = if (ch == CR && endCharOffset < buf.length && buf(endCharOffset) == '\\') {
    val (c, nextOffset) = readUnicodeChar(buf, endCharOffset)
    if (c == LF) {
      ch = LF
      endCharOffset = nextOffset
    }
  }

  /** Handle line ends */
  private def potentialLineEnd(): Unit = if (checkLineEnd() && !dialect.allowMultilinePrograms)
    readerError("line breaks are not allowed in single-line quasiquotes", at = begCharOffset)

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

  private[tokenizers] def isNumberSeparator(checkOnly: Boolean = false): Boolean =
    if (ch != '_') false
    else if (dialect.allowNumericLiteralUnderscoreSeparators) true
    else if (checkOnly) false
    else syntaxError("numeric separators are not allowed", at = begCharOffset)

  @inline
  private[tokenizers] def isDigit(): Boolean = ch >= '0' && ch <= '9'

}

object CharArrayReader {

  /** Read next char interpreting \\uxxxx escapes; doesn't mutate internal state */
  private def readUnicodeChar(buf: Array[Char], offset: Int): (Char, Int) = {
    val c = buf(offset)
    val firstOffset = offset + 1 // offset after a single character

    def evenSlashPrefix: Boolean = {
      var p = firstOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (firstOffset - p) % 2 == 0
    }

    if (c != '\\' || firstOffset >= buf.length || buf(firstOffset) != 'u' || !evenSlashPrefix)
      return (c, firstOffset)

    var escapedOffset = firstOffset // offset after an escaped character
    do escapedOffset += 1 while (escapedOffset < buf.length && buf(escapedOffset) == 'u')

    // need 4 digits
    if (escapedOffset + 3 >= buf.length) return (c, firstOffset)

    def udigit: Int =
      try digit2int(buf(escapedOffset), 16)
      finally escapedOffset += 1

    val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
    (code.toChar, escapedOffset)
  }

}
