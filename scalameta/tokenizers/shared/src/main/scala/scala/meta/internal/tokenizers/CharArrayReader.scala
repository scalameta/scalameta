package scala.meta
package internal
package tokenizers

import scala.meta.internal.tokens.Chars._

private[meta] case class CharArrayReader(
    buf: Array[Char],
    dialect: Dialect,
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

  import CharArrayReader.NextChar

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = setNextRawCharAndCheck(peekRawChar)

  private def checkRawChar(): Unit = checkLineEnd()

  final def nextCharFrom(offset: Int): Unit = {
    endCharOffset = offset
    nextChar()
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
  final def nextRawChar(): Unit = setNextRawChar(peekRawChar)

  @inline
  private[tokenizers] def setNextRawChar(nch: NextChar): Unit = setNextRawChar(endCharOffset, nch)

  private[tokenizers] def setNextRawCharAndCheck(nch: NextChar): Unit = {
    setNextRawChar(nch)
    checkRawChar()
  }

  private[tokenizers] def setNextRawChar(peekEndOffset: Int, nch: NextChar): Unit = {
    ch = nch.ch
    if (begCharOffset < peekEndOffset) begCharOffset = peekEndOffset
    if (peekEndOffset < nch.end) endCharOffset = nch.end
  }

  @inline
  final def peekRawChar: NextChar = peekRawChar(endCharOffset)

  @inline
  final def peekRawChar(offset: Int): NextChar = CharArrayReader.readRawChar(buf, offset)

  private def checkLineEnd(): Unit = if (ch == LF || ch == FF) {
    lastLineStartOffset = lineStartOffset
    lineStartOffset = endCharOffset
  }

  final def wasMultiChar: Boolean = begCharOffset < endCharOffset - 1
  final def wasEscapedMultiChar: Boolean = wasMultiChar && dialect.treatUnicodeEscapesAsOrdinary

  private[tokenizers] def isNumberSeparator(): Boolean = looksLikeNumberSeparator().contains(true)

  private[tokenizers] def looksLikeNumberSeparator(): Option[Boolean] =
    if (ch != '_') None else Some(dialect.allowNumericLiteralUnderscoreSeparators)

  @inline
  private[tokenizers] def isDigit(): Boolean = CharArrayReader.isDigit(ch)

}

object CharArrayReader {

  case class NextChar(ch: Int, end: Int)

  private val noNextChar = NextChar(SU, -1)

  @inline
  private[tokenizers] def isDigit(ch: Int): Boolean = ch >= '0' && ch <= '9'

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

  final def readRawChar(buf: Array[Char], offset: Int): NextChar =
    if (offset >= buf.length) noNextChar
    else {
      val (hi, hiEnd) = readUnicodeChar(buf, offset)
      if (hiEnd < buf.length && Character.isHighSurrogate(hi)) {
        val (lo, loEnd) = readUnicodeChar(buf, hiEnd)
        if (Character.isLowSurrogate(lo)) return NextChar(Character.toCodePoint(hi, lo), loEnd)
      }
      NextChar(hi, hiEnd)
    }

}
