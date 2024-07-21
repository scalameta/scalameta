package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.internal.tokens.Chars._

import scala.annotation.tailrec

private[meta] case class CharArrayReader private (
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

  import CharArrayReader._

  def this(input: Input, dialect: Dialect) = this(buf = input.chars, dialect = dialect)

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    nextRawChar()
    checkRawChar()
  }

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
  final def nextRawChar(): Unit = setNextRawChar(peekRawChar())

  @inline
  private[tokenizers] def setNextRawChar(nextChar: NextChar): Unit =
    setNextRawChar(endCharOffset, nextChar)

  private[tokenizers] def setNextRawChar(peekEndOffset: Int, nextChar: NextChar): Unit = {
    ch = nextChar.ch
    if (begCharOffset < peekEndOffset) begCharOffset = peekEndOffset
    if (peekEndOffset < nextChar.end) endCharOffset = nextChar.end
  }

  final def nextCharIf(f: Int => Boolean): Boolean = {
    val nextChar = peekRawChar()
    val ok = f(nextChar.ch)
    if (ok) {
      setNextRawChar(nextChar)
      checkRawChar()
    }
    ok
  }

  @inline
  final def peekRawChar(): NextChar = peekRawChar(endCharOffset)

  @inline
  final def peekRawChar(offset: Int): NextChar = CharArrayReader.readRawChar(buf, offset)

  @inline
  final def peekNonWhitespace(): NextChar = findNonWhitespace(buf, ch, endCharOffset)

  private def checkLineEnd(): Unit = if (ch == LF || ch == FF) {
    lastLineStartOffset = lineStartOffset
    lineStartOffset = endCharOffset
  }

  final def wasMultiChar: Boolean = begCharOffset < endCharOffset - 1

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

  def findNonWhitespace(buf: Array[Char], ch: Int, offset: Int): NextChar =
    findNonWhitespace(buf, NextChar(ch, offset))

  @tailrec
  def findNonWhitespace(buf: Array[Char], nextChar: NextChar): NextChar = nextChar.ch match {
    case ' ' | '\t' => findNonWhitespace(buf, readRawChar(buf, nextChar.end))
    case _ => nextChar
  }

}
