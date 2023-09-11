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
    var charOffset: Int = 0,
    /** The start offset of the current line */
    var lineStartOffset: Int = 0,
    /** The start offset of the line before the current one */
    private var lastLineStartOffset: Int = 0,
    private var lastUnicodeOffset: Int = -1,
    /** Is last character a unicode escape \\uxxxx? */
    var isUnicodeEscape: Boolean = false
) {

  def this(input: Input, dialect: Dialect, reporter: Reporter) =
    this(buf = input.chars, dialect = dialect, reporter = reporter)

  import reporter._

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    // If the last character is a unicode escape, skip charOffset to the end of
    // the last character. In case `potentialUnicode` restores charOffset
    // to the head of last character.
    if (isUnicodeEscape) charOffset = lastUnicodeOffset
    isUnicodeEscape = false
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
      if (c == '\\') potentialUnicode()
      if (ch < ' ') {
        skipCR()
        potentialLineEnd()
      }
      if (ch == '"' && !dialect.allowMultilinePrograms) {
        readerError("double quotes are not allowed in single-line quasiquotes", at = charOffset - 1)
      }
    }
  }

  final def nextCommentChar(): Unit = {
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      ch = buf(charOffset)
      charOffset += 1
      checkLineEnd()
    }
  }

  /**
   * Advance one character, leaving CR;LF pairs intact. This is for use in multi-line strings, so
   * there are no "potential line ends" here.
   */
  final def nextRawChar(): Unit = {
    if (isUnicodeEscape) charOffset = lastUnicodeOffset
    isUnicodeEscape = false
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
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
      var p = charOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (charOffset - p) % 2 == 0
    }
    def udigit: Int = {
      if (charOffset >= buf.length) {
        // Since the positioning code is very insistent about throwing exceptions,
        // we have to decrement the position so our error message can be seen, since
        // we are one past EOF.  This happens with e.g. val x = \ u 1 <EOF>
        readerError("incomplete unicode escape", at = charOffset - 1)
        SU
      } else {
        val d = digit2int(buf(charOffset), 16)
        if (d >= 0) charOffset += 1
        else readerError("error in unicode escape", at = charOffset)
        d
      }
    }

    // save the end of the current token (exclusive) in case this method
    // advances the offset more than once. See UnicodeEscapeSuite for a
    // and https://github.com/scalacenter/scalafix/issues/593 for
    // an example why this this is necessary.
    val end = charOffset
    if (charOffset < buf.length && buf(charOffset) == 'u' && evenSlashPrefix) {
      do charOffset += 1 while (charOffset < buf.length && buf(charOffset) == 'u')
      try {
        val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
        lastUnicodeOffset = charOffset
        isUnicodeEscape = true
        ch = code.toChar
      } catch {
        case NonFatal(_) =>
      }
    }

    // restore the charOffset to the saved position
    if (end < buf.length) charOffset = end
  }

  /** replace CR;LF by LF */
  private def skipCR() =
    if (ch == CR && charOffset < buf.length && buf(charOffset) == '\\') {
      val lookahead = lookaheadReader
      lookahead.charOffset += 1 // skip the backslash
      lookahead.potentialUnicode()
      if (lookahead.ch == LF) {
        ch = LF
        isUnicodeEscape = true
        lastUnicodeOffset = lookahead.lastUnicodeOffset
      }
    }

  /** Handle line ends */
  private def potentialLineEnd(): Unit = {
    if (checkLineEnd() && !dialect.allowMultilinePrograms) {
      readerError("line breaks are not allowed in single-line quasiquotes", at = charOffset - 1)
    }
  }

  private def checkLineEnd(): Boolean = {
    val ok = ch == LF || ch == FF
    if (ok) {
      lastLineStartOffset = lineStartOffset
      lineStartOffset = charOffset
    }
    ok
  }

  /** A new reader that takes off at the current character position */
  def lookaheadReader = copy()

  /** A mystery why CharArrayReader.nextChar() returns Unit */
  def getc() = { nextChar(); ch }

}
