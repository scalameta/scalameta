package scala.meta
package internal
package tokenizers

import Chars._
import scala.meta.inputs._

trait CharArrayReaderData {
  /** the last read character */
  var ch: Char = _

  /** The offset one past the last read character */
  var charOffset: Int = 0

  /** The start offset of the current line */
  var lineStartOffset: Int = 0

  /** The start offset of the line before the current one */
  var lastLineStartOffset: Int = 0

  protected var lastUnicodeOffset = -1
}

class CharArrayReader(input: Input, dialect: Dialect, reporter: Reporter) extends CharArrayReaderData { self =>
  val buf = input.chars
  import reporter._

  /** Is last character a unicode escape \\uxxxx? */
  var isUnicodeEscape = false

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    // If the last character is a unicode escape, skip charOffset to the end of
    // the last character. In case `potentialUnicode` restores charOffset
    // to the head of last character.
    if(isUnicodeEscape) charOffset = lastUnicodeOffset
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

  /** Advance one character, leaving CR;LF pairs intact.
   *  This is for use in multi-line strings, so there are no
   *  "potential line ends" here.
   */
  final def nextRawChar() {
    if(isUnicodeEscape) charOffset = lastUnicodeOffset
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
      }
      else {
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
      do charOffset += 1
      while (charOffset < buf.length && buf(charOffset) == 'u')
      val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
      lastUnicodeOffset = charOffset
      isUnicodeEscape = true
      ch = code.toChar
    }

    // restore the charOffset to the saved position
    if (end < buf.length) charOffset = end
  }

  /** replace CR;LF by LF */
  private def skipCR() =
    if (ch == CR && charOffset < buf.length)
      buf(charOffset) match {
        case '\\' =>
          if (lookaheadReader.getu == LF)
            potentialUnicode()
        case _ =>
      }

  /** Handle line ends */
  private def potentialLineEnd() {
    if (ch == LF || ch == FF) {
      if (!dialect.allowMultilinePrograms) {
        readerError("line breaks are not allowed in single-line quasiquotes", at = charOffset - 1)
      }
      lastLineStartOffset = lineStartOffset
      lineStartOffset = charOffset
    }
  }

  /** A new reader that takes off at the current character position */
  def lookaheadReader = new CharArrayLookaheadReader

  class CharArrayLookaheadReader extends CharArrayReader(input, dialect, reporter) {
    charOffset = self.charOffset
    ch = self.ch
    /** A mystery why CharArrayReader.nextChar() returns Unit */
    def getc() = { nextChar() ; ch }
    def getu() = { require(buf(charOffset) == '\\') ; ch = '\\' ; charOffset += 1 ; potentialUnicode() ; ch }
  }
}
