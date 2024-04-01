package scala.meta.internal.tokens

import scala.annotation.switch
import scala.language.postfixOps

/** Contains constants and classifier methods for characters */
object Chars {
  // Be very careful touching these.
  // Apparently trivial changes to the way you write these constants
  // will cause Scanners.scala to go from a nice efficient switch to
  // a ghastly nested if statement which will bring the type checker
  // to its knees. See ticket #1456
  // Martin: (this should be verified now that the pattern rules have been redesigned).
  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  /**
   * Convert a character digit to an Int according to given base, -1 if no success
   */
  def digit2int(ch: Int, base: Int): Int = {
    val num =
      if (ch <= '9') ch - '0'
      else if ('a' <= ch && ch <= 'z') ch - 'a' + 10
      else if ('A' <= ch && ch <= 'Z') ch - 'A' + 10
      else -1
    if (0 <= num && num < base) num else -1
  }

  /** Can character start an alphanumeric Scala identifier? */
  @inline
  def isIdentifierStart(c: Int): Boolean = (c == '_') || isIdentifierPart(c)

  /** Can character form part of an alphanumeric Scala identifier? */
  def isIdentifierPart(c: Int) = (c == '$') || Character.isUnicodeIdentifierPart(c)

  @inline
  def isUnicodeIdentifierPart(c: Int) =
    // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
    (c != SU) && Character.isUnicodeIdentifierPart(c)

  /** Is character a math or other symbol in Unicode? */
  def isSpecial(c: Int) = {
    val chtp = Character.getType(c)
    chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
  }

  /** Can character form part of a Scala operator name? */
  def isOperatorPart(c: Int): Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' | '?' | ':' | '=' | '&' |
        '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }
  @inline
  def isOperatorPart(c: Char): Boolean = isOperatorPart(c.toInt)

  /**
   * {{{
   *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender
   * }}}
   * See [4] and Appendix B of XML 1.0 specification.
   */
  def isNameChar(ch: Char) = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    isNameStart(ch) ||
    (getType(ch).toByte match {
      case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK | MODIFIER_LETTER |
          DECIMAL_DIGIT_NUMBER => true
      case _ => ".-:".contains(ch)
    })
  }

  /**
   * {{{
   *  NameStart ::= ( Letter | '_' )
   * }}}
   * where Letter means in one of the Unicode general categories {{{Ll, Lu, Lo, Lt, Nl}}}.
   *
   * We do not allow a name to start with ':'. See [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart(ch: Int) = {
    import java.lang.Character._

    getType(ch).toByte match {
      case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER =>
        true
      case _ => ch == '_'
    }
  }

  private val codepage =
    Map('\t' -> "\\t", '\b' -> "\\b", '\n' -> "\\n", '\r' -> "\\r", '\f' -> "\\f", '\\' -> "\\\\")

  def escape(value: Char): String = codepage.getOrElse(value, value.toString)

  def escape(value: String): String = {
    val buf = new StringBuilder(value.length)
    value.foreach(c => codepage.get(c).fold(buf.append(c))(buf.append))
    buf.result()
  }

}
