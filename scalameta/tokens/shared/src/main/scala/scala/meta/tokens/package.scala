package scala.meta

import scala.meta.internal.tokens.Chars

import scala.annotation.tailrec

package object tokens extends tokens.Api {

  implicit class TokenExtensions(private val value: Token) extends AnyVal {
    @inline
    def isBackquoted: Boolean = value.text.isBackquoted
    @inline
    def isSymbolicInfixOperator: Boolean = value.isInstanceOf[Token.Ident] &&
      isIdentSymbolicInfixOperator
    @inline
    def isIdentSymbolicInfixOperator: Boolean = value.text.isIdentSymbolicInfixOperator
  }

  implicit class StringExtensions(private val value: String) extends AnyVal {

    private def isBackquotedNonEmpty: Boolean = value.head == '`' && value.last == '`'

    def isBackquoted: Boolean = value.nonEmpty && isBackquotedNonEmpty

    def isPatVar: Boolean = value.nonEmpty && value.head.isLower && !isBackquotedNonEmpty

    def isIdentSymbolicNonBackquotedInfixOperator: Boolean = {
      @tailrec
      def iter(idx: Int, nonEmpty: Boolean): Boolean = {
        val ch = value(idx)
        if (ch == '_') nonEmpty || idx > 0 && iter(idx - 1, false)
        else Chars.isOperatorPart(ch) && (idx == 0 || iter(idx - 1, true))
      }

      val len = value.length
      len == 0 || iter(len - 1, false)
    }

    def isIdentSymbolicInfixOperator: Boolean = isBackquoted ||
      isIdentSymbolicNonBackquotedInfixOperator
  }

}
