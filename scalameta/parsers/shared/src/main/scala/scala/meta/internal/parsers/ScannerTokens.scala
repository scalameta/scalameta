package scala.meta.internal.parsers

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.inputs.Input
import scala.meta.internal.tokenizers._
import scala.meta.tokenizers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens

import org.scalameta.debug

class ScannerTokens(tokens: Tokens, input: Input)(implicit dialect: Dialect) {

  // NOTE: This is a cache that's necessary for reasonable performance of prev/next for tokens.
  // It maps character positions in input's content into indices in the scannerTokens vector.
  // One complication here is that there can be multiple tokens that sort of occupy a given position,
  // so the clients of this cache need to be wary of that!
  private val cache: Array[Int] = {
    val result = new Array[Int](input.chars.length)
    var i = 0
    while (i < tokens.length) {
      val token = tokens(i)
      var j = token.start
      while (j < token.end) {
        result(j) = i
        j += 1
      }
      i += 1
    }
    result
  }

  @inline def apply(idx: Int): Token = tokens(idx)
  @inline def length: Int = tokens.length
  @inline def head: Token = tokens.head

  import Implicits._

  @tailrec
  private final def getPrev(token: Token): Token = {
    val prev = token.prevSafe
    if (prev.is[Trivia]) getPrev(prev) else prev
  }

  @tailrec
  private final def getNext(token: Token): Token = {
    val next = token.nextSafe
    if (next.is[Trivia]) getNext(next) else next
  }

  @tailrec
  private final def getStrictAfterSafe(token: Token): Token = {
    if (token.is[Space] || token.is[Tab] || token.is[Comment])
      getStrictAfterSafe(token.nextSafe)
    else token
  }

  object Implicits {
    // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
    // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
    // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
    // This leads to extremely dirty and seriously crazy code.
    implicit class XtensionTokenClass(token: Token) {

      def isClassOrObject = token.is[KwClass] || token.is[KwObject]
      def isClassOrObjectOrEnum = isClassOrObject || (token.is[Ident] && dialect.allowEnums)

      def index: Int = {
        @tailrec
        def lurk(roughIndex: Int): Int = {
          require(roughIndex >= 0 && debug(token))
          val scannerToken = tokens(roughIndex)

          def exactMatch = scannerToken eq token

          def originMatch =
            (token.is[LFLF] || token.is[Indentation.Outdent] || token.is[Indentation.Indent]) &&
              scannerToken.start == token.start && scannerToken.end == token.end

          if (exactMatch || originMatch) roughIndex
          else lurk(roughIndex - 1)
        }

        if (token.start == input.chars.length) tokens.length - 1
        else lurk(cache(token.start))
      }

      @inline
      def prev: Token = getPrev(token)

      @inline
      def prevSafe: Token = tokens(Math.max(index - 1, 0))

      @inline
      def next: Token = getNext(token)

      @inline
      def nextSafe: Token = tokens(Math.min(index + 1, tokens.length - 1))

      @inline
      def strictNext: Token = getStrictAfterSafe(nextSafe)

      def isLeadingInfixOperator: Boolean = dialect.allowSignificantIndentation && {
        val parts = token.text.split("_")
        parts.nonEmpty &&
        parts.last.forall(Chars.isOperatorPart) &&
        !token.text.startsWith("@") && {
          val nextSafeToken = nextSafe
          nextSafeToken.is[Whitespace] && {
            val nextToken = getStrictAfterSafe(nextSafeToken)
            nextToken.is[Ident] || nextToken.is[Interpolation.Id] ||
            nextToken.is[Literal] || nextToken.is[LeftParen] ||
            nextToken.is[LeftBrace]
          }
        }
      }

    }
  }

}

object ScannerTokens {

  def apply(input: Input)(implicit dialect: Dialect): ScannerTokens = {
    val tokens = input.tokenize.get
    new ScannerTokens(tokens, input)
  }

}
