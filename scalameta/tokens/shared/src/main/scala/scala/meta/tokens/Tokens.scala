package scala.meta
package tokens

import org.scalameta.data._
import org.scalameta.internal.ScalaCompat.IndexedSeqOptimized
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.internal.prettyprinters._
import scala.meta.prettyprinters._

import scala.collection.immutable

// WONTFIX: https://github.com/scalameta/scalameta/issues/385
// WONTFIX: https://github.com/scalameta/scalameta/issues/150
//
// NOTE: `Tokens` is no longer a general-purpose wrapper over arbitrary lists of tokens.
// Now, it has a representation specialized to ensure that:
//   1) Random access is O(1)
//   2) Slice is O(1)
//   3) Everything else is no slower than O(N)
//   4) Operations on slices satisfy #1-3
// We pay very close attention to #1 and #2, because these operations are at the core of
// Tree.tokens and follow-up inspections of the resulting Tokens collection.
// As a result, I've changed both the constructor of Tokens and the factory method of Tokens to private[meta].
// We can afford that because we no longer have APIs that take adhoc token streams.
//
// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore `end` can point to the last token plus one.
@data
class Tokens private (private val tokens: Array[Token], private val start: Int, val length: Int)
    extends immutable.IndexedSeq[Token] with IndexedSeqOptimized[Token] {
  @inline
  private def get(idx: Int): Token = tokens(start + idx)
  def apply(idx: Int): Token =
    if (idx >= 0 && idx < length) get(idx)
    else throw new NoSuchElementException(s"token $idx out of $length")
  def end: Int = start + length // for MIMA compat

  /** get element in full Tokens relative to start of this slice */
  def getWideOpt(idx: Int): Option[Token] = {
    val wideIdx = start + idx
    if (wideIdx >= 0 && wideIdx < tokens.length) Some(tokens(wideIdx)) else None
  }

  override def slice(from: Int, until: Int): Tokens = {
    val lo = start + from.max(0).min(length)
    val len = 0.max(start + until.min(length) - lo)
    new Tokens(tokens, lo, len)
  }

  /* Both head and headOption need to be implemented here due to
   * binary incompatibility caused by
   * https://github.com/scala/scala/commit/b20dd00b11f06c14c823d277cdfb58043a2586fc
   */
  override def head: Token = apply(0)
  override def headOption: Option[Token] = if (isEmpty) None else Some(get(0))

  override def last: Token = apply(length - 1)
  override def lastOption: Option[Token] = if (isEmpty) None else Some(get(length - 1))

  override def toString = scala.meta.internal.prettyprinters.TokensToString(this)

  override def segmentLength(p: Token => Boolean, from: Int = 0): Int = skipIf(p, from) - from

  def segmentLengthRight(p: Token => Boolean, from: Int = 0): Int = {
    val beg = length - from - 1
    beg - rskipIf(p, beg)
  }

  @inline
  def skipIf(p: Token => Boolean, rangeBeg: Int): Int = skipIf(p, rangeBeg, length)
  @inline
  def rskipIf(p: Token => Boolean, rangeBeg: Int): Int = rskipIf(p, rangeBeg, -1)

  /**
   * Skip tokens satisfying a given predicate.
   * @param rangeBeg
   *   first index to check
   * @param rangeEnd
   *   first index not to check
   */
  def skipIf(p: Token => Boolean, rangeBeg: Int, rangeEnd: Int): Int =
    if (rangeBeg < 0) rangeBeg else skipIfFull(p, start + rangeBeg, start + rangeEnd.min(length))

  def skipWideIf(p: Token => Boolean, rangeBeg: Int, rangeEnd: Int): Int = {
    val beg = start + rangeBeg
    if (beg < 0) rangeBeg else skipIfFull(p, beg, tokens.length.min(start + rangeEnd))
  }

  private def skipIfFull(p: Token => Boolean, rangeBeg: Int, rangeEnd: Int): Int = {
    var i = rangeBeg
    while (i < rangeEnd && p(tokens(i))) i += 1
    i - start
  }

  /**
   * Skip tokens satisfying a given predicate, iterating in reverse.
   * @param rangeBeg
   *   first index to check
   * @param rangeEnd
   *   first index not to check
   */
  def rskipIf(p: Token => Boolean, rangeBeg: Int, rangeEnd: Int): Int =
    if (rangeBeg >= length || rangeBeg <= rangeEnd) rangeBeg
    else rskipIfFull(p, start + rangeBeg, start + rangeEnd.max(-1))

  def rskipWideIf(p: Token => Boolean, rangeBeg: Int, rangeEnd: Int): Int = {
    val beg = start + rangeBeg
    if (beg >= tokens.length) rangeBeg else rskipIfFull(p, beg, (start + rangeEnd).max(-1))
  }

  private def rskipIfFull(p: Token => Boolean, rangeBeg: Int, rangeEnd: Int): Int = {
    var i = rangeBeg
    while (i > rangeEnd && p(tokens(i))) i -= 1
    i - start
  }

  override def take(n: Int): Tokens = slice(0, n)

  override def takeRight(n: Int): Tokens = slice(length - n, length)

  override def drop(n: Int): Tokens = slice(n, length)

  override def dropRight(n: Int): Tokens = slice(0, length - n)

  override def takeWhile(p: Token => Boolean): Tokens = take(segmentLength(p))

  def takeRightWhile(p: Token => Boolean): Tokens = takeRight(segmentLengthRight(p))

  override def dropWhile(p: Token => Boolean): Tokens = drop(segmentLength(p))

  def dropRightWhile(p: Token => Boolean): Tokens = dropRight(segmentLengthRight(p))

  override def splitAt(n: Int): (Tokens, Tokens) = (take(n), drop(n))

  override def span(p: Token => Boolean): (Tokens, Tokens) = splitAt(segmentLength(p))

  def spanRight(p: Token => Boolean): (Tokens, Tokens) = splitAt(length - segmentLengthRight(p))

  def findNot(p: Token => Boolean): Option[Token] = {
    val idx = segmentLength(p)
    if (idx < length) Some(get(idx)) else None
  }
}

object Tokens {
  private[meta] def apply(tokens: Array[Token]): Tokens = apply(tokens, 0, tokens.length)
  private[meta] def apply(tokens: Array[Token], start: Int, end: Int): Tokens =
    new Tokens(tokens, start, end - start)
  def unapplySeq(tokens: Tokens): Option[Seq[Token]] = Some(tokens)

  private def convertTokensToInput(tokens: Tokens): Input = Input.String(tokens.syntax)
  implicit val tokensToInput: Convert[Tokens, Input] = Convert(convertTokensToInput)
  implicit val listTokenToInput: Convert[List[Token], Input] =
    Convert(tokens => convertTokensToInput(Tokens(tokens.toArray)))
  implicit def showStructure[T <: Tokens]: Structure[T] = TokensStructure.apply[T]
  implicit def showSyntax[T <: Tokens](implicit dialect: Dialect): Syntax[T] = TokensSyntax
    .apply[T](dialect)
}
