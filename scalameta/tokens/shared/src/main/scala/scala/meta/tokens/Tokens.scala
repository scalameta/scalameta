package scala.meta
package tokens

import scala.collection.immutable
import org.scalameta.data._
import org.scalameta.internal.ScalaCompat.IndexedSeqOptimized
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

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
@data class Tokens private (
    private val tokens: Array[Token],
    private val start: Int,
    private val end: Int
) extends immutable.IndexedSeq[Token] with IndexedSeqOptimized[Token] {
  def apply(idx: Int): Token = tokens(start + idx)
  def length: Int = end - start
  override def slice(from: Int, until: Int): Tokens = {
    val lo = start + from.max(0).min(length)
    val hi = lo.max(start + until.min(length))
    Tokens(tokens, lo, hi)
  }

  /* Both head and headOption need to be implemented here due to
   * binary incompatibility caused by
   * https://github.com/scala/scala/commit/b20dd00b11f06c14c823d277cdfb58043a2586fc
   */
  override def head: Token = apply(0)

  override def headOption: Option[Token] = if (isEmpty) None else Some(head)

  override def toString = scala.meta.internal.prettyprinters.TokensToString(this)

  override def segmentLength(p: Token => Boolean, from: Int = 0): Int = super.segmentLength(p, from)

  def segmentLengthRight(p: Token => Boolean, from: Int = 0): Int =
    reverseIterator.drop(from).takeWhile(p).length

  override def take(n: Int): Tokens = slice(0, n)

  override def takeRight(n: Int): Tokens = slice(length - n, length)

  override def drop(n: Int): Tokens = slice(n, length)

  override def dropRight(n: Int): Tokens = slice(0, length - n)

  override def takeWhile(p: Token => Boolean): Tokens = take(segmentLength(p))

  def takeRightWhile(p: Token => Boolean): Tokens = takeRight(segmentLengthRight(p))

  override def dropWhile(p: Token => Boolean): Tokens = drop(segmentLength(p, 0))

  def dropRightWhile(p: Token => Boolean): Tokens = dropRight(segmentLengthRight(p))

  override def splitAt(n: Int): (Tokens, Tokens) = (take(n), drop(n))

  override def span(p: Token => Boolean): (Tokens, Tokens) = {
    val index = indexWhere(!p.apply(_))

    splitAt(if (index < 0) length else index)
  }

  def spanRight(p: Token => Boolean): (Tokens, Tokens) = {
    val index = reverseIterator.indexWhere(!p.apply(_))

    splitAt(length - (if (index < 0) length else index))
  }
}

object Tokens {
  private[meta] def apply(tokens: Array[Token], start: Int, end: Int): Tokens =
    new Tokens(tokens, start, end)
  def unapplySeq(tokens: Tokens): Option[Seq[Token]] = Some(tokens)

  implicit val tokensToInput: Convert[Tokens, Input] =
    Convert(tokens => Input.String(tokens.syntax))
  implicit val listTokenToInput: Convert[List[Token], Input] =
    Convert(tokens => Input.String(Tokens(tokens.toArray, 0, tokens.length).syntax))
  implicit def showStructure[T <: Tokens]: Structure[T] = TokensStructure.apply[T]
  implicit def showSyntax[T <: Tokens](implicit dialect: Dialect): Syntax[T] =
    TokensSyntax.apply[T](dialect)
}
