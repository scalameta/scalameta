package scala.meta
package tokens

import scala.collection.immutable
import org.scalameta.data._
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.tokens._

// TODO: better API for the Tokens collection (#385)
// TODO: better internal representation for tokens (#150)
//
// NOTE: `Tokens` is no longer a general-purpose wrapper over arbitrary sequences of tokens.
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
@data class Tokens private (private val tokens: Array[Token], private val start: Int, private val end: Int)
extends immutable.IndexedSeq[Token] with collection.IndexedSeqOptimized[Token, immutable.IndexedSeq[Token]] {
  def apply(idx: Int): Token = tokens(start + idx)
  def length: Int = end - start
  override def slice(from: Int, until: Int): Tokens = Tokens(tokens, start + math.max(from, 0), start + math.min(math.max(until, 0), length))
  override def toString = scala.meta.internal.prettyprinters.TokensToString(this)
}

object Tokens {
  private[meta] def apply(tokens: Array[Token], start: Int, end: Int): Tokens = new Tokens(tokens, start, end)
  def unapplySeq(tokens: Tokens): Option[Seq[Token]] = Some(tokens)

  implicit val tokensToInput: Convert[Tokens, Input] = Convert(tokens => Input.String(tokens.syntax))
  implicit val seqTokenToInput: Convert[Seq[Token], Input] = Convert(tokens => Input.String(Tokens(tokens.toArray, 0, tokens.length).syntax))
  implicit def showStructure[T <: Tokens](implicit options: Options): Structure[T] = TokensStructure.apply[T](options)
  implicit def showSyntax[T <: Tokens](implicit dialect: Dialect, options: Options): Syntax[T] = TokensSyntax.apply[T](dialect, options)
}
