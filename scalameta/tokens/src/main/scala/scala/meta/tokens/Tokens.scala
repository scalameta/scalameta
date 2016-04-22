package scala.meta
package tokens

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.{Builder, ArrayBuilder, ListBuffer}
import scala.collection.immutable.VectorBuilder
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

// TODO: We should really give up on trying to use the standard IndexedSeq machinery,
// because it doesn't give us a good way to load the elements lazily, which is necessary for Tokens.Slice
// and would obviate the need for the very existence of Tokens.Prototype.
// TODO: https://www.dropbox.com/s/5xmjr755tnlqcwk/2015-05-04%2013.50.48.jpg?dl=0
// TODO: not sealed because TransformedTokens is declared in a different project
abstract class Tokens(repr: Token*) extends Tokens.Projection(repr: _*) with Input {
  def input: Input
  def dialect: Dialect
  def isAuthentic: Boolean
  def isSynthetic: Boolean = !isAuthentic

  // TODO: having to override all these methods just to change the return type feels kind of stupid
  // why weren't they implemented on top of CanBuildFrom as well?
  override def filter(pred: Token => Boolean): Tokens = Tokens(super.filter(pred): _*)
  override def filterNot(pred: Token => Boolean): Tokens = Tokens(super.filterNot(pred): _*)
  override def partition(pred: Token => Boolean): (Tokens, Tokens) = super.partition(pred) match { case (left, right) => (Tokens(left: _*), Tokens(right: _*)) }
  override def groupBy[K](f: Token => K): immutable.Map[K, Tokens] = super.groupBy(f).mapValues(v => Tokens(v: _*))
  override def take(n: Int): Tokens = Tokens.Slice(this, 0, n + 1)
  override def drop(n: Int): Tokens = Tokens.Slice(this, n, length)
  override def slice(from: Int, until: Int): Tokens = drop(from).take(until - from)
  override def splitAt(n: Int): (Tokens, Tokens) = (take(n), drop(n))
  override def takeWhile(pred: Token => Boolean): Tokens = Tokens(super.takeWhile(pred): _*)
  override def span(pred: Token => Boolean): (Tokens, Tokens) = super.span(pred) match { case (left, right) => (Tokens(left: _*), Tokens(right: _*)) }
  override def dropWhile(pred: Token => Boolean): Tokens = Tokens(super.dropWhile(pred): _*)
  // TODO: combinatorial explosion!
  // def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
  // def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) = {
  // TODO: have I missed anything else?
}

object Tokens {
  def apply(tokens: Token*) = {
    if ((tokens: Seq[Token]).isInstanceOf[Tokens]) tokens.asInstanceOf[Tokens]
    else Tokens.Adhoc(tokens: _*)
  }
  def unapplySeq(tokens: Tokens): Some[Seq[Token]] = Some(tokens)

  sealed class Projection[A](repr: A*)
  extends AbstractSeq[A]
     with IndexedSeq[A]
     with GenericTraversableTemplate[A, Projection]
     with IndexedSeqLike[A, Projection[A]] {
    override def companion = Projection
    override def apply(idx: Int): A = repr(idx)
    override def length: Int = repr.length
    override def toString: String = s"Seq(${repr.mkString(", ")})"
  }

  object Projection extends IndexedSeqFactory[Projection] {
    def newBuilder[A] = new VectorBuilder[A].mapResult(xs => new Projection(xs: _*))
    implicit def defaultCanBuildFrom[A]: CanBuildFrom[Coll, A, Projection[A]] = new GenericCanBuildFrom[A]

    def newTokensBuilder = new VectorBuilder[Token].mapResult(xs => Tokens.Synthetic(xs: _*))
    implicit def tokensCanBuildFrom: CanBuildFrom[Coll, Token, Tokens] = new CanBuildFrom[Coll, Token, Tokens] {
      def apply(from: Coll): Builder[Token, Tokens] = newTokensBuilder
      def apply(): Builder[Token, Tokens] = newTokensBuilder
    }
  }

  private[meta] case class Tokenized(content: Content, dialect: Dialect, underlying: Token*) extends Tokens(underlying: _*) {
    override def input = content
    override def isAuthentic = true
    override def toString = s"Tokenized($content, $dialect, $underlying)"
  }

  private[meta] case class Adhoc(underlying: Token*) extends Tokens(underlying: _*) {
    override def input = this
    override def dialect = scala.meta.dialects.Scala211
    override def isAuthentic = true
    override def toString = s"Adhoc($underlying)"
  }

  private[meta] case class Synthetic(underlying: Token*) extends Tokens(underlying: _*) {
    override def input = this
    override def dialect = scala.meta.dialects.Scala211
    override def isAuthentic = false
    override def toString = s"Synthetic($underlying)"
  }

  private[meta] case class Slice(tokens: Tokens, from: Int, until: Int) extends Tokens(tokens.view(from, until): _*) {
    override def input = tokens.input
    override def dialect = tokens.dialect
    override def isAuthentic = true
    override def take(n: Int): Tokens = new Slice(tokens, from, Math.min(from + n, until))
    override def drop(n: Int): Tokens = new Slice(tokens, Math.min(from + n, until), until)
    override def toString = s"Slice($tokens, $from, $until)"
  }

  implicit def showStructure[T <: Tokens]: Structure[T] = TokensStructure.apply[T]
  implicit def showSyntax[T <: Tokens]: Syntax[T] = TokensSyntax.apply[T]
}
