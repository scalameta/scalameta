package scala.meta
package syntactic

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.{Builder, ArrayBuilder, ListBuffer}
import scala.collection.immutable.VectorBuilder

// TODO: https://www.dropbox.com/s/5xmjr755tnlqcwk/2015-05-04%2013.50.48.jpg?dl=0
final class Tokens private (repr: Token*) extends Tokens.Projected(repr: _*) {
  // TODO: having to override all these methods just to change the return type feels kind of stupid
  // why weren't they implemented on top of CanBuildFrom as well?
  override def filter(pred: Token => Boolean): Tokens = Tokens(super.filter(pred): _*)
  override def filterNot(pred: Token => Boolean): Tokens = Tokens(super.filterNot(pred): _*)
  override def partition(pred: Token => Boolean): (Tokens, Tokens) = super.partition(pred) match { case (left, right) => (Tokens(left: _*), Tokens(right: _*)) }
  override def groupBy[K](f: Token => K): immutable.Map[K, Tokens] = super.groupBy(f).mapValues(v => Tokens(v: _*))
  override def take(n: Int): Tokens = Tokens(super.take(n): _*)
  override def drop(n: Int): Tokens = Tokens(super.drop(n): _*)
  override def slice(unc_from: Int, unc_until: Int): Tokens = Tokens(super.slice(unc_from, unc_until): _*)
  override def splitAt(n: Int): (Tokens, Tokens) = super.splitAt(n) match { case (left, right) => (Tokens(left: _*), Tokens(right: _*)) }
  override def takeWhile(pred: Token => Boolean): Tokens = Tokens(super.takeWhile(pred): _*)
  override def span(pred: Token => Boolean): (Tokens, Tokens) = super.span(pred) match { case (left, right) => (Tokens(left: _*), Tokens(right: _*)) }
  override def dropWhile(pred: Token => Boolean): Tokens = Tokens(super.dropWhile(pred): _*)
  // TODO: combinatorial explosion!
  // def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
  // def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) = {
  // TODO: have I missed anything else?
  override def toString: String = s"Tokens(${repr.mkString(", ")})"
}

object Tokens {
  def apply(tokens: Token*) = {
    if ((tokens: Seq[Token]).isInstanceOf[Tokens]) tokens.asInstanceOf[Tokens]
    else new Tokens(tokens: _*)
  }

  class Projected[A](repr: A*)
  extends AbstractSeq[A]
     with IndexedSeq[A]
     with GenericTraversableTemplate[A, Projected]
     with IndexedSeqLike[A, Projected[A]] {
    override def companion = Projected
    override def apply(idx: Int): A = repr(idx)
    override def length: Int = repr.length
    override def toString: String = s"Seq(${repr.mkString(", ")})"
  }

  object Projected extends IndexedSeqFactory[Projected] {
    def newBuilder[A] = new VectorBuilder[A].mapResult(xs => new Projected(xs: _*))
    implicit def defaultCanBuildFrom[A]: CanBuildFrom[Coll, A, Projected[A]] = new GenericCanBuildFrom[A]

    def newTokensBuilder = new VectorBuilder[Token].mapResult(xs => new Tokens(xs: _*))
    implicit def tokensCanBuildFrom: CanBuildFrom[Coll, Token, Tokens] = new CanBuildFrom[Coll, Token, Tokens] {
      def apply(from: Coll): Builder[Token, Tokens] = newTokensBuilder
      def apply(): Builder[Token, Tokens] = newTokensBuilder
    }
  }
}
