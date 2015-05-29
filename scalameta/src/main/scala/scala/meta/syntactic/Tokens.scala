package scala.meta
package syntactic

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.{Builder, ArrayBuilder, ListBuffer}
import scala.collection.immutable.VectorBuilder

import scala.reflect.macros.blackbox.Context

// TODO: We should really give up on trying to use the standard IndexedSeq machinery,
// because it doesn't give us a good way to load the elements lazily, which is necessary for Tokens.Slice
// and would obviate the need for the very existence of Tokens.Prototype.
// TODO: https://www.dropbox.com/s/5xmjr755tnlqcwk/2015-05-04%2013.50.48.jpg?dl=0
sealed abstract class Tokens(repr: Token*) extends Tokens.Projection(repr: _*) with Input {
  def input: Input
  def dialect: Dialect
  def isAuthentic: Boolean
  def isSynthetic: Boolean = !isAuthentic

  // NOTE: This Input is really special in the sense that
  // doing `input.tokens.head.input` won't return `input`.
  // Previously, I tried to do Token.adjust on every token in the payload,
  // so that they point back to the newly created Input.Tokens.
  // Unfortunately, this runs into performance problems - we really can't afford
  // to clone an entire token stream every time when a tree undergoes a slight change.
  // Therefore, I'm letting this inconsistency alone, and we'll see how it pans out.
  def tokens(implicit dialect: Dialect) = this

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

  // NOTE: Actually doesn't contain any tokens and simply signalizes the internal infrastructure
  // that it's necessary to call inferTokens on the current tree + the prototype (the one that's stored in this object).
  // Once I get to refactoring Tokens to not inherit from Seq and friends, it'll become possible
  // to merge this class into Tokens.Synthetic.
  private[meta] case class Prototype(underlying: Tree) extends Tokens(Nil: _*) {
    override def input = this
    override def dialect = scala.meta.dialects.Scala211
    override def isAuthentic = false
    override def toString = s"Prototype($underlying)"
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
}

trait TokensLiftables extends TokenLiftables {
  val c: Context

  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  import c.universe._

  implicit def liftTokens: Liftable[Tokens] = Liftable[Tokens] { tokens =>
    def prepend(tokens: Tokens, t: Tree): Tree =
      (tokens foldRight t) { case (token, acc) => q"$token +: $acc" }

    def append(t: Tree, tokens: Tokens): Tree =
      // We call insert tokens again because there may be things that need to be spliced in it
      q"$t ++ ${insertTokens(tokens)}"

    def insertTokens(tokens: Tokens): Tree = {
      val (pre, middle) = tokens span (!_.isInstanceOf[Token.Unquote])
      middle match {
        case Tokens() =>
          prepend(pre, q"_root_.scala.meta.syntactic.Tokens()")
        case Token.Unquote(_, _, _, _, tree: Tree) +: rest =>
          // If we are splicing only a single token we need to wrap it in a Vector
          // to be able to append and prepend other tokens to it easily.
          val quoted = if (tree.tpe <:< typeOf[Token]) q"_root_.scala.meta.syntactic.Tokens($tree)" else tree
          append(prepend(pre, quoted), Tokens(rest: _*))
      }
    }

    insertTokens(tokens)
  }
}
