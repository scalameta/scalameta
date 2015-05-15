package scala.meta
package syntactic

import java.nio.charset.Charset
import org.scalameta.adt._
import org.scalameta.convert._
import org.scalameta.invariants._
import scala.collection._
import scala.collection.generic._
import scala.collection.{immutable, mutable}
import scala.collection.mutable.{Builder, ArrayBuilder, ListBuffer}
import scala.collection.immutable.VectorBuilder
import scala.meta.internal.tokenizers._

sealed trait Input {
  def tokens(implicit dialect: Dialect): Tokens
}

trait Content extends Input {
  def chars: Array[Char]
  private val tokenCache = mutable.Map[Dialect, Tokens]()
  final override def tokens(implicit dialect: Dialect): Tokens = tokenCache.getOrElseUpdate(dialect, tokenize(this))
}

object Input {
  case object None extends Input {
    def tokens(implicit dialect: Dialect) = Tokens()
  }
  final case class String(s: scala.Predef.String) extends Content {
    lazy val chars = s.toArray
  }
  final case class File(f: java.io.File, charset: Charset) extends Content {
    lazy val chars = scala.io.Source.fromFile(f)(scala.io.Codec(charset)).mkString.toArray
  }
  object File {
    def apply(path: Predef.String): Input.File = Input.File(new java.io.File(path))
    def apply(f: java.io.File): Input.File = Input.File(f, Charset.forName("UTF-8"))
  }
  implicit val charsToInput: Convert[Array[Char], Input] = Convert.apply(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert.apply(Input.String(_))
  implicit val fileToInput: Convert[java.io.File, Input] = Convert.apply(f => Input.File(f, Charset.forName("UTF-8")))
}

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

  override def toString: String = s"Tokens(${repr.mkString(", ")})"
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
  }

  private[meta] case class Adhoc(underlying: Token*) extends Tokens(underlying: _*) {
    override def input = this
    override def dialect = scala.meta.dialects.Scala211
    override def isAuthentic = true
  }

  private[meta] case class Synthetic(underlying: Token*) extends Tokens(underlying: _*) {
    override def input = this
    override def dialect = scala.meta.dialects.Scala211
    override def isAuthentic = false
  }

  private[meta] case class Slice(tokens: Tokens, from: Int, until: Int) extends Tokens(tokens.view(from, until): _*) {
    override def input = tokens.input
    override def dialect = tokens.dialect
    override def isAuthentic = true
    override def take(n: Int): Tokens = new Slice(tokens, from, Math.min(from + n, until))
    override def drop(n: Int): Tokens = new Slice(tokens, Math.min(from + n, until), until)
  }
}
