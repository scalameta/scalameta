package scala.meta
package tokens

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.data._
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.prettyprinters.Syntax.Options
import scala.meta.internal.prettyprinters._
import scala.meta.internal.tokens._

// TODO: better API for the Tokens collection (#385)
// TODO: better internal representation for tokens (#150)
@data class Tokens(private val tokens: Token*) extends Seq[Token] with InternalTokens {
  def iterator: Iterator[Token] = tokens.iterator
  def apply(idx: Int): Token = tokens(idx)
  def length: Int = tokens.length
  override def toString = scala.meta.internal.prettyprinters.TokensToString(this)
}

object Tokens {
  implicit val tokensToInput: Convert[Tokens, Input] = Convert(tokens => Input.String(tokens.syntax))
  implicit val seqTokenToInput: Convert[Seq[Token], Input] = Convert(tokens => Input.String(Tokens(tokens: _*).syntax))
  implicit def showStructure[T <: Tokens]: Structure[T] = TokensStructure.apply[T]
  implicit def showSyntax[T <: Tokens](implicit dialect: Dialect, options: Options): Syntax[T] = TokensSyntax.apply[T](dialect, options)
}
