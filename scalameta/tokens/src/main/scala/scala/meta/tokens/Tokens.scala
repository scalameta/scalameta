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

// TODO: https://www.dropbox.com/s/5xmjr755tnlqcwk/2015-05-04%2013.50.48.jpg?dl=0
@data class Tokens(private val tokens: Token*) extends Seq[Token] with InternalTokens {
  def iterator: Iterator[Token] = tokens.iterator
  def apply(idx: Int): Token = tokens(idx)
  def length: Int = tokens.length
  def syntax(implicit dialect: Dialect, options: Options = Options.Eager): String = Tokens.showSyntax[Tokens](dialect, options).apply(this).toString
  def structure: String = this.show[Structure]
  override def toString = scala.meta.internal.prettyprinters.TokensToString(this)
}

object Tokens {
  implicit val tokensToInput: Convert[Tokens, Input] = Convert(tokens => Input.String(tokens.syntax))
  implicit val seqTokenToInput: Convert[Seq[Token], Input] = Convert(tokens => Input.String(Tokens(tokens: _*).syntax))
  implicit def showStructure[T <: Tokens]: Structure[T] = TokensStructure.apply[T]
  implicit def showSyntax[T <: Tokens](implicit dialect: Dialect, options: Options): Syntax[T] = TokensSyntax.apply[T](dialect, options)
}
