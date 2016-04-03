package scala.meta
package tokenizers

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.internal.tokenizers._

trait Tokenize {
  // TODO: see whether it's possible to make the return type less specific
  def apply(content: Content)(implicit dialect: Dialect): Tokens.Tokenized
}

object Tokenize {
  implicit def scalametaTokenize: Tokenize = ScalametaTokenizer.toTokenize
}