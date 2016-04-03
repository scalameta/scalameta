package scala.meta
package tokenizers

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.internal.tokenizers._

trait Tokenize {
  def apply(content: Content)(implicit dialect: Dialect): Tokens
}

object Tokenize {
  implicit def scalametaTokenize: Tokenize = ScalametaTokenizer.toTokenize
}