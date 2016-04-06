package scala.meta
package tokenizers

import scala.annotation.implicitNotFound
import scala.meta.inputs._
import scala.meta.internal.tokenizers._

trait Tokenize {
  def apply(content: Content)(implicit dialect: Dialect): Tokenized
}

object Tokenize {
  implicit def scalametaTokenize: Tokenize = ScalametaTokenizer.toTokenize
}