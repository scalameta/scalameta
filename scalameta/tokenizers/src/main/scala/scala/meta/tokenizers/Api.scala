package scala.meta
package tokenizers

import scala.meta.internal.tokenizers.ScalametaTokenizer
import scala.meta.tokenizers.common._
import scala.meta.internal.tokenizers._

private[meta] trait Api extends common.Api {
  implicit def scalametaTokenize: Tokenize = ScalametaTokenizer.toTokenize
}

private[meta] trait Aliases extends common.Aliases {
}
