package scala.meta
package syntactic

import scala.meta.internal.tokenizers.ScalametaTokenizer

private[meta] trait ScalametaTokenizeApi {
  implicit def scalametaTokenize: Tokenize = ScalametaTokenizer.toTokenize
}

object tokenizeApi extends GenericTokenizeApi with ScalametaTokenizeApi