package scala.meta.tokenizers

import scala.meta.internal.SingletonReference

final class TokenizerOptions private[meta] (
    // options which control how tokens are emitted
    val groupWhitespace: Boolean = false,
    val tokenize: Option[Tokenize] = None
) {
  def getTokenize(implicit tokenize: Tokenize): Tokenize = this.tokenize.getOrElse(tokenize)

  def withTokenize(tokenize: Option[Tokenize]): TokenizerOptions = privateCopy(tokenize = tokenize)
  def withGroupWhitespace(value: Boolean): TokenizerOptions = privateCopy(groupWhitespace = value)

  private def privateCopy(
      tokenize: Option[Tokenize] = this.tokenize,
      groupWhitespace: Boolean = this.groupWhitespace
  ): TokenizerOptions = new TokenizerOptions(tokenize = tokenize, groupWhitespace = groupWhitespace)
}

object TokenizerOptions {
  val default: TokenizerOptions = new TokenizerOptions()
  val global: SingletonReference[TokenizerOptions] = new SingletonReference(default)

  implicit def implicitTokenizerOptions: TokenizerOptions = global.value
}
