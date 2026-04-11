package scala.meta.tokenizers

import scala.util.DynamicVariable

final class TokenizerOptions private[meta] (
    // options which control how tokens are emitted
    val groupWhitespace: Boolean = false
) {
  def withGroupWhitespace(value: Boolean): TokenizerOptions = privateCopy(groupWhitespace = value)

  private def privateCopy(groupWhitespace: Boolean = this.groupWhitespace): TokenizerOptions =
    new TokenizerOptions(groupWhitespace = groupWhitespace)
}

object TokenizerOptions {
  val default: TokenizerOptions = new TokenizerOptions()
  val global: DynamicVariable[TokenizerOptions] = new DynamicVariable(default)

  implicit def implicitTokenizerOptions: TokenizerOptions = global.value
}
