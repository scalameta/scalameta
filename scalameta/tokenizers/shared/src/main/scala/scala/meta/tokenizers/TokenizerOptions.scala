package scala.meta.tokenizers

import scala.util.DynamicVariable

final class TokenizerOptions(
    // options which control how tokens are emitted
    val groupWhitespace: Boolean = false
)

object TokenizerOptions {
  val default: TokenizerOptions = new TokenizerOptions()
  val global: DynamicVariable[TokenizerOptions] = new DynamicVariable(default)

  implicit def implicitTokenizerOptions: TokenizerOptions = global.value
}
