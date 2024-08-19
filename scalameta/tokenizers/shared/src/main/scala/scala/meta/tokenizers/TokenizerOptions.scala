package scala.meta.tokenizers

import scala.util.DynamicVariable

final class TokenizerOptions(
    // options which control how tokens are emitted
    val groupWhitespace: Boolean = false
)

object TokenizerOptions {
  val default: TokenizerOptions = new TokenizerOptions()
  val global: DynamicVariable[Option[TokenizerOptions]] = new DynamicVariable(None)

  implicit def implicitTokenizerOptions: Option[TokenizerOptions] = global.value
}
