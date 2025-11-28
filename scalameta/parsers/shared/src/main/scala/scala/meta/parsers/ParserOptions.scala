package scala.meta.parsers

import scala.util.DynamicVariable

final class ParserOptions(
    // options which control parsing
    val captureComments: Boolean = true
)

object ParserOptions {
  val default: ParserOptions = new ParserOptions()
  val global: DynamicVariable[ParserOptions] = new DynamicVariable(default)

  implicit def implicitParseOptions: ParserOptions = global.value
}
