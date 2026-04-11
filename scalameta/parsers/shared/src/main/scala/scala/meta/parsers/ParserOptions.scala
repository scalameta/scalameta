package scala.meta.parsers

import scala.util.DynamicVariable

final class ParserOptions private[meta] (
    // options which control parsing
    val captureComments: Boolean = true
) {
  def withCaptureComments(value: Boolean): ParserOptions = privateCopy(captureComments = value)

  private def privateCopy(captureComments: Boolean = this.captureComments): ParserOptions =
    new ParserOptions(captureComments = captureComments)
}

object ParserOptions {
  val default: ParserOptions = new ParserOptions()
  val global: DynamicVariable[ParserOptions] = new DynamicVariable(default)

  implicit def implicitParseOptions: ParserOptions = global.value
}
