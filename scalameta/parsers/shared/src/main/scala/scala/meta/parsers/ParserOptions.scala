package scala.meta.parsers

import scala.meta.internal.SingletonReference

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
  val global: SingletonReference[ParserOptions] = new SingletonReference(default)

  implicit def implicitParseOptions: ParserOptions = global.value
}
