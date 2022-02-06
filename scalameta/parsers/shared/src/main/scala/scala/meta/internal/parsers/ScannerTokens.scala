package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.inputs.Input
import scala.meta.tokenizers._
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

class ScannerTokens(tokens: Tokens, input: Input)(implicit dialect: Dialect) {
  @inline def apply(idx: Int): Token = tokens(idx)
  @inline def length: Int = tokens.length
  @inline def head: Token = tokens.head
}

object ScannerTokens {

  def apply(input: Input)(implicit dialect: Dialect): ScannerTokens = {
    val tokens = input.tokenize.get
    new ScannerTokens(tokens, input)
  }

}
