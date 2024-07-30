package scala.meta.internal.parsers

import scala.meta.tokens.Token

// NOTE: public methods of TokenIterator return scannerTokens-based positions
trait TokenIterator {
  def hasCurr: Boolean
  def next(): Unit
  def fork: TokenIterator

  def prevToken: Token
  def prevIndex: Int
  def previousIndentation: Int

  def currToken: Token
  def currIndex: Int
  def indenting: Boolean

  def peekToken: Token
  def peekIndex: Int
}
