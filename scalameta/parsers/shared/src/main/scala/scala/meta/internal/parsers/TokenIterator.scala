package scala.meta.internal.parsers

import scala.meta.tokens.Token

// NOTE: public methods of TokenIterator return scannerTokens-based positions
trait TokenIterator {
  def next(): Unit
  def prevTokenPos: Int
  def tokenPos: Int
  def currentIndentation: Int
  def token: Token
  def fork: TokenIterator
  def observeIndented(): Boolean
  def observeOutdented(): Boolean
  def observeIndentedEnum(): Boolean
  def undoIndent(): Unit
}
