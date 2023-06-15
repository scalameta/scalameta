package scala.meta.internal.parsers

import scala.meta.tokens.Token

// NOTE: public methods of TokenIterator return scannerTokens-based positions
trait TokenIterator {
  def next(): Unit
  def fork: TokenIterator

  def prevToken: Token
  def prevTokenPos: Int
  def previousIndentation: Int

  def token: Token
  def tokenPos: Int
  def currentIndentation: Int

  def observeIndented(): Boolean
  def undoIndent(): Unit

  def peekToken: Token
  def peekIndex: Int
  def peekIndentation: Int
}
