package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.internal.inputs._
import scala.meta.tokenizers._
import scala.meta.tokens._

trait Reporter {
  // NOTE: not making this public, e.g. by exposing Position.Offset
  // because I don't want to advertise this style of positioning
  private implicit class XtensionOffsetPosition(offset: Offset) {
    def pos = input.pos(offset)
  }

  def input: Input
  protected def error(msg: String, at: Position): Nothing = throw new TokenizeException(at, msg)
  def deprecationWarning(msg: String, at: Position): Unit = ()
  def deprecationWarning(msg: String, at: Token): Unit = deprecationWarning(msg, at.pos)
  def deprecationWarning(msg: String, at: Offset): Unit = deprecationWarning(msg, at.pos)
  def readerError(msg: String, at: Position): Nothing = error(msg, at)
  final def readerError(msg: String, at: Token): Nothing = readerError(msg, at.pos)
  final def readerError(msg: String, at: Offset): Nothing = readerError(msg, at.pos)
  def syntaxError(msg: String, at: Position): Nothing = error(msg, at)
  final def syntaxError(msg: String, at: Token): Nothing = syntaxError(msg, at.pos)
  final def syntaxError(msg: String, at: Offset): Nothing = syntaxError(msg, at.pos)
  def incompleteInputError(msg: String, at: Position): Nothing = error(msg, at)
  final def incompleteInputError(msg: String, at: Token): Nothing = incompleteInputError(msg, at.pos)
  final def incompleteInputError(msg: String, at: Offset): Nothing =
    incompleteInputError(msg, at.pos)
}

object Reporter {
  def apply(content0: Input) = new Reporter {
    def input = content0
  }
}
