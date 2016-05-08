package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokenizers._

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: distinguish flavors of errors with exception types
trait Reporter {
  // NOTE: not making this public, e.g. by exposing Position.Offset
  // because I don't want to advertise this style of positioning
  private implicit class XtensionOffsetPosition(offset: Offset) {
    private val point = Point.Offset(input, offset)
    def pos = Position.Range(input, point, point)
  }

  def input: Input
  def deprecationWarning(msg: String, at: Position): Unit      = ()
  def deprecationWarning(msg: String, at: Token): Unit         = deprecationWarning(msg, at.pos)
  def deprecationWarning(msg: String, at: Offset): Unit        = deprecationWarning(msg, at.pos)
  def readerError(msg: String, at: Position): Nothing          = throw new TokenizeException(at, msg)
  def readerError(msg: String, at: Token): Nothing             = readerError(msg, at.pos)
  def readerError(msg: String, at: Offset): Nothing            = readerError(msg, at.pos)
  def syntaxError(msg: String, at: Position): Nothing          = throw new TokenizeException(at, msg)
  def syntaxError(msg: String, at: Token): Nothing             = syntaxError(msg, at.pos)
  def syntaxError(msg: String, at: Offset): Nothing            = syntaxError(msg, at.pos)
  def incompleteInputError(msg: String, at: Position): Nothing = throw new TokenizeException(at, msg)
  def incompleteInputError(msg: String, at: Token): Nothing    = incompleteInputError(msg, at.pos)
  def incompleteInputError(msg: String, at: Offset): Nothing   = incompleteInputError(msg, at.pos)
}

object Reporter {
  def apply(content0: Input) = new Reporter { def input = content0 }
}
