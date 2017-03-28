package scala.meta
package internal
package parsers

import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.parsers._

// TODO: when I grow up I want to become a monad, just like my daddy
trait Reporter {
  def deprecationWarning(msg: String, at: Position): Unit = ()
  def deprecationWarning(msg: String, at: Token): Unit = deprecationWarning(msg, at.pos)
  def deprecationWarning(msg: String, at: Tree): Unit = deprecationWarning(msg, at.pos)
  def syntaxWarning(msg: String, at: Position): Unit = ()
  def syntaxWarning(msg: String, at: Token): Unit = syntaxWarning(msg, at.pos)
  def syntaxWarning(msg: String, at: Tree): Unit = syntaxWarning(msg, at.pos)
  def syntaxError(msg: String, at: Position): Nothing = throw new ParseException(at, msg)
  def syntaxError(msg: String, at: Token): Nothing = syntaxError(msg, at.pos)
  def syntaxError(msg: String, at: Tree): Nothing = syntaxError(msg, at.pos)
}

object Reporter {
  def apply() = new Reporter {}
}
