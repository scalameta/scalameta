package scala.meta
package syntactic
package parsers

import scala.meta.syntactic.tokenizers.Token

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: when we have working `Tree.tokens`, start using `at` parameters of methods on Reporter
trait Reporter {
  def currentToken: Token
  def deprecationWarning(msg: String): Unit           = ()
  def deprecationWarning(msg: String, at: Tree): Unit = ()
  def syntaxError(msg: String): Nothing               = throw Reporter.SyntaxError(msg, currentToken)
  def syntaxError(msg: String, at: Tree): Nothing     = throw Reporter.SyntaxError(msg, currentToken)
}

object Reporter {
  def apply(current: () => Token) = new Reporter { def currentToken = current() }
  sealed abstract class Exception(msg: String) extends scala.Exception(msg)
  final case class ReaderError(msg: String, at: Token) extends Exception(s"$msg at $at")
  final case class SyntaxError(msg: String, at: Token) extends Exception(s"syntax error at $at: $msg")
  final case class IncompleteInputError(msg: String, at: Token) extends Exception(s"incomplete input at $at: $msg")
}
