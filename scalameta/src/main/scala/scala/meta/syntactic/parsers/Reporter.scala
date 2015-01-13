package scala.meta
package syntactic
package parsers

import scala.meta.syntactic.tokenizers.Token
import scala.meta.ui.Exception

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: when we have Positions, attach them to the exception being thrown
// TODO: when we have working `Tree.tokens`, use `at.tokens` instead of `currentToken`
trait Reporter {
  def currentToken: Token
  def deprecationWarning(msg: String): Unit           = ()
  def deprecationWarning(msg: String, at: Tree): Unit = ()
  def syntaxError(msg: String): Nothing               = throw Exception(s"syntax error at $currentToken: $msg")
  def syntaxError(msg: String, at: Tree): Nothing     = throw Exception(s"syntax error at $currentToken: $msg")
}

object Reporter {
  def apply(current: () => Token) = new Reporter { def currentToken = current() }
}
