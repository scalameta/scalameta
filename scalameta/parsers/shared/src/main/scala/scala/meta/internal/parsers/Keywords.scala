package scala.meta.internal.parsers

import scala.meta.tokens.Token
import scala.reflect.classTag

object Keywords {

  private val identClass = classTag[Token.Ident].runtimeClass

  abstract class IsWithPred(isEnabled: Boolean, pred: String => Boolean) {
    private def checkEnabled(value: => String): Boolean = pred(value)
    private val check: (=> String) => Boolean = if (isEnabled) checkEnabled else _ => false
    @inline final def unapply(value: String): Boolean = check(value)
    @inline final def unapply(token: Token.Ident): Boolean = check(token.text)
    @inline final def unapply(token: Token): Boolean =
      identClass.isInstance(token) && check(token.text)
    @inline final def apply(token: Token.Ident): Boolean = unapply(token)
    @inline final def apply(token: Token): Boolean = unapply(token)
  }

  abstract class IsWithName(isEnabled: Boolean, val name: String)
      extends IsWithPred(isEnabled, name == _)

  abstract class NotWithName(val name: String) extends IsWithPred(true, name != _)

  object PatAlt extends IsWithName(true, "|")
  object NotPatAlt extends NotWithName("|")

}
