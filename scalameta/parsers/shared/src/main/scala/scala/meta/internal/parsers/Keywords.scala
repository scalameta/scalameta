package scala.meta.internal.parsers

import scala.meta.tokens.Token

import scala.reflect.classTag

object Keywords {

  private val identClass = classTag[Token.Ident].runtimeClass

  @inline
  final def unapply(token: Token.Ident): Option[String] = Some(token.text)

  abstract class AsWithFunc[A](isEnabled: Boolean, func: String => Option[A]) {
    private def getEnabled(value: => String): Option[A] = func(value)
    private def getDisabled(value: => String): Option[A] = None
    private val get: (=> String) => Option[A] = if (isEnabled) getEnabled else getDisabled
    @inline
    final def unapply(value: String): Option[A] = get(value)
    @inline
    final def unapply(token: Token.Ident): Option[A] = get(token.text)
    @inline
    final def unapply(token: Token): Option[A] =
      if (identClass.isInstance(token)) get(token.text) else None
    @inline
    final def apply(token: Token.Ident): Option[A] = unapply(token)
    @inline
    final def apply(token: Token): Option[A] = unapply(token)
  }

  abstract class IsWithPred(isEnabled: Boolean, pred: String => Boolean)
      extends Function[Token, Boolean] {
    private def checkEnabled(value: => String): Boolean = pred(value)
    private val check: (=> String) => Boolean = if (isEnabled) checkEnabled else _ => false
    @inline
    final def unapply(value: String): Boolean = check(value)
    @inline
    final def unapply(token: Token.Ident): Boolean = matches(token)
    @inline
    final def unapply(token: Token): Boolean = matches(token)
    @inline
    final def apply(token: Token.Ident): Boolean = matches(token)
    @inline
    final def apply(token: Token): Boolean = matches(token)
    @inline
    final def matches(token: Token.Ident): Boolean = check(token.text)
    @inline
    final def matches(token: Token): Boolean = identClass.isInstance(token) && check(token.text)
  }

  abstract class IsWithName(isEnabled: Boolean, val name: String)
      extends IsWithPred(isEnabled, name == _)

  abstract class NotWithName(val name: String) extends IsWithPred(true, name != _)

  object Star extends IsWithName(true, "*")
  object PatAlt extends IsWithName(true, "|")
  object NotPatAlt extends NotWithName("|")

}
