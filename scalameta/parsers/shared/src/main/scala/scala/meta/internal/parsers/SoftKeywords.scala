package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Ident
import scala.meta.internal.classifiers._
import scala.meta.classifiers._
import scala.meta.tokens.Token.LeftParen
import scala.meta.tokens.Token.LeftBracket

class SoftKeywords(dialect: Dialect) {

  private def matches(token: Token, name: String, isEnabled: => Boolean): Boolean = {
    isEnabled && token.toString() == name
  }

  @classifier
  trait KwUsing {
    val name = "using"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowGivenUsing)
    }
  }

  @classifier
  trait KwInline {
    val name = "inline"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowInlineMods)
    }
  }

  @classifier
  trait KwOpaque {
    val name = "opaque"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowOpaqueTypes)
    }
  }

  @classifier
  trait KwOpen {
    val name = "open"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowOpenClass)
    }
  }

  @classifier
  trait KwTransparent {
    val name = "transparent"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowInlineMods)
    }
  }

  @classifier
  trait KwDerives {
    val name = "derives"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowDerives)
    }
  }

  @classifier
  trait KwEnd {
    val name = "end"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowSignificantIndentation)
    }
  }

  @classifier
  trait KwInfix {
    val name = "infix"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowInfixMods)
    }
  }

  @classifier
  trait KwExtension {
    val name = "extension"
    def unapply(token: Token): Boolean = {
      matches(token, name, dialect.allowExtensionMethods)
    }
  }
}
