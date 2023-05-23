package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Ident
import scala.meta.internal.classifiers._
import scala.meta.classifiers._
import scala.meta.tokens.Token.LeftParen
import scala.meta.tokens.Token.LeftBracket

class SoftKeywords(dialect: Dialect) {

  @classifier
  trait KwAs {
    val name = "as"
    @inline def isEnabled = dialect.allowAsForImportRename
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwUsing {
    val name = "using"
    @inline def isEnabled = dialect.allowGivenUsing
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwInline {
    val name = "inline"
    @inline def isEnabled = dialect.allowInlineMods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwOpaque {
    val name = "opaque"
    @inline def isEnabled = dialect.allowOpaqueTypes
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwOpen {
    val name = "open"
    @inline def isEnabled = dialect.allowOpenClass
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwTransparent {
    val name = "transparent"
    @inline def isEnabled = dialect.allowInlineMods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwDerives {
    val name = "derives"
    @inline def isEnabled = dialect.allowDerives
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwEnd {
    val name = "end"
    @inline def isEnabled = dialect.allowEndMarker
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwInfix {
    val name = "infix"
    @inline def isEnabled = dialect.allowInfixMods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  @classifier
  trait KwExtension {
    val name = "extension"
    @inline def isEnabled = dialect.allowExtensionMethods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }
}
