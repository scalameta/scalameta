package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.tokens.Token

class SoftKeywords(dialect: Dialect) {

  object KwAs {
    val name = "as"
    @inline def isEnabled = dialect.allowAsForImportRename
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwUsing {
    val name = "using"
    @inline def isEnabled = dialect.allowGivenUsing
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwInline {
    val name = "inline"
    @inline def isEnabled = dialect.allowInlineMods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwOpaque {
    val name = "opaque"
    @inline def isEnabled = dialect.allowOpaqueTypes
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwOpen {
    val name = "open"
    @inline def isEnabled = dialect.allowOpenClass
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwTransparent {
    val name = "transparent"
    @inline def isEnabled = dialect.allowInlineMods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwDerives {
    val name = "derives"
    @inline def isEnabled = dialect.allowDerives
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwEnd {
    val name = "end"
    @inline def isEnabled = dialect.allowEndMarker
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwInfix {
    val name = "infix"
    @inline def isEnabled = dialect.allowInfixMods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwExtension {
    val name = "extension"
    @inline def isEnabled = dialect.allowExtensionMethods
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

  object KwErased {
    val name = "erased"
    @inline def isEnabled = dialect.allowErasedDefs
    @inline final def unapply(token: Token): Boolean = isEnabled && name == token.text
    @inline final def apply(token: Token): Boolean = unapply(token)
    @inline final def unapply(token: String): Boolean = isEnabled && name == token
  }

}
