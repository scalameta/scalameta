package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Ident
import scala.meta.internal.classifiers.classifier

class SoftKeywords(dialect: Dialect) {

  def isIdentAnd(token: Token, pred: String => Boolean): Boolean = token match {
    case Ident(value) if pred(value.stripPrefix("`").stripSuffix("`")) => true
    case _ => false
  }

  def isSoftKw(token: Token, name: String): Boolean = isIdentAnd(token, _ == name)

  @classifier
  trait SkAs {
    val name = "as"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowAsPatternBinding
    }
  }
  @classifier
  trait SkUsing {
    val name = "using"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowGivenUsing
    }
  }

  @classifier
  trait SkInline {
    val name = "inline"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowInlineMods
    }
  }

  @classifier
  trait SkOpaque {
    val name = "opaque"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowOpaqueTypes
    }
  }

  @classifier
  trait SkOpen {
    val name = "open"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowOpenClass
    }
  }

  @classifier
  trait SkTransparent {
    val name = "transparent"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowInlineMods
    }
  }

  @classifier
  trait SkDerives {
    val name = "derives"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowDerives
    }
  }

  @classifier
  trait SkEnd {
    val name = "end"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowSignificantIndentation
    }
  }

  @classifier
  trait SkInfix {
    val name = "infix"
    def unapply(token: Token): Boolean = {
      isSoftKw(token, name) && dialect.allowInfixMods
    }
  }
}
