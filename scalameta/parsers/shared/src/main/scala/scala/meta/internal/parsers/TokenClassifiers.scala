package scala.meta.internal.parsers

import scala.meta.classifiers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

object MultilineComment {
  def unapply(token: Token): Boolean = token.is[Comment] && AsMultilineComment.isMultiline(token)
  def apply(token: Token): Boolean = unapply(token)
}

object AsMultilineComment {
  def isMultiline(token: Token): Boolean = {
    val pos = token.pos
    pos.endLine > pos.startLine
  }

  def unapply(token: Token): Option[Comment] = token match {
    case c: Comment if isMultiline(token) => Some(c)
    case _ => None
  }
}
