package scala.meta.internal.parsers

import scala.meta.classifiers._
import scala.meta.internal.classifiers.classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

@classifier
trait CloseDelim {
  def unapply(token: Token): Boolean = {
    token.is[RightBrace] || token.is[RightBracket] || token.is[RightParen] ||
    token.is[Indentation.Outdent]
  }
}

@classifier
trait Literal {
  def unapply(token: Token): Boolean = token match {
    case Constant.Int(_) => true
    case Constant.Long(_) => true
    case Constant.Float(_) => true
    case Constant.Double(_) => true
    case Constant.Char(_) => true
    case Constant.Symbol(_) => true
    case Constant.String(_) => true
    case KwTrue() => true
    case KwFalse() => true
    case KwNull() => true
    case _ => false
  }
}

@classifier
trait NumericLiteral {
  def unapply(token: Token): Boolean = token match {
    case Constant.Int(_) => true
    case Constant.Long(_) => true
    case Constant.Float(_) => true
    case Constant.Double(_) => true
    case _ => false
  }
}

@classifier
trait Whitespace {
  def unapply(token: Token): Boolean = {
    token.is[Space] || token.is[Tab] ||
    token.is[LineEnd] || token.is[FF]
  }
}

@classifier
trait LineEnd {
  def unapply(token: Token): Boolean = {
    token.is[LF] || token.is[LFLF] || token.is[CR]
  }
}

@classifier
trait Trivia {
  def unapply(token: Token): Boolean = {
    token.is[Whitespace] || token.is[Comment]
  }
}

@classifier
trait MultilineComment {
  def unapply(token: Token): Boolean =
    token.is[Comment] && AsMultilineComment.isMultiline(token)
}

object AsMultilineComment {
  def isMultiline(token: Token): Boolean = {
    val pos = token.pos
    pos.endLine > pos.startLine
  }

  def unapply(token: Token): Option[Comment] =
    token match {
      case c: Comment if isMultiline(token) => Some(c)
      case _ => None
    }
}
