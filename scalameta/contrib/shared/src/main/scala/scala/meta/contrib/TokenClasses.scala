package scala.meta
package contrib

import scala.meta.internal.classifiers.classifier
import scala.meta.tokens.Token._
import scala.meta.tokens.{Token => T}

// These token classes are classes extracted from the parser,
// the parser contains more classes but many of those are parser specific.

@classifier
trait Trivia
object Trivia {
  def unapply(token: Token): Boolean = token.isAny[T.Trivia, BOF, EOF]
}

@classifier
trait Keyword
object Keyword {
  def unapply(token: Token): Boolean = token.is[T.Keyword]
}

@classifier
trait Delim
object Delim {
  def unapply(token: Token): Boolean = token.is[T.Symbolic]
}

@classifier
trait Modifier
object Modifier {
  def unapply(token: Token): Boolean = token.isAny[T.ModifierKeyword, KwSuper]
}

@classifier
trait Literal
object Literal {
  def unapply(token: Token): Boolean = token.is[T.Literal]
}

@classifier
trait Whitespace
object Whitespace {
  def unapply(token: Token): Boolean = token.is[T.Whitespace]
}
