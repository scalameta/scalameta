package scala.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Origin {
  def input: Input
  def dialect: Dialect
  def position: Position
  def start: Point = position.start
  def end: Point = position.end
  def tokens: Tokens
}

object Origin {
  @leaf class Parsed(input: Input, dialect: Dialect, startTokenPos: Int, endTokenPos: Int) extends Origin {
    lazy val position = input match {
      case input: Input.Real =>
        val startToken = input.tokens(dialect).apply(startTokenPos)
        val endToken = input.tokens(dialect).apply(endTokenPos)
        Position.Real(input, startToken, endToken)
      case input: Input.Virtual =>
        Position.Virtual(input)
    }
    lazy val tokens = {
      input.tokens(dialect).slice(startTokenPos, endTokenPos + 1)
    }
  }

  @leaf class Synthetic(tree: Tree) extends Origin {
    lazy val input = Input.Virtual(tokens)
    lazy val dialect = scala.meta.internal.ui.inferDialect(tree)
    lazy val position = Position.Virtual(input)
    lazy val tokens = scala.meta.internal.ui.inferTokens(tree)
  }
}