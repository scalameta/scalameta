package scala.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Origin {
  def input: Input
  def dialect: Dialect
  def start: Int
  def end: Int
  def startLine: Int
  def endLine: Int
  def tokens: Tokens
}

object Origin {
  @leaf class Parsed(input: Input, dialect: Dialect, startTokenPos: Int, endTokenPos: Int) extends Origin {
    lazy val tokens = input.tokens(dialect).slice(startTokenPos, endTokenPos + 1)
    lazy val start = input.tokens(dialect).apply(startTokenPos).start
    lazy val end = if (endTokenPos != -1) input.tokens(dialect).apply(endTokenPos).end else -1
    private def findLine(x: Int, default: Int) = input match {
      case Input.None => default
      case els => els.content.take(x + 1).count(_ == '\n')
    }
    lazy val startLine = findLine(start, 0)
    lazy val endLine = findLine(end, -1)
  }

  @leaf class Synthetic(tree: Tree) extends Origin {
    lazy val input = Input.Tokens(tokens)
    lazy val dialect = scala.meta.internal.ui.inferDialect(tree)
    lazy val tokens = scala.meta.internal.ui.inferTokens(tree)
    def start = 0
    def end = -1
    def startLine = 0
    def endLine = -1
  }
}