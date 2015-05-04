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
  @leaf object None extends Origin {
    val input = Input.None
    val dialect = scala.meta.dialects.Scala211
    val start = 0
    val end = -1
    val startLine = 0
    val endLine = -1
    def tokens = Tokens()
  }

  @leaf class Parsed(input: Input, dialect: Dialect, startTokenPos: Int, endTokenPos: Int) extends Origin {
    private implicit lazy val thisDialect: Dialect = this.dialect
    val start = input.tokens.apply(startTokenPos).start
    val end = if (endTokenPos != -1) input.tokens.apply(endTokenPos).end else -1
    private def findLine(x: Int, default: Int) = input match {
      case Input.None => default
      case els => els.content.take(x + 1).count(_ == '\n')
    }
    lazy val startLine = findLine(start, 0)
    lazy val endLine = findLine(end, -1)
    def tokens: Tokens = input.tokens(dialect).slice(startTokenPos, endTokenPos + 1)
  }

  // TODO: also include information about the transformer
  // TODO: tokens should be more complicated than just a forwarder to tree.origin.tokens
  // if we take a tree, then replace one of its subnodes with a synthetic subnode, then we've just lost tokens
  @leaf class Transformed(tree: Tree) extends Origin {
    def input = tree.origin.input
    def dialect = tree.origin.dialect
    def start = tree.origin.start
    def end = tree.origin.end
    def startLine = tree.origin.startLine
    def endLine = tree.origin.endLine
    def tokens = tree.origin.tokens
  }
}