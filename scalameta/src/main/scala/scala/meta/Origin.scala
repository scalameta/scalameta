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
  def tokens: Seq[Token]
}

object Origin {
  @leaf object None extends Origin {
    val input = Input.None
    val dialect = scala.meta.dialects.Scala211
    val start = 0
    val end = -1
    def tokens = Nil
  }

  @leaf class Parsed(input: Input, dialect: Dialect, startTokenPos: Int, endTokenPos: Int) extends Origin {
    locally {
      var maxStartTokenPos = input.tokens.length - 1
      if (startTokenPos > endTokenPos) maxStartTokenPos += 1
      val maxEndTokenPos = input.tokens.length - 1
      require(input.tokens.last.is[Token.EOF])
      require(0 <= startTokenPos && startTokenPos < maxStartTokenPos && debug(startTokenPos, input.tokens.length))
      require(-1 <= endTokenPos && endTokenPos < maxEndTokenPos && debug(endTokenPos, input.tokens.length))
      require(startTokenPos <= endTokenPos + 1 && debug(startTokenPos, endTokenPos))
    }
    private implicit lazy val thisDialect: Dialect = this.dialect
    val start = input.tokens.apply(startTokenPos).start
    val end = if (endTokenPos != -1) input.tokens.apply(endTokenPos).end else -1
    def tokens: Seq[Token] = input.tokens(dialect).slice(startTokenPos, endTokenPos + 1)
  }

  // TODO: also include information about the transformer
  // TODO: tokens should be more complicated than just a forwarder to tree.origin.tokens
  // if we take a tree, then replace one of its subnodes with a synthetic subnode, then we've just lost tokens
  @leaf class Transformed(tree: Tree) extends Origin {
    def input = tree.origin.input
    def dialect = tree.origin.dialect
    def start = tree.origin.start
    def end = tree.origin.end
    def tokens = tree.origin.tokens
  }
}