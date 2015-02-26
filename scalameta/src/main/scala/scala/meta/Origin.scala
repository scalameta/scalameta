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
  def tokens: Seq[Token] = input.tokens(dialect).slice(start, end + 1)
}

object Origin {
  @leaf object None extends Origin {
    val input = Input.None
    val dialect = scala.meta.dialects.Scala211
    val start = 0
    val end = -1
  }

  @leaf class Parsed(input: Input, dialect: Dialect, startTokenPos: Int, endTokenPos: Int) extends Origin {
    private implicit val thisDialect: Dialect = this.dialect
    require(input.tokens.last.is[Token.EOF])
    require(0 <= startTokenPos && startTokenPos < input.tokens.length - 1 && debug(startTokenPos, input.tokens.length))
    require(0 <= endTokenPos && endTokenPos < input.tokens.length - 1 && debug(endTokenPos, input.tokens.length))
    require(startTokenPos <= endTokenPos + 1 && debug(startTokenPos, endTokenPos))
    val start = input.tokens.apply(startTokenPos).start
    val end = input.tokens.apply(endTokenPos).end
  }

  // TODO: also include information about the transformer
  @leaf class Transformed(tree: Tree) extends Origin {
    def input = tree.origin.input
    def dialect = tree.origin.dialect
    def start = tree.origin.start
    def end = tree.origin.end
  }
}