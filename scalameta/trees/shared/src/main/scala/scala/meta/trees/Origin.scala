package scala.meta.trees

import org.scalameta.adt

import scala.meta.Dialect
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.tokenizers._
import scala.meta.tokens._

@adt.root
trait Origin extends Optional {
  def position: Position
  def dialectOpt: Option[Dialect]
}

object Origin {
  @adt.none
  object None extends Origin {
    val position: Position = Position.None
    val dialectOpt: Option[Dialect] = scala.None
  }

  // `begTokenIdx` and `endTokenIdx` are half-open interval of index range
  @adt.leaf
  class Parsed(source: ParsedSource, begTokenIdx: Int, endTokenIdx: Int) extends Origin {
    @inline private def tokenize() = source.tokens

    def position: Position = {
      val tokens = tokenize()
      val start = tokens(begTokenIdx).start
      val end = tokens(endTokenIdx - 1).end
      Position.Range(input, start, end)
    }

    def dialectOpt: Option[Dialect] = Some(dialect)

    def tokens: Tokens = {
      tokenize().slice(begTokenIdx, endTokenIdx)
    }

    @inline def input: Input = source.input
    @inline def dialect: Dialect = source.dialect
  }

  class ParsedSource(val input: Input)(implicit val dialect: Dialect) {
    lazy val tokenized = implicitly[Tokenize].apply(input, dialect)
    @inline def tokens = tokenized.get
  }

  @adt.leaf
  class DialectOnly(dialect: Dialect) extends Origin {
    val position: Position = Position.None
    def dialectOpt: Option[Dialect] = Some(dialect)
  }

}
