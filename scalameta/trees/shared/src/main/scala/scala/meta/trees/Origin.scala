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
  private[meta] def inputOpt: Option[Input]
  private[meta] def textOpt: Option[String]
  private[meta] def tokensOpt: Option[Tokens]
}

object Origin {
  @adt.none
  object None extends Origin {
    val position: Position = Position.None
    val dialectOpt: Option[Dialect] = scala.None
    private[meta] val inputOpt: Option[Input] = scala.None
    private[meta] val textOpt: Option[String] = scala.None
    private[meta] val tokensOpt: Option[Tokens] = scala.None
  }

  // `begTokenIdx` and `endTokenIdx` are half-open interval of index range
  @adt.leaf
  class Parsed(source: ParsedSource, begTokenIdx: Int, endTokenIdx: Int) extends Origin {
    @inline
    def allInputTokens() = source.tokens

    lazy val position: Position = {
      val tokens = allInputTokens()
      val start = tokens(begTokenIdx).start
      val end = tokens(endTokenIdx - 1).end
      Position.Range(input, start, end)
    }

    def dialectOpt: Option[Dialect] = Some(dialect)
    private[meta] def inputOpt: Option[Input] = Some(input)
    private[meta] def textOpt: Option[String] = Some(text)
    private[meta] def tokensOpt: Option[Tokens] = Some(tokens)

    @inline
    def input: Input = source.input
    @inline
    def dialect: Dialect = source.dialect
    @inline
    def text: String = position.text
    def tokens: Tokens = allInputTokens().slice(begTokenIdx, endTokenIdx)
  }

  class ParsedSource(val input: Input)(implicit val dialect: Dialect) {
    lazy val tokenized = implicitly[Tokenize].apply(input, dialect)
    @inline
    def tokens = tokenized.get
  }

  @adt.leaf
  class DialectOnly(dialect: Dialect) extends Origin {
    val position: Position = Position.None
    def dialectOpt: Option[Dialect] = Some(dialect)
    private[meta] val inputOpt: Option[Input] = scala.None
    private[meta] val textOpt: Option[String] = scala.None
    private[meta] val tokensOpt: Option[Tokens] = scala.None
  }

  object DialectOnly {
    implicit def fromDialect(implicit dialect: Dialect): DialectOnly = new DialectOnly(dialect)

    private[meta] def getFromArgs(args: Any*): DialectOnly = {
      val queue = scala.collection.mutable.Queue.empty[Iterator[Any]]
      @scala.annotation.tailrec
      def loop(iterator: Iterator[Any]): DialectOnly =
        if (!iterator.hasNext) if (queue.isEmpty) implicitly[DialectOnly] else loop(queue.dequeue())
        else iterator.next() match {
          case x: scala.meta.Tree => x.origin.dialectOpt match {
              case Some(dialect) => fromDialect(dialect)
              case _ => loop(iterator)
            }
          case x: Iterable[_] =>
            queue.enqueue(x.iterator)
            loop(iterator)
          case _ => loop(iterator)
        }
      loop(Iterator(args))
    }
  }

  private[meta] def first(one: Origin, two: Origin): Origin = if (one ne None) one else two

}
