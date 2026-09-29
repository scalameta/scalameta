package scala.meta.trees

import scala.meta.Dialect
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.tokenizers._
import scala.meta.tokens._

sealed trait Origin extends Optional {
  def position: Position
  // Start/end character offsets, available without forcing (allocating) `position`
  // -- consumers that only need offsets (e.g. scalafmt) can avoid the Range alloc.
  // Default via `position` (cheap for the Position.None singleton); ParsedPartial
  // overrides to read token offsets directly.
  def begOffset: Int = position.start
  def endOffset: Int = position.end
  def dialectOpt: Option[Dialect]
  private[meta] def inputOpt: Option[Input]
  private[meta] def textOpt: Option[String]
  private[meta] def tokensOpt: Option[Tokens]

  private[meta] def begTokenIdx: Int
  private[meta] def endTokenIdx: Int
}

object Origin {
  object None extends Origin {
    val position: Position = Position.None
    val dialectOpt: Option[Dialect] = scala.None
    override def isEmpty: Boolean = true
    private[meta] val inputOpt: Option[Input] = scala.None
    private[meta] val textOpt: Option[String] = scala.None
    private[meta] val tokensOpt: Option[Tokens] = scala.None
    private[meta] def begTokenIdx: Int = -1
    private[meta] def endTokenIdx: Int = -1
  }

  // `begTokenIdx` and `endTokenIdx` are half-open interval of index range
  sealed trait Partial extends Origin {
    val begTokenIdx: Int
    val endTokenIdx: Int
    override def isEmpty: Boolean = begTokenIdx >= endTokenIdx
  }

  sealed trait ParsedPartial extends Partial {
    val source: ParsedSource

    @inline
    def allInputTokens() = source.tokens

    override def begOffset: Int = allInputTokens()(begTokenIdx).start
    override def endOffset: Int = allInputTokens()(endTokenIdx - 1).end
    override def isEmpty: Boolean = begOffset >= endOffset

    lazy val position: Position = Position.Range(input, begOffset, endOffset)

    def dialectOpt: Option[Dialect] = Some(dialect)
    private[meta] def inputOpt: Option[Input] = Some(input)
    private[meta] def tokensOpt: Option[Tokens] = Some(tokens)

    @inline
    def input: Input = source.input
    @inline
    def dialect: Dialect = source.dialect
    def tokens: Tokens = allInputTokens().slice(begTokenIdx, endTokenIdx)
  }

  final case class Parsed(source: ParsedSource, begTokenIdx: Int, endTokenIdx: Int)
      extends ParsedPartial {
    private[meta] def textOpt: Option[String] = Some(text)
    @inline
    def text: String = position.text
  }

  final case class ParsedSpliced(source: ParsedSource, begTokenIdx: Int, endTokenIdx: Int)
      extends ParsedPartial {
    private[meta] def textOpt: Option[String] = scala.None
  }

  final class ParsedSource(val input: Input)(implicit val dialect: Dialect) {
    lazy val tokenized = input.tokenizerOptions.getTokenize.apply(input, dialect)
    @inline
    def tokens = tokenized.get
  }

  final class DialectOnly(dialect: Dialect) extends Origin {
    val position: Position = Position.None
    def dialectOpt: Option[Dialect] = Some(dialect)
    private[meta] val inputOpt: Option[Input] = scala.None
    private[meta] val textOpt: Option[String] = scala.None
    private[meta] val tokensOpt: Option[Tokens] = scala.None
    private[meta] def begTokenIdx: Int = -1
    private[meta] def endTokenIdx: Int = -1
  }

  object DialectOnly {
    def apply(dialect: Dialect): DialectOnly = new DialectOnly(dialect)

    implicit def fromDialect(implicit dialect: Dialect): DialectOnly = new DialectOnly(dialect)

    private[meta] def fromOrigin(origin: Origin): Origin = origin.dialectOpt
      .fold[Origin](Origin.None)(x => fromDialect(x))

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

  final class PartialProxy(origin: Partial) extends Origin {
    override val position: Position = Position.None
    override def dialectOpt: Option[Dialect] = origin.dialectOpt
    override private[meta] def inputOpt: Option[Input] = origin.inputOpt
    override private[meta] val textOpt: Option[String] = scala.None
    override private[meta] val tokensOpt: Option[Tokens] = scala.None
    private[meta] def begTokenIdx: Int = -1
    private[meta] def endTokenIdx: Int = -1
  }
  object PartialProxy {
    def apply(origin: Origin): Origin = origin match {
      case origin: Partial => new PartialProxy(origin)
      case origin => origin // includes None, DialectOnly, PartialProxy
    }
  }

  private[meta] def first(one: Origin, two: => Origin): Origin = if (one ne None) one else two

}
