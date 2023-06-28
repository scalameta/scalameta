package scala.meta.internal.parsers

import scala.meta.classifiers._
import scala.meta.tokens.Token

object LazyTokenIterator {

  def apply(st: ScannerTokens): LazyTokenIterator = {
    val curr = TokenRef(Nil, st.tokens(0), 0, null)
    val prev = TokenRef(Nil, null, -1, curr)
    new LazyTokenIterator(st, prev, curr)
  }

}

private[parsers] class LazyTokenIterator private (
    private val scannerTokens: ScannerTokens,
    private var prev: TokenRef,
    private var curr: TokenRef
) extends TokenIterator {

  import scannerTokens._

  private def getNextTokenRef(): TokenRef = {
    if (curr.next eq null) curr.next = nextToken(curr)
    curr.next
  }

  override def next(): Unit = {
    prev = curr
    // also adds to prev in case we are forked
    curr = getNextTokenRef()
  }

  private def resetCurr(ref: TokenRef): Unit = {
    prev.next = ref
    curr = ref
  }

  private def getIndentation(ref: TokenRef): Int = {
    val foundIndentation = countIndent(ref.pointPos)
    if (foundIndentation < 0)
      // empty sepregions means we are at toplevel
      ref.regions.headOption.fold(0)(_.indent)
    else
      foundIndentation
  }

  override def currentIndentation: Int = getIndentation(curr)

  def previousIndentation: Int = curr.regions match {
    case _ :: r :: _ => r.indent
    case _ => 0
  }

  override def prevTokenPos: Int = prev.pointPos
  override def prevToken: Token = prev.token

  override def tokenPos: Int = curr.pointPos
  override def token: Token = curr.token

  override def fork: TokenIterator =
    new LazyTokenIterator(scannerTokens, prev, curr)

  override def peekIndentation: Int =
    getIndentation(getNextTokenRef())

  override def peekToken: Token =
    getNextTokenRef().token

  override def peekIndex: Int =
    getNextTokenRef().pointPos

}
