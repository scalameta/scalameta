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

  @inline
  private def getNextTokenRef(): TokenRef = nextToken(curr)

  override def hasCurr: Boolean = currToken ne null

  override def next(): Unit = {
    prev = curr
    // also adds to prev in case we are forked
    curr = getNextTokenRef()
  }

  private def resetCurr(ref: TokenRef): Unit = {
    prev.next = ref
    curr = ref
  }

  override def indenting: Boolean = curr.regions match {
    case (r: RegionLine) :: rs if curr.token.is[Token.AtEOL] =>
      // empty sepregions means we are at toplevel
      rs.headOption.forall(x => x.indent >= 0 && x.indent < r.indent)
    case _ => false
  }

  def previousIndentation: Int = curr.regions match {
    case (r: RegionLine) :: _ if !curr.token.is[Token.AtEOL] => r.indent
    case _ :: r :: _ => r.indent
    case _ => 0
  }

  override def prevIndex: Int = prev.pointPos
  override def prevToken: Token = prev.token

  override def currIndex: Int = curr.pointPos
  override def currToken: Token = curr.token

  override def fork: TokenIterator = new LazyTokenIterator(scannerTokens, prev, curr)

  override def peekToken: Token = getNextTokenRef().token

  override def peekIndex: Int = getNextTokenRef().pointPos

}
