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

  override def indenting: Boolean = curr.token.is[Token.EOL] && {
    // empty sepregions means we are at toplevel
    val lastIndentation = curr.regions.headOption.fold(0)(_.indent)
    lastIndentation >= 0 && lastIndentation < peekToken.pos.startColumn
  }

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

  override def peekToken: Token =
    getNextTokenRef().token

  override def peekIndex: Int =
    getNextTokenRef().pointPos

}
