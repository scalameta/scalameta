package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

object LazyTokenIterator {

  def apply(st: ScannerTokens)(implicit dialect: Dialect): LazyTokenIterator = {
    val curr = TokenRef(Nil, st.tokens(0), 0, null)
    val prev = TokenRef(Nil, null, -1, curr)
    new LazyTokenIterator(st, prev, curr)
  }

}

private[parsers] class LazyTokenIterator private (
    private val scannerTokens: ScannerTokens,
    private var prev: TokenRef,
    private var curr: TokenRef
)(implicit dialect: Dialect)
    extends TokenIterator {

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

  private def observeIndented0(f: List[SepRegion] => List[SepRegion]): Boolean = {
    if (!dialect.allowSignificantIndentation) false
    else if (curr.token.is[Indentation.Indent]) true
    else {
      val currRegions = curr.regions
      val existingIndent = currRegions.find(_.isIndented).fold(0)(_.indent)
      val (expected, pointPos) = countIndentAndNewlineIndex(tokenPos)
      if (expected > existingIndent) {
        val regions = f(currRegions)
        val indentRegions = RegionIndent(expected) :: regions
        val indent = mkIndentToken(pointPos)
        resetCurr(TokenRef(indentRegions, indent, curr.pos, curr.pos, pointPos))
        true
      } else false
    }
  }

  def observeIndented(): Boolean = {
    observeIndented0 { prev =>
      /* When adding RegionIndent (we wrap the current code block in indentation)
       * we might no longer need the region added on the current token.
       *
       * In case the region was needed, we will add it again as we haven't yet progressed
       * to the next token.
       */
      prev match {
        case RegionParen :: tail if token.is[LeftParen] => tail
        case (_: RegionBrace) :: tail if token.is[LeftBrace] => tail
        case _ => prev
      }
    }
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
