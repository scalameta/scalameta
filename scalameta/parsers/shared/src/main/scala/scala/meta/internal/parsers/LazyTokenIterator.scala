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

  private def observeIndented0(f: (Int, List[SepRegion]) => List[SepRegion]): Boolean = {
    if (!dialect.allowSignificantIndentation) false
    else {
      val currRegions = curr.regions
      val existingIndent = currRegions.find(_.isIndented).fold(0)(_.indent)
      val (expected, pointPos) = countIndentAndNewlineIndex(tokenPos)
      if (expected > existingIndent) {
        val regions = f(expected, currRegions)
        val indent = mkIndentToken(pointPos)
        resetCurr(TokenRef(regions, indent, curr.pos, curr.pos, pointPos))
        true
      } else false
    }
  }

  /**
   * Deals with different rules for indentation after self type arrow.
   */
  def undoIndent(): Unit = {
    curr.regions match {
      case (_: SepRegionIndented) :: others if curr.token.is[Indentation.Indent] =>
        val ref = nextToken(curr.token, curr.pos, curr.nextPos, others)
        resetCurr(ref)
      case _ =>
    }
  }

  def observeIndented(): Boolean = {
    observeIndented0 { (i, prev) =>
      /* When adding RegionIndent (we wrap the current code block in indentation)
       * we might no longer need the region added on the current token.
       *
       * In case the region was needed, we will add it again as we haven't yet progressed
       * to the next token.
       */
      val undoRegionChange =
        prev.headOption match {
          case Some(RegionParen) if token.is[LeftParen] => prev.tail
          case Some(RegionEnumArtificialMark) if token.is[KwEnum] => prev.tail
          case Some(_: RegionBrace) if token.is[LeftBrace] => prev.tail
          //  Handle fewer braces and partial function.
          case Some(RegionCaseExpr) if dialect.allowFewerBraces && token.is[KwCase] => prev.tail
          case _ => prev
        }
      RegionIndent(i, false) :: undoRegionChange
    }
  }

  def observeIndentedEnum(): Boolean = {
    observeIndented0((i, prev) => {
      val nextPrev = prev match {
        case RegionCaseExpr :: RegionEnumArtificialMark :: other => other
        case RegionEnumArtificialMark :: other => other
        case x => x
      }
      RegionIndentEnum(i) :: nextPrev
    })
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

  def observeOutdented(): Boolean =
    dialect.allowSignificantIndentation && (curr.regions match {
      case (region: SepRegionIndented) :: tail if (curr.token match {
            case _: KwMatch | _: KwCatch | _: KwFinally => true
            case _ => false
          }) =>
        val outdentPos = findOutdentPos(prevTokenPos, curr.pos, region)
        val outdent = mkOutdentToken(outdentPos)
        resetCurr(TokenRef(tail, outdent, curr.pos, curr.pos, outdentPos, curr.withRegions(tail)))
        true
      case _ => false
    })

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
