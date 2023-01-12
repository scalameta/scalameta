package scala.meta.internal.parsers

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens

object LazyTokenIterator {

  def apply(st: ScannerTokens)(implicit dialect: Dialect): LazyTokenIterator =
    new LazyTokenIterator(st, TokenRef(Nil, null, -1), TokenRef(Nil, st.tokens(0), 0))

}

private[parsers] class LazyTokenIterator private (
    private val scannerTokens: ScannerTokens,
    private var prev: TokenRef,
    private var curr: TokenRef
)(implicit dialect: Dialect)
    extends TokenIterator {

  import scannerTokens._

  private def getNextTokenRef(): TokenRef =
    nextToken(curr.token, curr.pos, curr.nextPos, curr.regions)

  override def next(): Unit = {
    prev = curr
    curr = getNextTokenRef()
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
        curr = TokenRef(regions, indent, curr.pos, curr.pos, pointPos)
        true
      } else false
    }
  }

  /**
   * Deals with different rules for indentation after self type arrow.
   */
  def undoIndent(): Unit = {
    curr.regions match {
      case (region: SepRegionIndented) :: others if curr.token.is[Indentation.Indent] =>
        next()
        curr = curr.withRegions(curr.regions match {
          // deal with  region added by `case` in enum after self type
          case RegionArrow :: _ => others
          // if no region was added
          case `region` :: _ => others
          // keep any added region in `next()`
          case head :: _ => head :: others
          case xs => xs
        })
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
          case Some(_: RegionParen) if token.is[LeftParen] => prev.tail
          case Some(RegionEnumArtificialMark) if token.is[KwEnum] => prev.tail
          case Some(_: RegionBrace) if token.is[LeftBrace] => prev.tail
          //  Handle fewer braces and partial function.
          case Some(RegionArrow) if dialect.allowFewerBraces && token.is[KwCase] => prev.tail
          case _ => prev
        }
      RegionIndent(i, false) :: undoRegionChange
    }
  }

  def observeIndentedEnum(): Boolean = {
    observeIndented0((i, prev) => {
      val nextPrev = prev match {
        case RegionArrow :: RegionEnumArtificialMark :: other => other
        case RegionEnumArtificialMark :: other => other
        case x => x
      }
      RegionIndentEnum(i) :: nextPrev
    })
  }

  def currentIndentation: Int = {
    val foundIndentation = countIndent(curr.pointPos)
    if (foundIndentation < 0)
      // empty sepregions means we are at toplevel
      curr.regions.headOption.fold(0)(_.indent)
    else
      foundIndentation
  }

  def previousIndentation: Int = curr.regions match {
    case _ :: r :: _ => r.indent
    case _ => 0
  }

  def observeOutdented(): Boolean =
    dialect.allowSignificantIndentation && (curr.regions match {
      case (region: SepRegionIndented) :: tail if (curr.token match {
            case _: KwThen | _: KwElse | _: KwDo | _: KwYield => true
            case _: KwMatch | _: KwCatch | _: KwFinally => true
            case _ => false
          }) =>
        val outdentPos = findOutdentPos(prevTokenPos, curr.pos, region)
        val outdent = mkOutdentToken(outdentPos)
        curr = TokenRef(tail, outdent, curr.pos, curr.pos, outdentPos)
        true
      case _ => false
    })

  override def prevTokenPos: Int = prev.pointPos
  override def prevToken: Token = prev.token

  override def tokenPos: Int = curr.pointPos
  override def token: Token = curr.token

  override def fork: TokenIterator =
    new LazyTokenIterator(scannerTokens, prev, curr)

}
