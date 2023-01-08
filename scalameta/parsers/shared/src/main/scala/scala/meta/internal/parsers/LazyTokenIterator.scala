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

  @tailrec
  private def nextToken(
      prevToken: Token,
      prevPos: Int,
      currPos: Int,
      sepRegions: List[SepRegion]
  ): TokenRef = {
    val prev = if (prevPos >= 0) tokens(prevPos) else null
    val curr = tokens(currPos)
    val currNonTrivial = !curr.is[Trivia]
    val nextPos = tokens.indexWhere(!_.is[Trivia], currPos + 1)
    val next = if (nextPos >= 0) tokens(nextPos) else null

    def isTrailingComma: Boolean =
      dialect.allowTrailingCommas &&
        curr.is[Comma] &&
        next.is[CloseDelim] &&
        next.pos.startLine > curr.pos.endLine

    def mkIndent(pointPos: Int, regions: List[SepRegion]): TokenRef =
      TokenRef(regions, mkIndentToken(pointPos), prevPos, currPos, pointPos)

    def mkOutdent(region: SepRegion, regions: List[SepRegion]): TokenRef =
      mkOutdentTo(region, currPos, regions)

    def mkOutdentTo(region: SepRegion, maxPointPos: Int, regions: List[SepRegion]): TokenRef = {
      val pointPos = findOutdentPos(prevPos, maxPointPos, region)
      TokenRef(regions, mkOutdentToken(pointPos), prevPos, currPos, pointPos)
    }

    def currRef(regions: List[SepRegion]): TokenRef = TokenRef(regions, curr, currPos)

    def nonTrivial = curr match {
      case _: LeftParen => currRef(RegionParen(false) :: sepRegions)
      case _: LeftBracket => currRef(RegionBracket :: sepRegions)
      case _: Comma =>
        sepRegions match {
          case (head: SepRegionIndented) :: tail
              if tail.find(!_.isIndented).exists(_.isInstanceOf[RegionParen]) =>
            mkOutdent(head, tail)
          case _ => currRef(sepRegions)
        }
      case _: LeftBrace =>
        val indentInBrace = if (isAheadNewLine(currPos)) countIndent(nextPos) else -1
        def indentOnArrow = !prev.isAny[KwMatch, KwCatch]
        // After encountering keyword Enum we add artificial '{' on top of stack.
        // Then always after Enum next token is '{'. On token '{' we check if top of stack is '{'
        // (which in case of enum is always true) and replace it with '$'.
        // Now if we have token 'case' and top of stack is '$' we know it is Enum-case.
        // In any other case it is 'match-case' or 'try-case'
        currRef(sepRegions match {
          case RegionEnumArtificialMark :: tail => RegionEnum(indentInBrace) :: tail
          case xs => RegionBrace(indentInBrace, indentOnArrow) :: xs
        })
      case _: KwEnum => currRef(RegionEnumArtificialMark :: sepRegions)
      case _ if isCaseIntro(currPos) =>
        currRef(sepRegions match {
          case (_: RegionEnum | _: RegionIndentEnum) :: _ => sepRegions
          case (_: RegionCase) :: tail => RegionArrow :: tail
          case _ => RegionArrow :: sepRegions
        })
      case _: RightBrace =>
        // produce outdent for every indented region before RegionBrace|RegionEnum
        @tailrec
        def nextRegions(in: List[SepRegion]): TokenRef = in match {
          case (_: RegionBrace | _: RegionEnum) :: xs => currRef(xs)
          case (x: SepRegionIndented) :: xs => mkOutdent(x, xs)
          case _ :: xs => nextRegions(xs)
          case Nil => currRef(Nil)
        }
        nextRegions(sepRegions)
      case _: RightBracket =>
        currRef(sepRegions match {
          case RegionBracket :: tail => tail
          case xs => xs
        })
      case _: EOF =>
        sepRegions match {
          case (x: SepRegionIndented) :: xs => mkOutdent(x, xs)
          case other => currRef(other)
        }
      case _: RightParen =>
        sepRegions match {
          case (x: SepRegionIndented) :: xs => mkOutdent(x, xs)
          case (_: RegionParen) :: xs => currRef(xs)
          case _ => currRef(sepRegions)
        }
      case _: LeftArrow =>
        currRef(sepRegions match {
          case RegionArrow :: tail => tail
          case xs => xs
        })
      case _: RightArrow =>
        currRef(sepRegions match {
          case RegionArrow :: tail =>
            // add case region for `match {` to calculate proper indentation
            // for statements in indentation dialects
            if (tail.isEmpty || dialect.allowSignificantIndentation && tail.head.indentOnArrow ||
              !isAheadNewLine(currPos)) tail
            else {
              val indentInCase = countIndent(nextPos)
              if (indentInCase > 0) RegionCase(indentInCase) :: tail else tail
            }
          case xs => xs
        })
      case _: KwFor if dialect.allowSignificantIndentation =>
        currRef(sepRegions match {
          case (_: RegionParen) :: tail => RegionParen(true) :: tail
          case _ => sepRegions
        })
      case _ => currRef(sepRegions)
    }

    if (currNonTrivial)
      if (isTrailingComma) nextToken(curr, currPos, currPos + 1, sepRegions)
      else nonTrivial
    else {
      var i = prevPos + 1
      var lastNewlinePos = -1
      var newlineStreak = false
      var newlines = false
      var hasLF = false
      while (i < nextPos) {
        val token = tokens(i)
        if (token.is[EOL]) {
          lastNewlinePos = i
          hasLF = true
          if (newlineStreak) newlines = true
          newlineStreak = true
        } else if (!token.is[Whitespace]) {
          newlineStreak = false
          hasLF |= token.is[MultilineComment]
        }
        i += 1
      }

      def lastWhitespaceToken = {
        val token = tokens(lastNewlinePos)
        val out =
          if (newlines) LFLF(token.input, token.dialect, token.start, token.end) else token
        TokenRef(sepRegions, out, lastNewlinePos)
      }

      def canProduceLF: Boolean = {
        lastNewlinePos != -1 &&
        (prevToken.is[Indentation.Outdent] || prevPos >= 0 && canEndStat(prevPos)) &&
        next.isNot[CantStartStat] && sepRegions.headOption.forall {
          case _: RegionBrace | _: RegionCase | _: RegionEnum => true
          case _: RegionIndent | _: RegionIndentEnum => true
          case x: RegionParen => x.canProduceLF
          case _ => false
        }
      }

      def getIfCanProduceLF =
        if (canProduceLF) Some(lastWhitespaceToken) else None

      val resOpt =
        if (next == null || !hasLF) None
        else if (!dialect.allowSignificantIndentation) getIfCanProduceLF
        else {
          val (nextIndent, indentPos) = countIndentAndNewlineIndex(nextPos)

          /**
           * Outdent is needed in following cases:
           *   - If indentation on next line is less than current and previous token can't continue
           *     expr on the next line
           *   - At the end of `match` block even if indentation level is not changed. Example:
           *     ```
           *     x match
           *     case 1 =>
           *     case 2 =>
           *     // <- produce outdent
           *     foo()
           *     ```
           */
          def getOutdentIfNeeded() = sepRegions match {
            case (r: SepRegionIndented) :: tail =>
              val ok =
                if (nextIndent < r.indent)
                  r.closeOnNonCase ||
                  !(!newlines && dialect.allowInfixOperatorAfterNL &&
                    isLeadingInfixOperator(nextPos) && // exclude leading infix op
                    tail.find(_.isIndented).forall(_.indent <= nextIndent)) &&
                  // need to check prev.prev in case of `end match`
                  (prev.isNot[CanContinueOnNextLine] || getPrevToken(prevPos).is[soft.KwEnd])
                else r.closeOnNonCase && next.isNot[KwCase] && nextIndent == r.indent
              if (ok) Some(mkOutdentTo(r, nextPos, tail)) else None
            case _ => None
          }

          /**
           * Indent is needed in the following cases:
           *   - Indetation on new line is greater and previous token can start indentation and
           *     token can start indentation
           *   - Indentation on the new line is the same and the next token is the first `case`
           *     clause in match. Example:
           *     ```
           *     x match // <- mk indent
           *     case 1 =>
           *     ```
           *
           * Notice: Indentation after `:` isn't hadled here. It's produced manually on the parser
           * level.
           */
          def getIndentIfNeeded = {
            val ok = nextIndent >= 0 && {

              val (currIndent, indentOnArrow) =
                sepRegions.headOption.fold((0, true))(r => (r.indent, r.indentOnArrow))

              // !next.is[RightBrace] - braces can sometimes have -1 and we can start indent on }
              if (nextIndent > currIndent && prev.is[RightArrow]) {
                indentOnArrow && next.isNot[RightBrace] && !isEndMarkerIntro(nextPos)
              } else if (nextIndent > currIndent) {
                // if does not work with indentation in pattern matches
                val shouldNotIndentIf =
                  prev.is[KwIf] && sepRegions.headOption.contains(RegionArrow)
                !shouldNotIndentIf && !next.is[RightBrace] && canStartIndent(prevPos)
              } else
                // always add indent for indented `match` block
                // check the previous token to avoid infinity loop
                prev.isAny[KwMatch, KwCatch] && !getPrevToken(prevPos).is[soft.KwEnd] &&
                next.is[KwCase] && prevToken.isNot[Indentation.Indent]
            }
            if (ok)
              Some(mkIndent(indentPos, RegionIndent(nextIndent, prev.is[KwMatch]) :: sepRegions))
            else None
          }

          getOutdentIfNeeded()
            .orElse { getIndentIfNeeded }
            .orElse { getIfCanProduceLF }
        }
      resOpt match {
        case Some(res) => res
        case _ => nextToken(prevToken, prevPos, nextPos, sepRegions)
      }
    }
  }

  override def prevTokenPos: Int = prev.pointPos
  override def prevToken: Token = prev.token

  override def tokenPos: Int = curr.pointPos
  override def token: Token = curr.token

  override def fork: TokenIterator =
    new LazyTokenIterator(scannerTokens, prev, curr)

}
