package scala.meta.internal.parsers

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens

object LazyTokenIterator {

  def apply(st: ScannerTokens)(implicit dialect: Dialect): LazyTokenIterator =
    new LazyTokenIterator(st, Nil, TokenRef(st.tokens(0), 0), -1)

  private class TokenRef private (
      val token: Token,
      val pos: Int,
      val nextPos: Int,
      val pointPos: Int
  )

  private object TokenRef {
    def apply(token: Token, pos: Int) = new TokenRef(token, pos, pos + 1, pos)
    def apply(token: Token, pos: Int, nextPos: Int, pointPos: Int) =
      new TokenRef(token, pos, nextPos, pointPos)
  }

  private def multilineCommentIndent(t: Comment): Int = {
    @tailrec
    def loop(idx: Int, indent: Int, isAfterNewline: Boolean): Int = {
      if (idx == t.value.length) indent
      else {
        t.value.charAt(idx) match {
          case '\n' => loop(idx + 1, 0, isAfterNewline = true)
          case ' ' | '\t' if isAfterNewline => loop(idx + 1, indent + 1, isAfterNewline)
          case _ => loop(idx + 1, indent, isAfterNewline = false)
        }
      }
    }
    loop(0, 0, false)
  }

}

private[parsers] class LazyTokenIterator private (
    private val scannerTokens: ScannerTokens,
    private var sepRegions: List[SepRegion],
    private var curr: LazyTokenIterator.TokenRef,
    private var prevPos: Int
)(implicit dialect: Dialect)
    extends TokenIterator {

  import LazyTokenIterator._
  import scannerTokens._

  override def next(): Unit = {
    val (newSepRegions, newTokenRef) = nextToken(curr.pos, curr.nextPos, sepRegions)
    prevPos = curr.pointPos
    curr = newTokenRef
    sepRegions = newSepRegions
  }

  private def observeIndented0(f: (Int, List[SepRegion]) => List[SepRegion]): Boolean = {
    if (!dialect.allowSignificantIndentation) false
    else {
      val existingIndent = sepRegions.find(_.isIndented).fold(0)(_.indent)
      val (expected, pointPos) = countIndentAndNewlineIndex(tokenPos)
      if (expected > existingIndent) {
        sepRegions = f(expected, sepRegions)
        val indent = mkIndentToken(pointPos)
        curr = TokenRef(indent, curr.pos, curr.pos, pointPos)
        true
      } else false
    }
  }

  /**
   * Deals with different rules for indentation after self type arrow.
   */
  def undoIndent(): Unit = {
    sepRegions match {
      case (region: SepRegionIndented) :: others if curr.token.is[Indentation.Indent] =>
        next()
        sepRegions = sepRegions match {
          // deal with  region added by `case` in enum after self type
          case RegionArrow :: _ => others
          // if no region was added
          case `region` :: _ => others
          // keep any added region in `next()`
          case head :: _ => head :: others
          case _ => sepRegions
        }
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
      sepRegions.headOption.fold(0)(_.indent)
    else
      foundIndentation
  }

  def previousIndentation: Int = sepRegions match {
    case _ :: r :: _ => r.indent
    case _ => 0
  }

  def observeOutdented(): Boolean =
    dialect.allowSignificantIndentation && (sepRegions match {
      case (region: SepRegionIndented) :: tail if (curr.token match {
            case _: KwThen | _: KwElse | _: KwDo | _: KwYield => true
            case _: KwMatch | _: KwCatch | _: KwFinally => true
            case _ => false
          }) =>
        sepRegions = tail
        val outdentPos = findOutdentPos(prevPos, curr.pos, region)
        val outdent = mkOutdentToken(outdentPos)
        curr = TokenRef(outdent, curr.pos, curr.pos, outdentPos)
        true
      case _ => false
    })

  private def mkIndentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    new Indentation.Indent(token.input, token.dialect, token.start, token.start)
  }

  private def mkOutdentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    new Indentation.Outdent(token.input, token.dialect, token.start, token.start)
  }

  private def findOutdentPos(prevPos: Int, currPos: Int, region: SepRegion): Int = {
    val outdent = region.indent
    @tailrec
    def iter(i: Int, pos: Int, indent: Int): Int = {
      if (i >= currPos) {
        if (pos < currPos) pos else currPos - 1
      } else {
        tokens(i) match {
          case _: EOL =>
            iter(i + 1, i, 0)
          case _: HSpace if indent >= 0 =>
            iter(i + 1, pos, indent + 1)
          case _: Whitespace =>
            iter(i + 1, pos, indent)
          case _: Comment if indent < 0 || outdent <= indent =>
            iter(i + 1, i + 1, -1)
          case _ => pos
        }
      }
    }

    val iterPos = 1 + prevPos
    if (iterPos < currPos) iter(iterPos, prevPos, -1)
    else if (tokens(currPos).is[EOF]) currPos
    else prevPos
  }

  @tailrec
  private def nextToken(
      prevPos: Int,
      currPos: Int,
      sepRegions: List[SepRegion]
  ): (List[SepRegion], TokenRef) = {
    val prev = if (prevPos >= 0) tokens(prevPos) else null
    val curr = tokens(currPos)
    val (nextPos, next) = {
      @tailrec
      def iter(i: Int): (Int, Token) =
        if (i == tokens.length) (-1, null)
        else
          tokens(i) match {
            case _: Trivia => iter(i + 1)
            case t => (i, t)
          }
      iter(currPos + 1)
    }

    def isTrailingComma: Boolean =
      dialect.allowTrailingCommas &&
        curr.is[Comma] &&
        next.is[CloseDelim] &&
        next.pos.startLine > curr.pos.endLine

    def mkIndent(pointPos: Int): TokenRef =
      TokenRef(mkIndentToken(pointPos), prevPos, currPos, pointPos)

    def mkOutdent(region: SepRegion): TokenRef =
      mkOutdentTo(region, currPos)

    def mkOutdentTo(region: SepRegion, maxPointPos: Int): TokenRef = {
      val pointPos = findOutdentPos(prevPos, maxPointPos, region)
      TokenRef(mkOutdentToken(pointPos), prevPos, currPos, pointPos)
    }

    def currRef: TokenRef = TokenRef(curr, currPos)

    def nonTrivial = curr match {
      case _: LeftParen => (RegionParen(false) :: sepRegions, currRef)
      case _: LeftBracket => (RegionBracket :: sepRegions, currRef)
      case _: Comma =>
        sepRegions match {
          case (head: SepRegionIndented) :: tail
              if tail.find(!_.isIndented).exists(_.isInstanceOf[RegionParen]) =>
            (tail, mkOutdent(head))
          case _ => (sepRegions, currRef)
        }
      case _: LeftBrace =>
        val indentInBrace = if (isAheadNewLine(currPos)) countIndent(nextPos) else -1
        // After encountering keyword Enum we add artificial '{' on top of stack.
        // Then always after Enum next token is '{'. On token '{' we check if top of stack is '{'
        // (which in case of enum is always true) and replace it with '$'.
        // Now if we have token 'case' and top of stack is '$' we know it is Enum-case.
        // In any other case it is 'match-case' or 'try-case'
        val nextRegions =
          if (sepRegions.headOption.contains(RegionEnumArtificialMark))
            RegionEnum(indentInBrace) :: sepRegions.tail
          else {
            val indentOnArrow = !(prev.is[KwMatch] || prev.is[KwCatch])
            RegionBrace(indentInBrace, indentOnArrow) :: sepRegions
          }
        (nextRegions, currRef)
      case _: KwEnum =>
        (RegionEnumArtificialMark :: sepRegions, currRef)
      case _ if isCaseIntro(currPos) =>
        val nextRegions = sepRegions.headOption match {
          case Some(_: RegionEnum | _: RegionIndentEnum) => sepRegions
          case Some(_: RegionCase) => RegionArrow :: sepRegions.tail
          case _ => RegionArrow :: sepRegions
        }
        (nextRegions, currRef)
      case _: RightBrace =>
        // produce outdent for every indented region before RegionBrace|RegionEnum
        @tailrec
        def nextRegions(in: List[SepRegion]): (List[SepRegion], TokenRef) = {
          in match {
            case (_: RegionBrace | _: RegionEnum) :: xs =>
              (xs, currRef)
            case (x: SepRegionIndented) :: xs =>
              (xs, mkOutdent(x))
            case _ :: xs =>
              nextRegions(xs)
            case Nil =>
              (Nil, currRef)
          }
        }
        nextRegions(sepRegions)
      case _: RightBracket =>
        val nextRegions =
          if (sepRegions.headOption.contains(RegionBracket)) sepRegions.tail
          else sepRegions
        (nextRegions, currRef)
      case _: EOF =>
        sepRegions match {
          case (x: SepRegionIndented) :: xs => (xs, mkOutdent(x))
          case other => (other, currRef)
        }
      case _: RightParen =>
        sepRegions match {
          case (x: SepRegionIndented) :: xs => (xs, mkOutdent(x))
          case (_: RegionParen) :: xs => (xs, currRef)
          case _ => (sepRegions, currRef)
        }
      case _: LeftArrow =>
        val nextRegions =
          if (sepRegions.headOption.contains(RegionArrow)) sepRegions.tail
          else sepRegions
        (nextRegions, currRef)
      case _: RightArrow =>
        val nextRegions =
          if (sepRegions.headOption.contains(RegionArrow)) {
            // add case region for `match {` to calculate proper indentation
            // for statements in indentation dialects
            val newRegions = sepRegions.tail
            val shouldNotProduceIndentation =
              !dialect.allowSignificantIndentation ||
                newRegions.headOption.exists(!_.indentOnArrow)
            lazy val indentInCase = if (isAheadNewLine(currPos)) countIndent(nextPos) else -1
            if (newRegions.nonEmpty && shouldNotProduceIndentation && indentInCase > 0)
              RegionCase(indentInCase) :: newRegions
            else
              newRegions
          } else sepRegions
        (nextRegions, currRef)
      case _: KwFor if dialect.allowSignificantIndentation =>
        val updatedSepRegions = sepRegions match {
          case (_: RegionParen) :: tail => RegionParen(true) :: tail
          case _ => sepRegions
        }
        (updatedSepRegions, currRef)
      case _ =>
        (sepRegions, currRef)
    }
    if (isTrailingComma) nextToken(currPos, currPos + 1, sepRegions)
    else if (curr.isNot[Trivia]) nonTrivial
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
        TokenRef(out, lastNewlinePos)
      }

      def canProduceLF: Boolean = {
        lastNewlinePos != -1 &&
        prev != null && (token.is[Indentation.Outdent] || canEndStat(prevPos)) &&
        next.isNot[CantStartStat] && sepRegions.headOption.forall {
          case _: RegionBrace | _: RegionCase | _: RegionEnum => true
          case _: RegionIndent | _: RegionIndentEnum => true
          case x: RegionParen => x.canProduceLF
          case _ => false
        }
      }

      def getIfCanProduceLF =
        if (canProduceLF) Some((sepRegions, lastWhitespaceToken))
        else None

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
              if (ok) Some((tail, mkOutdentTo(r, nextPos))) else None
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
                ((prev.is[KwMatch] || prev.is[KwCatch]) && !getPrevToken(prevPos).is[soft.KwEnd]) &&
                next.is[KwCase] && token.isNot[Indentation.Indent]
            }
            if (ok) Some {
              val region = RegionIndent(nextIndent, prev.is[KwMatch])
              (region :: sepRegions, mkIndent(indentPos))
            }
            else None
          }

          getOutdentIfNeeded()
            .orElse { getIndentIfNeeded }
            .orElse { getIfCanProduceLF }
        }
      resOpt match {
        case Some(res) => res
        case _ => nextToken(prevPos, nextPos, sepRegions)
      }
    }
  }

  override def prevTokenPos: Int = prevPos

  override def tokenPos: Int = curr.pointPos

  override def token: Token = curr.token

  override def fork: TokenIterator =
    new LazyTokenIterator(scannerTokens, sepRegions, curr, prevPos)

  /**
   * When token on `tokenPosition` is not a whitespace and is a first non-whitespace character in a
   * current line then a result is a number of whitespace characters counted. Otherwise
   * {{{(-1, -1)}}} is returned.
   *
   * Returns a tuple2 where:
   *   - first value is indentation level
   *   - second is `LF` token index
   */
  private def countIndentAndNewlineIndex(tokenPosition: Int): (Int, Int) = {
    @tailrec
    def countIndentInternal(pos: Int, acc: Int = 0): (Int, Int) = {
      if (pos < 0) (acc, pos)
      else {
        val token = tokens(pos)
        token match {
          case _: EOL | _: BOF => (acc, pos)
          case AsMultilineComment(c) => (multilineCommentIndent(c), pos)
          case _: Comment => countIndentInternal(pos - 1)
          case _: HSpace => countIndentInternal(pos - 1, acc + 1)
          case _ => (-1, -1)
        }
      }
    }

    if (tokens(tokenPosition).is[Whitespace]) (-1, -1)
    else countIndentInternal(tokenPosition - 1)
  }

  private def countIndent(tokenPosition: Int): Int =
    countIndentAndNewlineIndex(tokenPosition)._1

  @tailrec
  private def isAheadNewLine(currentPosition: Int): Boolean = {
    val nextPos = currentPosition + 1
    nextPos < tokens.length && {
      val nextToken = tokens(nextPos)
      nextToken.is[LF] || nextToken.is[Trivia] && isAheadNewLine(nextPos)
    }
  }

}
