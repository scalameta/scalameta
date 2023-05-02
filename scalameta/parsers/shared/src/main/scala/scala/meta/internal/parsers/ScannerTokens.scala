package scala.meta.internal.parsers

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.inputs.Input
import scala.meta.internal.classifiers.classifier
import scala.meta.internal.tokenizers._
import scala.meta.internal.trees._
import scala.meta.prettyprinters._
import scala.meta.tokenizers._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens

final class ScannerTokens(val tokens: Tokens)(implicit dialect: Dialect) {

  import ScannerTokens._

  @tailrec
  final def getPrevIndex(index: Int): Int = {
    val prev = getPrevSafeIndex(index)
    if (tokens(prev).is[Trivia]) getPrevIndex(prev) else prev
  }

  def getPrevToken(index: Int): Token = tokens(getPrevIndex(index))

  @tailrec
  final def getNextIndex(index: Int): Int = {
    val next = getNextSafeIndex(index)
    if (tokens(next).is[Trivia]) getNextIndex(next) else next
  }

  def getNextToken(index: Int): Token = tokens(getNextIndex(index))

  @tailrec
  final def getStrictAfterSafe(index: Int): Int = {
    val token = tokens(index)
    if (token.isAny[HSpace, Comment]) getStrictAfterSafe(getNextSafeIndex(index))
    else index
  }

  def getStrictNext(index: Int): Int = getStrictAfterSafe(getNextSafeIndex(index))

  @inline def getPrevSafeIndex(index: Int): Int = Math.max(index - 1, 0)
  @inline def getNextSafeIndex(index: Int): Int = Math.min(index + 1, tokens.length - 1)

  // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
  // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
  // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
  // This leads to extremely dirty and seriously crazy code.
  implicit class XtensionTokenClass(token: Token) {

    def isClassOrObject = token.isAny[KwClass, KwObject]
    def isClassOrObjectOrEnum = isClassOrObject || (token.is[Ident] && dialect.allowEnums)

    def asString: String =
      s"[${token.getClass.getSimpleName}@${token.end}]${token.syntax.replace("\n", "")}"

    def isBackquoted: Boolean = {
      val text = token.text
      text.startsWith("`") && text.endsWith("`")
    }

    def isIdentSymbolicInfixOperator: Boolean = isBackquoted || {
      val text = token.text

      @tailrec
      def iter(idx: Int, nonEmpty: Boolean): Boolean = {
        val ch = text(idx)
        if (ch == '_') nonEmpty || idx > 0 && iter(idx - 1, false)
        else Chars.isOperatorPart(ch) && (idx == 0 || iter(idx - 1, true))
      }

      val len = text.length
      len == 0 || iter(len - 1, false)
    }
  }

  // https://github.com/lampepfl/dotty/blob/4e7ab609/compiler/src/dotty/tools/dotc/parsing/Scanners.scala#L435
  def canBeLeadingInfixArg(argToken: Token, argTokenPos: Int): Boolean =
    isExprIntro(argToken, argTokenPos) && (argToken match {
      case x: Ident => x.value.isUnaryOp || !x.isIdentSymbolicInfixOperator
      case _ => true
    })

  def getTokenAtLineStart(idx: Int): Token =
    getTokenAtLineStart(idx, tokens(idx))

  @tailrec
  private def getTokenAtLineStart(idx: Int, nextNonSpace: Token): Token =
    if (idx == 0) nextNonSpace
    else
      tokens(idx) match {
        case _: AtEOL => nextNonSpace
        case _: HSpace => getTokenAtLineStart(idx - 1, nextNonSpace)
        case t => getTokenAtLineStart(idx - 1, t)
      }

  val soft = new SoftKeywords(dialect)

  @classifier
  trait TypeIntro {
    def unapply(token: Token): Boolean = token match {
      case _: Ident | _: KwSuper | _: KwThis | _: LeftParen | _: At | _: Underscore | _: Unquote =>
        true
      case _: Literal => dialect.allowLiteralTypes
      case _ => false
    }
  }

  def isEndMarkerIntro(index: Int): Boolean = tokens(index).is[soft.KwEnd] && {
    val nextIndex = getStrictNext(index)
    tokens(nextIndex) match {
      case _: Ident | _: KwIf | _: KwWhile | _: KwFor | _: KwMatch | _: KwTry | _: KwNew |
          _: KwThis | _: KwGiven | _: KwVal =>
        tokens(getStrictNext(nextIndex)) match {
          case _: EOF | _: AtEOL => true
          case _ => false
        }
      case _ => false
    }
  }

  // then  else  do  catch  finally  yield  match
  @classifier
  trait CanContinueOnNextLine {
    def unapply(token: Token): Boolean = token match {
      case _: KwThen | _: KwElse | _: KwDo | _: KwCatch | _: KwFinally | _: KwYield | _: KwMatch =>
        true
      case _ => false
    }
  }

  def isExprIntro(token: Token, index: => Int): Boolean = token match {
    case _: Ident => !isSoftModifier(index) && !isEndMarkerIntro(index)
    case _: Literal | _: Interpolation.Id | _: Xml.Start | _: KwDo | _: KwFor | _: KwIf | _: KwNew |
        _: KwReturn | _: KwSuper | _: KwThis | _: KwThrow | _: KwTry | _: KwWhile | _: LeftParen |
        _: LeftBrace | _: Underscore | _: Unquote | _: MacroSplice | _: MacroQuote |
        _: Indentation.Indent =>
      true
    case _: LeftBracket => dialect.allowPolymorphicFunctions
    case _ => false
  }

  def isSoftModifier(index: Int): Boolean = {
    @inline def nextIsDclIntroOrModifierOr(f: Token => Boolean): Boolean = {
      val next = getNextIndex(index)
      isDclIntro(next) || isModifier(next) || f(tokens(next))
    }

    tokens(index).toString match {
      case soft.KwTransparent() => nextIsDclIntroOrModifierOr(_.is[KwTrait])
      case soft.KwOpaque() => nextIsDclIntroOrModifierOr(_ => false)
      case soft.KwInline() => nextIsDclIntroOrModifierOr(matchesAfterInlineMatchMod)
      case soft.KwOpen() | soft.KwInfix() => isDefIntro(getNextIndex(index))
      case _ => false
    }
  }

  @inline def isInlineMatchMod(index: Int): Boolean =
    tokens(index).is[soft.KwInline] && matchesAfterInlineMatchMod(getNextToken(index))

  private def matchesAfterInlineMatchMod(token: Token): Boolean = token match {
    case _: LeftParen | _: LeftBrace | _: KwNew | _: Ident | _: Literal | _: Interpolation.Id |
        _: Xml.Start | _: KwSuper | _: KwThis | _: MacroSplice | _: MacroQuote =>
      true
    case _ => false
  }

  def isCaseIntro(index: Int): Boolean =
    tokens(index).is[KwCase] && !getNextToken(index).isClassOrObject

  @tailrec
  final def isDefIntro(index: Int): Boolean = tokens(index) match {
    case _: At => true
    case _: Unquote | _: Ellipsis => isDefIntro(getNextIndex(index))
    case _: KwCase => getNextToken(index).isClassOrObjectOrEnum
    case _ => isDclIntro(index) || isModifier(index) || isTemplateIntro(index)
  }

  @tailrec
  final def isTemplateIntro(index: Int): Boolean = tokens(index) match {
    case _: At | _: KwClass | _: KwObject | _: KwTrait => true
    case _: Unquote => isTemplateIntro(getNextIndex(index))
    case _: KwCase => getNextToken(index).isClassOrObjectOrEnum
    case _ => isModifier(index)
  }

  @tailrec
  final def isDclIntro(index: Int): Boolean = tokens(index) match {
    case _: KwDef | _: KwType | _: KwEnum | _: KwVal | _: KwVar | _: KwGiven => true
    case _: Unquote => isDclIntro(getNextIndex(index))
    case _ => isKwExtension(index)
  }

  // Logic taken from the Scala 3 parser
  def isKwExtension(index: Int): Boolean =
    tokens(index).is[soft.KwExtension] && (getNextToken(index) match {
      case _: LeftParen | _: LeftBracket => true
      case _ => false
    })

  def isModifier(index: Int): Boolean =
    tokens(index).is[ModifierKeyword] || isSoftModifier(index)

  @classifier
  trait NonParamsModifier {
    def unapply(token: Token): Boolean = {
      token.toString match {
        case soft.KwOpen() | soft.KwOpaque() | soft.KwTransparent() | soft.KwInfix() => true
        case _ => false
      }
    }
  }

  def isNonlocalModifier(token: Token): Boolean = token match {
    case _: KwPrivate | _: KwProtected | _: KwOverride | soft.KwOpen() => true
    case _ => false
  }

  @classifier
  trait StatSeqEnd {
    def unapply(token: Token): Boolean = token match {
      case _: RightBrace | _: EOF | _: Indentation.Outdent => true
      case _ => false
    }
  }

  def isCaseDefEnd(token: Token, index: => Int): Boolean = token match {
    case _: RightBrace | _: RightParen | _: EOF | _: Indentation.Outdent => true
    case _: KwCase => !getNextToken(index).isClassOrObject
    case _: Ellipsis => getNextToken(index).is[KwCase]
    case _ => false
  }

  def canStartIndent(index: Int): Boolean = tokens(index) match {
    case _: KwYield | _: KwTry | _: KwCatch | _: KwFinally | _: KwMatch | _: KwDo | _: KwFor |
        _: KwThen | _: KwElse | _: KwWhile | _: KwIf | _: RightArrow | _: KwReturn | _: LeftArrow |
        _: ContextArrow =>
      true
    case _: Equals =>
      getNextToken(index) match {
        case _: KwMacro => false
        case _ => true
      }
    case _: KwWith =>
      val nextIndex = getNextIndex(index)
      isDefIntro(nextIndex) || (tokens(nextIndex) match {
        case _: KwImport | _: KwExport => true
        case _ => false
      })
    case _ => false
  }

  @classifier
  trait CantStartStat {
    def unapply(token: Token): Boolean = token match {
      case _: KwCatch | _: KwElse | _: KwExtends | _: KwFinally | _: KwForsome | _: KwMatch |
          _: KwWith | _: KwYield | _: RightParen | _: LeftBracket | _: RightBracket |
          _: RightBrace | _: Comma | _: Colon | _: Dot | _: Equals | _: Semicolon | _: Hash |
          _: RightArrow | _: LeftArrow | _: Subtype | _: Supertype | _: Viewbound | _: LF |
          _: LFLF | _: EOF =>
        true
      case _ => false
    }
  }

  def canEndStat(index: Int): Boolean = tokens(index) match {
    case _: Ident | _: KwGiven | _: Literal | _: Interpolation.End | _: Xml.End | _: KwReturn |
        _: KwThis | _: KwType | _: RightParen | _: RightBracket | _: RightBrace | _: Underscore |
        _: Ellipsis | _: Unquote =>
      true
    case _ => isEndMarkerIntro(getPrevIndex(index))
  }

  @classifier
  trait StatSep {
    def unapply(token: Token): Boolean = token match {
      case _: Semicolon | _: LF | _: LFLF | _: EOF => true
      case _ => false
    }
  }

  object Wildcard {
    def unapply(token: Token): Boolean =
      token.is[Underscore] || isStar(token)

    def isStar(token: Token): Boolean =
      dialect.allowStarWildcardImport && token.syntax == "*"
  }

  /**
   * When token on `tokenPosition` is not a whitespace and is a first non-whitespace character in a
   * current line then a result is a number of whitespace characters counted. Otherwise
   * {{{(-1, -1)}}} is returned.
   *
   * Returns a tuple2 where:
   *   - first value is indentation level
   *   - second is `LF` token index
   */
  private[parsers] def countIndentAndNewlineIndex(tokenPosition: Int): (Int, Int) = {
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

  private[parsers] def countIndent(tokenPosition: Int): Int =
    countIndentAndNewlineIndex(tokenPosition)._1

  private[parsers] def mkIndentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    new Indentation.Indent(token.input, token.dialect, token.start, token.start)
  }

  private[parsers] def mkOutdentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    new Indentation.Outdent(token.input, token.dialect, token.start, token.start)
  }

  private[parsers] def findOutdentPos(prevPos: Int, currPos: Int, region: SepRegion): Int = {
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
    if (iterPos < currPos) iter(iterPos, prevPos, if (tokens(prevPos).is[EOL]) 0 else -1)
    else if (tokens(currPos).is[EOF]) currPos
    else prevPos
  }

  @tailrec
  private[parsers] def isAheadNewLine(currentPosition: Int): Boolean = {
    val nextPos = currentPosition + 1
    nextPos < tokens.length && {
      val nextToken = tokens(nextPos)
      nextToken.is[LF] || nextToken.is[Trivia] && isAheadNewLine(nextPos)
    }
  }

  @inline
  private[parsers] def nextToken(ref: TokenRef): TokenRef =
    nextToken(ref.token, ref.pos, ref.nextPos, ref.regions)

  @tailrec
  private[parsers] def nextToken(
      prevToken: Token,
      prevPos: Int,
      currPos: Int,
      sepRegionsOrig: List[SepRegion]
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

    def mkOutdentTo(region: SepRegion, maxPointPos: Int, regions: List[SepRegion]) = {
      val pointPos = findOutdentPos(prevPos, maxPointPos, region)
      TokenRef(regions, mkOutdentToken(pointPos), prevPos, currPos, pointPos)
    }

    def mkOutdents(regions: List[SepRegion])(
        cond: SepRegionIndented => Boolean
    )(rest: List[SepRegion] => TokenRef): TokenRef =
      regions match {
        case (x: SepRegionIndented) :: xs if cond(x) => mkOutdentsTo(x, currPos, xs)(cond)(rest)
        case xs => rest(xs)
      }

    def mkOutdentsTo(region: SepRegion, maxPointPos: Int, regions: List[SepRegion])(
        cond: SepRegionIndented => Boolean
    )(rest: List[SepRegion] => TokenRef): TokenRef = {
      @tailrec
      def iter(ref: TokenRef, xs: List[SepRegion]): Unit = xs match {
        case (head: SepRegionIndented) :: tail =>
          if (cond(head)) {
            val tr = mkOutdentTo(head, maxPointPos, tail)
            ref.next = tr
            iter(tr, tail)
          }
        case _ => ref.next = rest(xs)
      }
      val res = mkOutdentTo(region, maxPointPos, regions)
      iter(res, regions)
      res
    }

    def currRef(regions: List[SepRegion], next: TokenRef = null): TokenRef =
      TokenRef(regions, curr, currPos, next)

    def nonTrivial(sepRegions: List[SepRegion]) = curr match {
      case _: LeftParen => currRef(RegionParen(false) :: sepRegions)
      case _: LeftBracket => currRef(RegionBracket :: sepRegions)
      case _: Comma =>
        sepRegions match {
          case (head: SepRegionIndented) :: tail
              if tail.find(!_.isIndented).exists(_.isInstanceOf[RegionParen]) =>
            mkOutdentsTo(head, currPos, tail)(_ => true)(currRef(_))
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
        val regions = sepRegions.dropWhile {
          case _: RegionBrace | _: RegionEnum | _: SepRegionIndented => false
          case _ => true
        }
        mkOutdents(regions)(_ => true) { rs =>
          currRef(dropUntil(rs) {
            case _: RegionBrace | _: RegionEnum => true
            case _ => false
          })
        }
      case _: RightBracket =>
        currRef(dropUntil(sepRegions)(_ eq RegionBracket))
      case _: EOF =>
        mkOutdents(sepRegions.filter(_.isIndented))(_ => true)(currRef(_))
      case _: RightParen =>
        mkOutdents(sepRegions)(_ => true) { rs =>
          currRef(dropUntil(rs)(_.isInstanceOf[RegionParen]))
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
      if (isTrailingComma) nextToken(curr, currPos, currPos + 1, sepRegionsOrig)
      else nonTrivial(sepRegionsOrig)
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

      def lastWhitespaceToken(regions: List[SepRegion]) = {
        val token = tokens(lastNewlinePos)
        val out =
          if (newlines) LFLF(token.input, token.dialect, token.start, token.end) else token
        TokenRef(regions, out, lastNewlinePos, null)
      }

      def canProduceLF(regions: List[SepRegion]): Boolean = {
        lastNewlinePos != -1 &&
        (prevToken.is[Indentation.Outdent] || prevPos >= 0 && canEndStat(prevPos)) &&
        next.isNot[CantStartStat] && regions.headOption.forall {
          case _: RegionBrace | _: RegionCase | _: RegionEnum => true
          case _: RegionIndent | _: RegionIndentEnum => true
          case x: RegionParen => x.canProduceLF
          case _ => false
        }
      }

      def getIfCanProduceLF(regions: List[SepRegion]) =
        if (canProduceLF(regions)) Some(lastWhitespaceToken(regions)) else None

      def isLeadingInfix() =
        !newlines && dialect.allowInfixOperatorAfterNL &&
          next.is[Ident] && next.isIdentSymbolicInfixOperator && {
            val nextSafeIndex = getNextSafeIndex(nextPos)
            tokens(nextSafeIndex).is[Whitespace] && {
              val argPos = getStrictAfterSafe(nextSafeIndex)
              canBeLeadingInfixArg(tokens(argPos), argPos)
            }
          }

      val resOpt =
        if (next == null || !hasLF) None
        else if (!dialect.allowSignificantIndentation) getIfCanProduceLF(sepRegionsOrig)
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
          def getOutdentIfNeeded(sepRegions: List[SepRegion]) = sepRegions match {
            case (r: SepRegionIndented) :: xs if {
                  if (nextIndent < r.indent)
                    r.closeOnNonCase ||
                    !(isLeadingInfix() && // exclude leading infix op
                      xs.find(_.isIndented).forall(_.indent <= nextIndent)) &&
                    // need to check prev.prev in case of `end match`
                    (prev.isNot[CanContinueOnNextLine] || getPrevToken(prevPos).is[soft.KwEnd])
                  else outdentOnCase(r)
                } =>
              emitOutdents(r, xs)(x => nextIndent < x.indent || outdentOnCase(x))
            case _ => None
          }
          def outdentOnCase(r: SepRegion): Boolean =
            r.closeOnNonCase && next.isNot[KwCase] && nextIndent == r.indent
          def emitOutdents(r: SepRegion, rs: List[SepRegion])(f: SepRegionIndented => Boolean) =
            Some(mkOutdentsTo(r, nextPos, rs)(f)(_ => null))

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
          def getIndentIfNeeded(sepRegions: List[SepRegion]) = {
            def exceedsIndent =
              nextIndent > sepRegions.find(_.indent > 0).fold(0)(_.indent)
            def emitIndent(regions: List[SepRegion], closeOnNonCase: Boolean = false) =
              Some(mkIndent(indentPos, RegionIndent(nextIndent, closeOnNonCase) :: regions))
            // !next.is[RightBrace] - braces can sometimes have -1 and we can start indent on }
            prev match {
              case _ if nextIndent < 0 || next.is[RightBrace] => None
              case _ if prevToken.is[Indentation.Indent] => None
              case _ if prevToken.is[Indentation.Outdent] => None
              // if does not work with indentation in pattern matches
              case _: KwIf if sepRegions.headOption.contains(RegionArrow) => None
              case _: KwCatch | _: KwMatch =>
                // always add indent for indented `match` block
                // check the previous token to avoid infinity loop
                val ok = next.is[KwCase] && !getPrevToken(prevPos).is[soft.KwEnd]
                if (ok) emitIndent(sepRegions, true) else None
              case _ if !exceedsIndent => None
              case _: RightArrow =>
                val ok = sepRegions.headOption.forall(_.indentOnArrow) &&
                  !isEndMarkerIntro(nextPos)
                if (ok) emitIndent(sepRegions) else None
              case _ =>
                val ok = canStartIndent(prevPos)
                if (ok) emitIndent(sepRegions) else None
            }
          }

          def iter(regions: List[SepRegion]): Option[TokenRef] = {
            val res = getOutdentIfNeeded(regions).orElse(getIndentIfNeeded(regions))
            if (res.isEmpty) getIfCanProduceLF(regions)
            else res
          }

          iter(sepRegionsOrig)
        }
      resOpt match {
        case Some(res) => res
        case _ => nextToken(prevToken, prevPos, nextPos, sepRegionsOrig)
      }
    }
  }

}

object ScannerTokens {

  def apply(input: Input)(implicit dialect: Dialect): ScannerTokens = {
    new ScannerTokens(input.tokenize.get)
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

  @tailrec
  def dropUntil(regions: List[SepRegion])(f: SepRegion => Boolean): List[SepRegion] =
    regions match {
      case head :: tail => if (f(head)) tail else dropUntil(tail)(f)
      case _ => Nil
    }

}
