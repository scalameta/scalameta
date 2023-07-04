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
        tokens(getStrictNext(nextIndex)).is[AtEOLorF]
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

    tokens(index).text match {
      case soft.KwTransparent() => nextIsDclIntroOrModifierOr(_.is[KwTrait])
      case soft.KwOpaque() => nextIsDclIntroOrModifierOr(_ => false)
      case soft.KwInline() => nextIsDclIntroOrModifierOr(matchesAfterInlineMatchMod)
      case soft.KwOpen() | soft.KwInfix() | soft.KwErased() => isDefIntro(getNextIndex(index))
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
      token.text match {
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
      case _: Semicolon | _: AtEOLorF => true
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

  private[parsers] def findOutdentPos(prevPos: Int, currPos: Int, outdent: Int): Int = {
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

    def mkIndent(pointPos: Int, regions: List[SepRegion], next: TokenRef = null): TokenRef =
      TokenRef(regions, mkIndentToken(pointPos), prevPos, currPos, pointPos, next)

    def mkOutdentTo(region: SepRegionIndented, maxPointPos: Int, regions: List[SepRegion]) = {
      mkOutdentAt(region.indent, maxPointPos, regions)
    }

    def mkOutdentAt(outdent: Int, maxPointPos: Int, regions: List[SepRegion]) = {
      val pointPos = findOutdentPos(prevPos, maxPointPos, outdent)
      TokenRef(regions, mkOutdentToken(pointPos), prevPos, currPos, pointPos)
    }

    @tailrec
    def mkOutdentsOpt(maxPointPos: Int, regions: List[SepRegion])(
        f: PartialFunction[List[SepRegion], (Either[SepRegionIndented, Boolean], List[SepRegion])]
    ): Either[List[SepRegion], TokenRef] =
      f.lift(regions) match {
        case None => Left(regions)
        case Some((Left(r), rs)) =>
          Right(mkOutdents(r, maxPointPos, rs)(f))
        case Some((Right(skip), rs)) =>
          if (skip && (rs ne regions)) mkOutdentsOpt(maxPointPos, rs)(f) else Left(rs)
      }

    def mkOutdents(region: SepRegionIndented, maxPointPos: Int, regions: List[SepRegion])(
        f: PartialFunction[List[SepRegion], (Either[SepRegionIndented, Boolean], List[SepRegion])]
    ): TokenRef = {
      @tailrec
      def iter(ref: TokenRef, xs: List[SepRegion]): Unit =
        f.lift(xs) match {
          case None =>
          case Some((Left(r), rs)) =>
            val tr = mkOutdentTo(r, maxPointPos, rs)
            ref.next = tr
            iter(tr, rs)
          case Some((Right(skip), rs)) =>
            if (skip && (rs ne xs)) iter(ref, rs)
            else if (currNonTrivial) ref.next = currRef(rs)
        }
      val res = mkOutdentTo(region, maxPointPos, regions)
      iter(res, regions)
      res
    }

    def currRef(regions: List[SepRegion], next: TokenRef = null): TokenRef =
      TokenRef(regions, curr, currPos, next)

    def outdentThenCurrRef(
        ri: RegionIndent,
        rs: List[SepRegion],
        rn: Option[SepRegion] = None
    ): TokenRef = {
      val ref = mkOutdentTo(ri, currPos, rs)
      ref.next = currRef(rn.fold(rs)(_ :: rs))
      ref
    }

    def getCaseIntro(sepRegions: List[SepRegion]) = {
      def markRegions() = RegionCaseMark :: sepRegions
      next match {
        /* Several facts:
         * - cases can be at the same level as catch/match; therefore, a non-case token
         *   starting a line at the same level as case can terminate the entire match
         * - however, the compiler seems to allow (although produces a warning) exactly
         *   one line of a case body which follows `catch case` (both on one line) to be
         *   at the same level
         *
         * To handle both cases, we need to insert an indent before the first case so that it
         * can be removed on the first line which matches this indent, without potentially
         * removing the indent around the match statement.
         *
         * If case is on a different line after catch/match, we will insert an indent when
         * handling newlines below; therefore, when there's no newline, we should explicitly
         * insert an indent here, relative to outer region. */
        case _: KwCase // case follows catch on same line
            if dialect.allowSignificantIndentation && curr.pos.endLine == next.pos.startLine =>
          val regions = markRegions()
          val nextRegions = RegionIndent(findIndent(sepRegions)) :: regions
          val nextToken = mkIndentToken(nextPos)
          currRef(regions, TokenRef(nextRegions, nextToken, currPos, nextPos, nextPos))
        case _: KwCase | _: LeftBrace => currRef(markRegions())
        case _ => currRef(sepRegions)
      }
    }

    def getTemplateInherit(sepRegions: List[SepRegion]): TokenRef =
      currRef(sepRegions match {
        case RegionTemplateMark :: rs => RegionTemplateInherit :: rs
        case rs => rs
      })

    def nonTrivial(sepRegions: List[SepRegion]) = curr match {
      case _: EOF =>
        mkOutdentsOpt(currPos, sepRegions) {
          case (r: SepRegionIndented) :: rs => (Left(r), rs)
          case _ :: rs => (Right(true), rs)
        }.fold(currRef(_), identity)
      case _: Comma =>
        sepRegions match {
          case (head: SepRegionIndented) :: tail
              if tail.find(!_.isIndented).contains(RegionParen) =>
            mkOutdents(head, currPos, tail) {
              case (r: SepRegionIndented) :: rs => (Left(r), rs)
              case rs => (Right(false), rs)
            }
          case _ => currRef(sepRegions)
        }
      case _: KwEnum => currRef(RegionTemplateMark :: sepRegions)
      case _: KwObject | _: KwClass | _: KwTrait | _: KwPackage | _: KwNew
          if dialect.allowSignificantIndentation =>
        currRef(RegionTemplateMark :: sepRegions)
      case _: KwMatch if !prev.is[soft.KwEnd] =>
        getCaseIntro(sepRegions)
      case _: KwCatch =>
        getCaseIntro(sepRegions)
      case _: KwCase if !next.isClassOrObject =>
        def expr() = new RegionCaseExpr(countIndent(currPos))
        currRef(sepRegions match {
          // `case` follows the body of a previous case
          case (_: RegionCaseBody) :: xs => expr() :: xs
          // x could be RegionIndent or RegionBrace
          case x :: RegionCaseMark :: xs => expr() :: x :: xs
          case (_: RegionBrace) :: (_: RegionFor | RegionTemplateBody) :: _ => sepRegions
          case (_: RegionBrace) :: _ => expr() :: sepRegions
          // `case` is at top-level (likely quasiquote)
          case Nil if prevPos == 0 => expr() :: Nil
          case xs => xs
        })
      case _: KwFinally =>
        sepRegions match {
          // covers case when finally follows catch case without a newline
          // otherwise, these two regions would have been removed already
          case (_: RegionCaseBody) :: (r: RegionIndent) :: xs =>
            val ref = mkOutdentTo(r, currPos, xs)
            ref.next = currRef(xs)
            ref
          case xs => currRef(xs)
        }
      case _: LeftBrace =>
        val lbRegions = sepRegions match {
          case RegionTemplateMark :: rs => RegionTemplateBody :: rs
          case RegionTemplateInherit :: rs if !prev.is[KwExtends] => RegionTemplateBody :: rs
          case rs => rs
        }
        currRef(RegionBrace(countIndent(nextPos)) :: lbRegions)
      case _: RightBrace =>
        // produce outdent for every indented region before RegionBrace|RegionEnum
        mkOutdentsOpt(currPos, sepRegions) {
          case (r: SepRegionIndented) :: rs => (Left(r), rs)
          case (_: RegionBrace) :: (RegionTemplateBody | RegionCaseMark) :: rs =>
            (Right(false), rs)
          case (_: RegionBrace) :: rs => (Right(false), rs)
          case _ :: rs => (Right(true), rs)
        }.fold(currRef(_), identity)
      case _: LeftBracket => currRef(RegionBracket :: sepRegions)
      case _: RightBracket =>
        currRef(dropUntil(sepRegions)(_ eq RegionBracket))
      case _: LeftParen => currRef(RegionParen :: sepRegions)
      case _: RightParen =>
        mkOutdentsOpt(currPos, sepRegions) {
          case (r: SepRegionIndented) :: rs => (Left(r), rs)
          case RegionParen :: rs => (Right(false), rs)
          case _ :: rs => (Right(true), rs)
        }.fold(currRef(_), identity)
      case _: RightArrow =>
        currRef(sepRegions match {
          case (_: RegionCaseExpr) :: xs =>
            // add case region for `match {` to calculate proper indentation
            // for statements in indentation dialects
            val bodyIndent = xs match {
              case (ri: RegionIndent) :: _ => ri.indent
              case _ => countIndent(nextPos)
            }
            new RegionCaseBody(bodyIndent, curr) :: xs
          case xs => xs
        })
      case _: KwFor =>
        currRef(RegionFor(next) :: sepRegions)
      case _: KwWhile if dialect.allowSignificantIndentation =>
        currRef(RegionWhile(next) :: sepRegions)
      case _: KwIf if dialect.allowSignificantIndentation =>
        currRef(sepRegions match {
          case (_: RegionCaseExpr | _: RegionFor) :: _ => sepRegions
          case (_: RegionDelim) :: (_: RegionFor) :: _ => sepRegions
          case xs => RegionIf(next) :: xs
        })
      case _: KwThen =>
        sepRegions match {
          case (r: RegionIndent) :: (_: RegionIf) :: xs =>
            outdentThenCurrRef(r, xs, Some(RegionThen))
          case (_: RegionIf) :: xs => currRef(RegionThen :: xs)
          case xs => currRef(RegionThen :: xs)
        }
      case _: KwElse if dialect.allowSignificantIndentation =>
        sepRegions match {
          case (r: RegionIndent) :: RegionThen :: xs =>
            outdentThenCurrRef(r, xs)
          case (_: RegionControl) :: xs => currRef(xs)
          case xs => currRef(xs)
        }
      case _: KwDo | _: KwYield =>
        sepRegions match {
          case (r: RegionIndent) :: (_: RegionControl) :: xs =>
            outdentThenCurrRef(r, xs)
          case (_: RegionControl) :: xs => currRef(xs)
          case xs => currRef(xs)
        }
      case _: KwDef | _: KwVal | _: KwVar
          if dialect.allowSignificantIndentation && !prev.is[soft.KwEnd] =>
        currRef(RegionDefMark :: sepRegions)
      case _: Colon =>
        sepRegions match {
          case RegionDefMark :: rs => currRef(RegionDefType :: rs)
          case rs => currRef(rs)
        }
      case _: Equals =>
        sepRegions match {
          case (_: RegionDefDecl) :: rs => currRef(rs)
          case rs => currRef(rs)
        }
      case _: KwExtends => getTemplateInherit(sepRegions)
      case _: Ident =>
        curr.text match {
          case soft.KwDerives() => getTemplateInherit(sepRegions)
          case soft.KwExtension()
              if next.isAny[LeftParen, LeftBracket] &&
                prevToken.isAny[StatSep, Indentation, LeftBrace] =>
            currRef(RegionExtensionMark :: sepRegions)
          case _ => currRef(sepRegions)
        }
      case _ => currRef(sepRegions)
    }

    def getNonTrivialRegions(regions: List[SepRegion]) = regions match {
      case RegionExtensionMark :: rs =>
        curr match {
          case _: LeftBrace => RegionTemplateBody :: rs
          case _: LeftParen | _: LeftBracket => regions
          case _ => rs
        }
      case all @ (r: RegionControl) :: rs if r.isNotTerminatingTokenIfOptional(curr) =>
        r match {
          case rc: RegionControlMaybeCond if prev.is[RightParen] =>
            curr match {
              case _: Dot => rc.asCond() :: rs
              // might continue cond or start body
              case _: Ident | _: LeftBrace | _: LeftBracket | _: LeftParen | _: Underscore
                  if dialect.allowSignificantIndentation =>
                rc.asCondOrBody() :: rs
              case _ =>
                rc.asBody().fold(rs)(_ :: rs)
            }
          case RegionForMaybeParens if prev.is[RightParen] =>
            curr match {
              case _: LeftArrow => RegionForOther :: rs
              case _ => rs
            }
          case RegionForBraces if prev.is[RightBrace] => rs
          case _ =>
            if (prevToken.is[AtEOL] || curr.is[CloseDelim]) rs else all
        }
      case rs => rs
    }

    if (currNonTrivial)
      if (isTrailingComma) nextToken(curr, currPos, currPos + 1, sepRegionsOrig)
      else nonTrivial(getNonTrivialRegions(sepRegionsOrig))
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

      def getIfCanProduceLF(regions: List[SepRegion]) = {
        if (lastNewlinePos != -1 &&
          (prevToken.is[Indentation.Outdent] || prevPos >= 0 && canEndStat(prevPos)) &&
          next.isNot[CantStartStat])
          (regions match {
            case Nil => Some(regions)
            case RegionDefType :: rs if !next.isAny[LeftParen, LeftBracket, Equals] => Some(rs)
            // `extends` and `with` are covered by canEndStat() and CantStartStat above
            case RegionTemplateMark :: rs if !next.isAny[LeftBrace, soft.KwDerives] => Some(rs)
            case RegionTemplateInherit :: rs if !next.isAny[LeftBrace, soft.KwDerives] =>
              if (prev.is[soft.KwDerives]) None else Some(rs)
            case (_: CanProduceLF) :: _ => Some(regions)
            case _ => None
          }).map(rs => Right(lastWhitespaceToken(rs)))
        else None
      }

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
          def getOutdentIfNeeded(sepRegions: List[SepRegion]) =
            mkOutdentsOpt(nextPos, sepRegions) {
              case (rc: RegionCaseBody) :: (r: RegionIndent) :: xs if (next match {
                    case _: KwCase => nextIndent < r.indent
                    case _: KwFinally => nextIndent <= r.indent
                    case _ => nextIndent < r.indent || nextIndent == r.indent && rc.arrow.ne(prev)
                  }) =>
                (Left(r), xs)
              case (r: RegionIndentTry) :: xs if (next match {
                    case _: KwCatch | _: KwFinally => nextIndent <= r.indent
                    case _ => nextIndent < r.indent
                  }) =>
                (Left(r), xs)
              case (_: RegionNonDelimNonIndented) :: rs if (prev match {
                    // [then]  else  do  [catch]  finally  yield  [match]
                    case _: KwThen | _: KwCatch | _: KwMatch => false
                    case _ => shouldOutdent(rs)
                  }) =>
                (Right(true), rs)
              case (r: RegionIndent) :: (rs @ (rc: RegionControl) :: xs) if nextIndent < r.indent =>
                (Left(r), if (rc.isNotTerminatingTokenIfOptional(next)) xs else rs)
              case (r: RegionIndent) :: RegionTemplateBody :: xs if nextIndent < r.indent =>
                (Left(r), xs)
              case (r: SepRegionIndented) :: xs if (prev match {
                    // then  [else]  [do]  catch  [finally]  [yield]  match
                    case _: KwElse | _: KwDo | _: KwFinally | _: KwYield => false
                    // exclude leading infix op
                    case _ => nextIndent < r.indent && (!isLeadingInfix() || shouldOutdent(xs))
                  }) =>
                (Left(r), xs)
            }
          def shouldOutdent(rs: List[SepRegion]): Boolean =
            rs.find(_.isIndented).exists(_.indent > nextIndent)

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
            def getPrevIndent() = findIndent(sepRegions)
            def exceedsIndent = nextIndent > getPrevIndent()
            def emitIndentWith(ri: SepRegionIndented, rs: List[SepRegion], next: TokenRef = null) =
              Some(Right(mkIndent(indentPos, ri :: rs, next)))
            def emitIndent(regions: List[SepRegion], next: TokenRef = null) =
              emitIndentWith(RegionIndent(nextIndent), regions, next)
            def emitIndentAndOutdent(regions: List[SepRegion]) =
              emitIndent(regions, mkOutdentAt(nextIndent, nextPos, regions))
            // !next.is[RightBrace] - braces can sometimes have -1 and we can start indent on }
            prev match {
              case _ if nextIndent < 0 || next.is[RightBrace] => None
              // if does not work with indentation in pattern matches
              case _: KwIf if !sepRegions.headOption.exists(_.isInstanceOf[RegionIf]) => None
              case _: KwCatch | _: KwMatch =>
                // always add indent for indented `match` block
                // check the previous token to avoid infinity loop
                val ok = next.is[KwCase] && sepRegions.headOption.contains(RegionCaseMark)
                if (ok) emitIndent(sepRegions) else None
              case _: Colon =>
                /**
                 * Colon with NL can appear in several contexts:
                 *   - after package/class/etc: handled with RegionColonEol
                 *   - after variable and before its type:
                 *     - within `def`, `val`, `var` declaration or definition: excluded with
                 *       RegionDefMark
                 *     - cast within an argument expression: doesn't allow newline as it can then be
                 *       confused with a fewer-braces invocation
                 *     - within self-type: not allowed by the compiler
                 *   - after fewer-braces method call: will apply if not handled otherwise
                 */
                def couldBeFewerBraces(): Boolean =
                  dialect.allowFewerBraces && !sepRegions.contains(RegionDefMark) &&
                    getPrevToken(prevPos).isAny[Ident, CloseDelim]
                sepRegions match {
                  case (_: RegionTemplateDecl) :: rs =>
                    if (exceedsIndent) emitIndent(RegionTemplateBody :: rs)
                    else if (next.is[soft.KwEnd]) emitIndentAndOutdent(rs)
                    else Some(Right(lastWhitespaceToken(rs)))
                  case (_: RegionDefDecl) :: _ => None
                  case rs if !exceedsIndent =>
                    if (couldBeFewerBraces()) Some(Right(lastWhitespaceToken(rs)))
                    else None
                  case _ =>
                    next match {
                      // RefineDcl
                      case _: KwVal | _: KwDef | _: KwType | _: Semicolon => emitIndent(sepRegions)
                      // fewer braces partial function
                      case _: KwCase => emitIndent(RegionCaseMark :: sepRegions)
                      // fewer braces function (although could be self-type)
                      case _ if couldBeFewerBraces() => emitIndent(sepRegions)
                      case _ => None
                    }
                }
              case _ if !exceedsIndent => None
              case _: RightArrow =>
                sepRegions match {
                  case (rc: RegionCaseBody) :: (_: RegionBrace) :: _ if rc.arrow eq prev => None
                  case _ if isEndMarkerIntro(nextPos) => None
                  case _ => emitIndent(sepRegions)
                }
              case _: KwTry =>
                emitIndentWith(new RegionIndentTry(nextIndent), sepRegions)
              case _ =>
                sepRegions match {
                  case RegionForBraces :: xs if prev.is[RightBrace] =>
                    val ko = RegionForBraces.isTerminatingToken(next)
                    if (ko) None else emitIndent(xs)
                  case RegionForMaybeParens :: xs if prev.is[RightParen] =>
                    val ko = next.is[LeftArrow] || RegionForMaybeParens.isTerminatingToken(next)
                    if (ko) None else emitIndent(xs)
                  case (x: RegionControlMaybeCond) :: xs if prev.is[RightParen] =>
                    val ko = next.is[Dot] || !x.isNotTerminatingTokenIfOptional(next) ||
                      isLeadingInfix()
                    if (ko) None else emitIndent(x.asBody().fold(xs)(_ :: xs))
                  case (x: RegionControlMaybeCond) :: xs if x.isControlKeyword(prev) =>
                    emitIndent(x.asCond() :: xs)
                  case (_: RegionTemplateDecl) :: _ if next.is[LeftParen] =>
                    Some(Left(sepRegions)) // skip this newline
                  case RegionExtensionMark :: rs if prev.is[RightParen] && !next.is[LeftParen] =>
                    emitIndent(RegionTemplateBody :: rs)
                  case xs =>
                    val ok = prev match {
                      case _: KwYield | _: KwFinally | _: KwDo | _: KwFor | _: KwThen | _: KwElse |
                          _: KwWhile | _: KwIf | _: KwReturn | _: LeftArrow | _: ContextArrow =>
                        true
                      case _: Equals => !next.is[KwMacro]
                      case _: KwWith => isDefIntro(nextPos) || next.isAny[KwImport, KwExport]
                      case _ => false
                    }
                    if (ok) emitIndent(xs) else None
                }
            }
          }

          @tailrec
          def iter(regions: List[SepRegion]): Option[Either[List[SepRegion], TokenRef]] = {
            val res =
              if (prevToken.is[Indentation]) None
              else getOutdentIfNeeded(regions).fold(getIndentIfNeeded, x => Some(Right(x)))
            if (res.isEmpty) regions match {
              case (r: RegionControl) :: rs
                  if !r.isControlKeyword(prev) &&
                    r.isNotTerminatingTokenIfOptional(next) =>
                r match {
                  case rc: RegionControlMaybeCond if prev.is[RightParen] =>
                    if (next.is[Dot]) None
                    else
                      rc.asBody() match {
                        case Some(body) => getIfCanProduceLF(body :: rs)
                        case None => iter(rs)
                      }
                  case _ => iter(rs)
                }
              case rs => getIfCanProduceLF(rs)
            }
            else res
          }

          iter(sepRegionsOrig)
        }
      resOpt match {
        case Some(Right(res)) => res
        case Some(Left(rs)) => nextToken(prevToken, prevPos, nextPos, rs)
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

  private def findIndent(sepRegions: List[SepRegion]): Int =
    sepRegions.find(_.indent >= 0).fold(0)(_.indent)

}
