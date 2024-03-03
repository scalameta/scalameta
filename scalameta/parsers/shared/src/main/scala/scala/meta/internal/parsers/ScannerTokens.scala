package scala.meta.internal.parsers

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.inputs.Input
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
    if (index <= 0) 0
    else {
      val prev = index - 1
      if (tokens(prev).is[Trivia]) getPrevIndex(prev) else prev
    }
  }

  def getPrevToken(index: Int): Token = tokens(getPrevIndex(index))

  @tailrec
  final def getNextIndex(index: Int): Int = {
    if (index >= tokens.length - 1) tokens.length - 1
    else {
      val next = index + 1
      if (tokens(next).is[Trivia]) getNextIndex(next) else next
    }
  }

  def getNextToken(index: Int): Token = tokens(getNextIndex(index))

  @tailrec
  final def getStrictPrev(index: Int): Int = {
    if (index <= 0) 0
    else {
      val prev = index - 1
      if (tokens(prev).isAny[HSpace, Comment]) getStrictPrev(prev) else prev
    }
  }

  @tailrec
  final def getStrictNext(index: Int): Int = {
    if (index >= tokens.length - 1) tokens.length - 1
    else {
      val next = index + 1
      if (tokens(next).isAny[HSpace, Comment]) getStrictNext(next) else next
    }
  }

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

  val soft = new SoftKeywords(dialect)

  object TypeIntro {
    def unapply(token: Token): Boolean = token match {
      case _: Ident | _: KwSuper | _: KwThis | _: LeftParen | _: At | _: Underscore | _: Unquote =>
        true
      case _: Literal => dialect.allowLiteralTypes
      case _ => false
    }

    def apply(token: Token) = unapply(token)
  }

  @inline
  private def isPrecededByNL(index: Int): Boolean = tokens(getStrictPrev(index)).is[AtEOLorF]
  @inline
  private def isFollowedByNL(index: Int): Boolean = tokens(getStrictNext(index)).is[AtEOLorF]

  @inline
  private def isEndMarkerIdentifier(token: Token) = soft.KwEnd(token)

  private def isEndMarkerSpecifier(token: Token) = token match {
    case _: Ident | _: KwIf | _: KwWhile | _: KwFor | _: KwMatch | _: KwTry | _: KwNew | _: KwThis |
        _: KwGiven | _: KwVal =>
      true
    case _ => false
  }

  private def isEndMarkerIntro(index: Int, fNextIndex: => Int) =
    isEndMarkerIdentifier(tokens(index)) && {
      val nextIndex = fNextIndex
      isEndMarkerSpecifier(tokens(nextIndex)) && isFollowedByNL(nextIndex)
    }

  def isEndMarkerIntro(index: Int): Boolean = isEndMarkerIntro(index, getStrictNext(index))

  def isExprIntro(token: Token, fIndex: => Int): Boolean = isExprIntroImpl(token) {
    val index = fIndex
    !isSoftModifier(index) && !isEndMarkerIntro(index)
  }

  def isIdentOrExprIntro(token: Token): Boolean = isExprIntroImpl(token)(true)

  private def isExprIntroImpl(token: Token)(isIdentOK: => Boolean): Boolean = token match {
    case _: Ident => isIdentOK
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
    soft.KwInline(tokens(index)) && matchesAfterInlineMatchMod(getNextToken(index))

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
    soft.KwExtension(tokens(index)) && (getNextToken(index) match {
      case _: LeftParen | _: LeftBracket => true
      case _ => false
    })

  def isModifier(index: Int): Boolean =
    tokens(index).is[ModifierKeyword] || isSoftModifier(index)

  object NonParamsModifier {
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

  object StatSeqEnd {
    def unapply(token: Token): Boolean = token match {
      case _: RightBrace | _: EOF | _: Indentation.Outdent => true
      case _ => false
    }
    def apply(token: Token) = unapply(token)
  }

  object CantStartStat {
    def unapply(token: Token): Boolean = token match {
      case _: KwCatch | _: KwElse | _: KwExtends | _: KwFinally | _: KwForsome | _: KwMatch |
          _: KwWith | _: KwYield | _: RightParen | _: LeftBracket | _: RightBracket |
          _: RightBrace | _: Comma | _: Colon | _: Dot | _: Equals | _: Semicolon | _: Hash |
          _: RightArrow | _: LeftArrow | _: Subtype | _: Supertype | _: Viewbound | _: AtEOLorF =>
        true
      case _ => false
    }

    def apply(token: Token) = unapply(token)
  }

  private def canEndStat(token: Token): Boolean = token match {
    case _: Ident | _: KwGiven | _: Literal | _: Interpolation.End | _: Xml.End | _: KwReturn |
        _: KwThis | _: KwType | _: RightParen | _: RightBracket | _: RightBrace | _: Underscore |
        _: Ellipsis | _: Unquote =>
      true
    case _ => false
  }

  object StatSep {
    def apply(token: Token) = unapply(token)
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
          case c: Comment =>
            if (AsMultilineComment.isMultiline(c)) (multilineCommentIndent(c), pos)
            else countIndentInternal(pos - 1)
          case _: HSpace => countIndentInternal(pos - 1, acc + 1)
          case _ => (-1, -1)
        }
      }
    }

    if (tokenPosition < 0 || tokens(tokenPosition).is[Whitespace]) (-1, -1)
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
      nextToken.is[EOL] || nextToken.is[Trivia] && isAheadNewLine(nextPos)
    }
  }

  @inline
  private[parsers] def nextToken(ref: TokenRef): TokenRef = {
    val next = nextToken(ref.token, ref.pos, ref.nextPos, ref.regions)
    ref.next = next
    next
  }

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
    lazy val (nextIndent, indentPos) = countIndentAndNewlineIndex(nextPos)

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
        f: List[SepRegion] => OutdentInfo
    ): Either[List[SepRegion], TokenRef] = {
      def setLastRef(ref: TokenRef, xs: List[SepRegion]): Unit =
        if (currNonTrivial) ref.next = currRef(xs)
      @tailrec
      def mkOutdents(ref: TokenRef, xs: List[SepRegion]): Unit =
        f(xs) match {
          case null => setLastRef(ref, xs)
          case OutdentInfo(outdent, rs, done) =>
            val tr = if (outdent ne null) {
              ref.next = mkOutdentTo(outdent, maxPointPos, rs)
              ref.next
            } else ref
            if (done || (rs eq xs)) setLastRef(tr, rs) else mkOutdents(tr, rs)
        }
      f(regions) match {
        case null => Left(regions)
        case OutdentInfo(outdent, rs, done) =>
          if (outdent eq null) {
            if (done || (rs eq regions)) Left(rs)
            else mkOutdentsOpt(maxPointPos, rs)(f)
          } else {
            val res = mkOutdentTo(outdent, maxPointPos, rs)
            if (!done) mkOutdents(res, rs)
            Right(res)
          }
      }
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
        case _ => sepRegions
      })

    def isPrevEndMarker(): Boolean =
      prevPos > 0 && isEndMarkerIdentifier(prev) && isPrecededByNL(prevPos)

    def getAtEof(sepRegions: List[SepRegion]) =
      mkOutdentsOpt(currPos, sepRegions) {
        case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
        case _ :: rs => OutdentInfo(null, rs)
        case _ => null
      }.fold(currRef(_), identity)

    def nonTrivial(sepRegions: List[SepRegion]) = curr match {
      case _: EOF => getAtEof(sepRegions)
      case _: Comma =>
        if (inParens(sepRegions)) {
          @tailrec
          def findLastAndPrevRef(ref: TokenRef, prev: TokenRef): (TokenRef, TokenRef) = {
            val next = ref.next
            if (next eq null) (ref, prev) else findLastAndPrevRef(next, ref)
          }
          def swapWithOutdents(tokenRef: TokenRef) = {
            @tailrec
            def iter(oref: TokenRef): TokenRef = {
              val nref = nextToken(oref)
              if (nref.token.is[Indentation.Outdent]) iter(nref)
              else if (oref eq tokenRef) tokenRef
              else {
                oref.next = tokenRef
                try tokenRef.next
                finally tokenRef.next = nref
              }
            }
            iter(tokenRef)
          }
          mkOutdentsOpt(currPos, sepRegions) {
            case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
            case (_: RegionNonDelimNonIndented) :: rs => OutdentInfo(null, rs)
            case _ => null
          } match {
            case Right(tokenRef) =>
              val (last, prev) = findLastAndPrevRef(tokenRef, null)
              val cref = swapWithOutdents(last)
              if (prev eq null) cref
              else {
                if (cref ne last) prev.next = cref
                tokenRef
              }
            case Left(regions) => swapWithOutdents(currRef(regions))
          }
        } else currRef(sepRegions)
      case _: KwEnum => currRef(RegionTemplateMark :: sepRegions)
      case _: KwObject | _: KwClass | _: KwTrait | _: KwPackage | _: KwNew
          if dialect.allowSignificantIndentation =>
        currRef(RegionTemplateMark :: sepRegions)
      case _: KwTry =>
        currRef(RegionTry :: dropRegionLine(sepRegions))
      case _: KwMatch if !isPrevEndMarker() =>
        getCaseIntro(sepRegions)
      case _: KwCatch =>
        getCaseIntro(dropWhile(sepRegions)(_ ne RegionTry))
      case _: KwCase if !next.isClassOrObject =>
        def expr() = new RegionCaseExpr(countIndent(currPos))
        currRef(dropRegionLine(sepRegions) match {
          // `case` follows the body of a previous case
          case (_: RegionCaseBody) :: rs => expr() :: rs
          // head could be RegionIndent or RegionBrace
          case x :: RegionCaseMark :: rs => expr() :: x :: rs
          case (_: RegionDelim) :: (_: RegionFor | RegionTemplateBody) :: _ => sepRegions
          // partial function
          case (_: RegionBrace) :: _ => expr() :: sepRegions
          // partial function in assignment or fewer braces
          case (_: RegionIndent) :: _
              if prevToken.is[Indentation.Indent] && prev.isAny[Equals, Colon] =>
            expr() :: sepRegions
          // `case` is at top-level (likely quasiquote)
          case Nil if prevPos == 0 => expr() :: sepRegions
          case _ => sepRegions
        })
      case _: KwFinally =>
        dropRegionLine(sepRegions) match {
          // covers case when finally follows catch case without a newline
          // otherwise, these two regions would have been removed already
          case (_: RegionCaseBody) :: (r: RegionIndent) :: rs =>
            val ref = mkOutdentTo(r, currPos, rs)
            ref.next = currRef(dropUntil(rs)(_ eq RegionTry))
            ref
          case rs => currRef(dropUntil(rs)(_ eq RegionTry))
        }
      case _: LeftBrace =>
        val lbRegions = sepRegions match {
          case RegionTemplateMark :: rs => RegionTemplateBody :: rs
          case RegionTemplateInherit :: rs if !prev.is[KwExtends] => RegionTemplateBody :: rs
          case _ => sepRegions
        }
        currRef(RegionBrace(nextIndent) :: lbRegions)
      case _: RightBrace =>
        // produce outdent for every indented region before RegionBrace|RegionEnum
        mkOutdentsOpt(currPos, sepRegions) {
          case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
          case (_: RegionBrace) :: (RegionTemplateBody | RegionCaseMark) :: rs =>
            OutdentInfo(null, rs, true)
          case (_: RegionBrace) :: rs => OutdentInfo(null, rs, true)
          case _ :: rs => OutdentInfo(null, rs)
          case _ => null
        }.fold(currRef(_), identity)
      case _: LeftBracket => currRef(RegionBracket :: sepRegions)
      case _: RightBracket =>
        currRef(dropUntil(sepRegions)(_ eq RegionBracket))
      case _: LeftParen => currRef(RegionParen :: sepRegions)
      case _: RightParen =>
        mkOutdentsOpt(currPos, sepRegions) {
          case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
          case RegionParen :: rs => OutdentInfo(null, rs, true)
          case _ :: rs => OutdentInfo(null, rs)
          case _ => null
        }.fold(currRef(_), identity)
      case _: RightArrow =>
        currRef(sepRegions match {
          case (_: RegionCaseExpr) :: rs =>
            // add case region for `match {` to calculate proper indentation
            // for statements in indentation dialects
            val bodyIndent = rs match {
              case (ri: RegionIndent) :: _ => ri.indent
              case _ => nextIndent
            }
            new RegionCaseBody(bodyIndent, curr) :: rs
          case _ => sepRegions
        })
      case _: KwFor =>
        currRef(RegionFor(next) :: sepRegions)
      case _: KwWhile if dialect.allowSignificantIndentation =>
        currRef(RegionWhile(next) :: sepRegions)
      case _: KwIf if dialect.allowSignificantIndentation =>
        currRef(dropRegionLine(sepRegions) match {
          case rs @ (_: RegionCaseExpr | _: RegionFor) :: _ => rs
          case rs @ (_: RegionDelim) :: (_: RegionFor) :: _ => rs
          case _ => RegionIf(next) :: sepRegions
        })
      case _: KwThen =>
        dropRegionLine(sepRegions) match {
          case (r: RegionIndent) :: (_: RegionIf) :: rs =>
            outdentThenCurrRef(r, rs, Some(RegionThen))
          case (_: RegionIf) :: rs => currRef(RegionThen :: rs)
          case _ => currRef(RegionThen :: sepRegions)
        }
      case _: KwElse if dialect.allowSignificantIndentation =>
        dropRegionLine(sepRegions) match {
          case (r: RegionIndent) :: RegionThen :: rs =>
            outdentThenCurrRef(r, rs)
          case (_: RegionControl) :: rs => currRef(rs)
          case _ => currRef(sepRegions)
        }
      case _: KwDo | _: KwYield =>
        dropRegionLine(sepRegions) match {
          case (r: RegionIndent) :: (_: RegionControl) :: rs =>
            outdentThenCurrRef(r, rs)
          case (_: RegionControl) :: rs => currRef(rs)
          case _ => currRef(sepRegions)
        }
      case _: KwDef | _: KwVal | _: KwVar
          if dialect.allowSignificantIndentation && !isPrevEndMarker() =>
        currRef(RegionDefMark :: sepRegions)
      case _: Colon =>
        sepRegions match {
          case RegionDefMark :: rs => currRef(RegionDefType :: rs)
          case _ => currRef(sepRegions)
        }
      case _: Equals =>
        sepRegions match {
          case (_: RegionDefDecl) :: rs => currRef(rs)
          case _ => currRef(sepRegions)
        }
      case _: KwExtends => getTemplateInherit(sepRegions)
      case _: Ident =>
        curr.text match {
          case soft.KwDerives() => getTemplateInherit(sepRegions)
          case soft.KwExtension()
              if next.isAny[LeftParen, LeftBracket] &&
                (StatSep(prevToken) || prevToken.is[Indentation] || prevToken
                  .is[LeftBrace] || prevToken.is[RightArrow]) =>
            currRef(RegionExtensionMark :: sepRegions)
          case _ => currRef(sepRegions)
        }
      case _ => currRef(sepRegions)
    }

    def getNonTrivialRegions(regions: List[SepRegion]) = dropRegionLine(regions) match {
      case RegionExtensionMark :: rs =>
        curr match {
          case _: LeftBrace => RegionTemplateBody :: rs
          case _: LeftParen | _: LeftBracket => regions
          case _ => rs
        }
      case (r: RegionControl) :: rs if r.isNotTerminatingTokenIfOptional(curr) =>
        r match {
          case rc: RegionControlMaybeCond if prev.is[RightParen] =>
            curr match {
              case _: Dot | _: KwMatch => rc.asCond() :: rs
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
            if (prevToken.is[AtEOL] || curr.is[CloseDelim]) rs else regions
        }
      case _ => regions
    }

    if (nextPos < 0) getAtEof(getNonTrivialRegions(sepRegionsOrig))
    else if (currNonTrivial)
      if (isTrailingComma) nextToken(curr, currPos, currPos + 1, sepRegionsOrig)
      else nonTrivial(getNonTrivialRegions(sepRegionsOrig))
    else {
      @tailrec
      def findLastEOL(pos: Int): Int =
        if (pos == prevPos) -1 else if (tokens(pos).is[EOL]) pos else findLastEOL(pos - 1)
      @tailrec
      def hasBlank(pos: Int, hadEOL: Boolean): Boolean =
        if (pos == prevPos) false
        else
          tokens(pos) match {
            case _: EOL => hadEOL || hasBlank(pos - 1, true)
            case _: Whitespace => hasBlank(pos - 1, hadEOL)
            case _ => hasBlank(pos - 1, false)
          }

      val hasLF = indentPos > prevPos // includes indentPos = -1
      val lastNewlinePos = if (hasLF) findLastEOL(indentPos) else -1
      val newlines = lastNewlinePos >= 0 && hasBlank(lastNewlinePos - 1, true)

      def lastWhitespaceToken(rs: List[SepRegion], lineIndent: Int) = {
        val regions = if (lineIndent < 0) rs else RegionLine(lineIndent) :: rs
        val token = tokens(lastNewlinePos)
        val out =
          if (newlines) LFLF(token.input, token.dialect, token.start, token.end) else token
        TokenRef(regions, out, lastNewlinePos, null)
      }

      def getIfCanProduceLF(regions: List[SepRegion], lineIndent: Int = -1) = {
        @inline def derives(token: Token) = soft.KwDerives(token)
        @inline def blankBraceOr(ok: => Boolean): Boolean = if (next.is[LeftBrace]) newlines else ok
        def isEndMarker() = isEndMarkerSpecifier(prev) && {
          val prevPrevPos = getStrictPrev(prevPos)
          isEndMarkerIdentifier(tokens(prevPrevPos)) && isPrecededByNL(prevPrevPos)
        }
        @tailrec def strip(rs: List[SepRegion]): Option[List[SepRegion]] = rs match {
          // `[`, `=` and `#` are covered by CantStartStat
          case RegionDefType :: xs => if (next.is[LeftParen]) None else strip(xs)
          // `extends` and `with` are covered by canEndStat() and CantStartStat
          case RegionTemplateMark :: xs => if (blankBraceOr(!derives(next))) strip(xs) else None
          case RegionTemplateInherit :: xs =>
            if (blankBraceOr(!derives(next) && !derives(prev))) strip(xs) else None
          case RegionTry :: xs
              if !next.isAny[KwCatch, KwFinally] && isLeadingInfix != LeadingInfix.Yes =>
            strip(xs)
          case Nil | (_: CanProduceLF) :: _ => Some(rs)
          case _ => None
        }
        val ok = lastNewlinePos >= 0 && !CantStartStat(next) &&
          (prevToken.is[Indentation.Outdent] || canEndStat(prev) || isEndMarker())
        if (ok) strip(regions).map(rs => Right(lastWhitespaceToken(rs, lineIndent))) else None
      }

      // https://dotty.epfl.ch/docs/reference/changed-features/operators.html#syntax-change-1
      lazy val isLeadingInfix = sepRegionsOrig match {
        case Nil | (_: CanProduceLF) :: _
            if !newlines && lastNewlinePos >= 0 && dialect.allowInfixOperatorAfterNL &&
              next.is[Ident] && next.isIdentSymbolicInfixOperator =>
          isLeadingInfixArg(nextPos + 1, nextIndent)
        case _ => LeadingInfix.No
      }

      def getInfixLFIfNeeded() = {
        def getInfixLF(invalid: Option[String]) = Some(Right {
          val lf = tokens(lastNewlinePos)
          val out = InfixLF(lf.input, lf.dialect, lf.start, lf.end, invalid)
          TokenRef(sepRegionsOrig, out, lastNewlinePos, null)
        })
        isLeadingInfix match {
          case LeadingInfix.Yes => getInfixLF(None)
          case LeadingInfix.InvalidArg if (sepRegionsOrig match {
                // see in `ieLeadingInfix` above, `x` is guaranteed to be CanProduceLF
                case x :: _ => x.indent >= 0 && x.indent < nextIndent
                case _ => false
              }) =>
            getInfixLF(Some("Invalid indented leading infix operator found"))
          case _ => None
        }
      }

      val resOpt =
        if (!hasLF) None
        else if (!dialect.allowSignificantIndentation)
          getInfixLFIfNeeded().orElse(getIfCanProduceLF(sepRegionsOrig))
        else {
          def noOutdent(sepRegions: List[SepRegion]) =
            sepRegions.find(_.isIndented).forall(_.indent <= nextIndent)

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
              case (rc: RegionCaseBody) :: (r: RegionIndent) :: rs =>
                if (nextIndent > r.indent) null
                else if (next.is[KwFinally]) OutdentInfo(r, rs, noOutdent(rs))
                else if (nextIndent < r.indent || rc.arrow.ne(prev) &&
                  (!next.is[KwCase]) || getNextToken(nextPos).isClassOrObject)
                  OutdentInfo(r, rs)
                else null
              case (r: RegionIndent) :: (rs @ RegionTry :: xs) =>
                if (nextIndent < r.indent ||
                  nextIndent == r.indent && next.isAny[KwCatch, KwFinally]) {
                  val done = noOutdent(xs)
                  OutdentInfo(r, if (done) rs else xs, done)
                } else null
              case RegionTry :: rs =>
                if (noOutdent(rs)) null else OutdentInfo(null, rs)
              case (_: RegionNonDelimNonIndented) :: rs if (prev match {
                    // [then]  else  do  [catch]  finally  yield  [match]
                    case _: KwThen | _: KwCatch | _: KwMatch => false
                    case _ => !noOutdent(rs)
                  }) =>
                OutdentInfo(null, rs)
              case (r: SepRegionIndented) :: _ if nextIndent >= r.indent =>
                null // we stop here
              case (r: RegionIndent) :: (rs @ (rc: RegionControl) :: xs) =>
                OutdentInfo(r, if (rc.isNotTerminatingTokenIfOptional(next)) xs else rs)
              case (r: RegionIndent) :: RegionTemplateBody :: rs =>
                OutdentInfo(r, rs)
              case (r: SepRegionIndented) :: rs if (prev match {
                    // then  [else]  [do]  catch  [finally]  [yield]  match
                    case _: KwElse | _: KwDo | _: KwFinally | _: KwYield => false
                    // exclude leading infix op
                    case _ => findIndent(rs) >= nextIndent || isLeadingInfix != LeadingInfix.Yes
                  }) =>
                OutdentInfo(r, rs)
              case _ => null
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
                    else if (soft.KwEnd(next)) emitIndentAndOutdent(rs)
                    else Some(Right(lastWhitespaceToken(rs, nextIndent)))
                  case (_: RegionDefDecl) :: _ => None
                  case _ if !exceedsIndent =>
                    if (!couldBeFewerBraces()) None
                    else Some(Right(lastWhitespaceToken(sepRegions, nextIndent)))
                  case _ =>
                    next match {
                      // RefineDcl
                      case _: KwVal | _: KwDef | _: KwType | _: Semicolon => emitIndent(sepRegions)
                      // fewer braces function (although could be self-type)
                      case _ if couldBeFewerBraces() => emitIndent(sepRegions)
                      case _ => None
                    }
                }
              case _ if !exceedsIndent => None
              case _: RightArrow =>
                dropRegionLine(sepRegions) match {
                  case (rc: RegionCaseBody) :: (_: RegionBrace) :: _ if rc.arrow eq prev => None
                  case _ if isEndMarkerIntro(nextPos) => None
                  case _ => emitIndent(sepRegions)
                }
              case _: KwTry =>
                emitIndent(sepRegions)
              case _ =>
                dropRegionLine(sepRegions) match {
                  case RegionForBraces :: rs if prev.is[RightBrace] =>
                    val ko = RegionForBraces.isTerminatingToken(next)
                    if (ko) None else emitIndent(rs)
                  case RegionForMaybeParens :: rs if prev.is[RightParen] =>
                    val ko = next.is[LeftArrow] || RegionForMaybeParens.isTerminatingToken(next)
                    if (ko) None else emitIndent(rs)
                  case (x: RegionControlMaybeCond) :: rs if prev.is[RightParen] =>
                    val ko = next.is[Dot] || !x.isNotTerminatingTokenIfOptional(next) ||
                      isLeadingInfix == LeadingInfix.Yes
                    if (ko) None else emitIndent(x.asBody().fold(rs)(_ :: rs))
                  case (x: RegionControlMaybeCond) :: rs if x.isControlKeyword(prev) =>
                    emitIndent(x.asCond() :: rs)
                  case (_: RegionTemplateDecl) :: _ if next.is[LeftParen] =>
                    Some(Left(sepRegions)) // skip this newline
                  case RegionExtensionMark :: rs if prev.is[RightParen] && !next.is[LeftParen] =>
                    emitIndent(RegionTemplateBody :: rs)
                  case _ =>
                    val ok = prev match {
                      case _: KwYield | _: KwFinally | _: KwDo | _: KwFor | _: KwThen | _: KwElse |
                          _: KwWhile | _: KwIf | _: KwReturn | _: LeftArrow | _: ContextArrow =>
                        true
                      case _: Equals => !next.is[KwMacro]
                      case _: KwWith => isDefIntro(nextPos) || next.isAny[KwImport, KwExport]
                      case _ => false
                    }
                    if (ok) emitIndent(sepRegions) else None
                }
            }
          }

          val regionsToRes: List[SepRegion] => Option[Either[List[SepRegion], TokenRef]] =
            if (prevToken.is[Indentation]) _ => None
            else getOutdentIfNeeded(_).fold(getIndentIfNeeded, x => Some(Right(x)))

          @tailrec
          def iter(regions: List[SepRegion]): Option[Either[List[SepRegion], TokenRef]] = {
            val res = regionsToRes(regions).orElse {
              if (regions ne sepRegionsOrig) None else getInfixLFIfNeeded()
            }
            if (res.isEmpty) regions match {
              case Nil if prev.is[BOF] => Some(Left(RegionLine(nextIndent) :: Nil))
              case (r: RegionLine) :: rs if r.indent >= nextIndent => iter(rs)
              case (r: RegionControl) :: rs
                  if !r.isControlKeyword(prev) &&
                    r.isNotTerminatingTokenIfOptional(next) =>
                r match {
                  case rc: RegionControlMaybeCond if prev.is[RightParen] =>
                    if (next.is[Dot]) None
                    else
                      rc.asBody() match {
                        case Some(body) => getIfCanProduceLF(body :: rs, nextIndent)
                        case None => iter(rs)
                      }
                  case _ => iter(rs)
                }
              case rs => getIfCanProduceLF(rs, nextIndent)
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

  private def isLeadingInfixArg(afterOpPos: Int, nextIndent: Int) = {
    // we don't check for pos to be within bounds since we would exit on EOF first
    @tailrec def iter(pos: Int, indent: Int, prevNoNL: Boolean): LeadingInfix = tokens(pos) match {
      case _: EOL => if (prevNoNL) iter(pos + 1, 0, false) else LeadingInfix.No
      case _: HSpace => iter(pos + 1, if (prevNoNL) indent else indent + 1, prevNoNL)
      case c: Comment =>
        val commentIndent = multilineCommentIndent(c)
        iter(pos + 1, if (commentIndent < 0) indent else commentIndent, true)
      case t =>
        if (!canBeLeadingInfixArg(t, pos)) LeadingInfix.No
        else if (indent >= 0 && indent < nextIndent) LeadingInfix.InvalidArg
        else LeadingInfix.Yes
    }
    tokens(afterOpPos) match {
      case _: EOL => iter(afterOpPos + 1, 0, false)
      case _: HSpace => iter(afterOpPos + 1, -1, true)
      case _: Comment => LeadingInfix.InvalidArg
      case _ => LeadingInfix.No
    }
  }

}

object ScannerTokens {

  private sealed trait LeadingInfix
  private object LeadingInfix {
    object No extends LeadingInfix
    object Yes extends LeadingInfix
    object InvalidArg extends LeadingInfix
  }

  def apply(input: Input)(implicit dialect: Dialect): ScannerTokens = {
    new ScannerTokens(input.tokenize.get)
  }

  private[parsers] case class OutdentInfo(
      outdent: SepRegionIndented,
      regions: List[SepRegion],
      done: Boolean = false
  )

  private[parsers] def multilineCommentIndent(t: Comment): Int = {
    val str: String = t.value
    @tailrec
    def loop(idx: Int, indent: Int): Int =
      if (idx <= 0) -1
      else
        str.charAt(idx) match {
          case '\n' => indent
          case ' ' | '\t' => loop(idx - 1, indent + 1)
          case _ => loop(idx - 1, 0)
        }
    loop(str.length - 1, 0)
  }

  @tailrec
  private def dropWhile(regions: List[SepRegion])(f: SepRegion => Boolean): List[SepRegion] =
    regions match {
      case head :: tail => if (f(head)) dropWhile(tail)(f) else regions
      case _ => Nil
    }

  @tailrec
  private def dropUntil(regions: List[SepRegion])(f: SepRegion => Boolean): List[SepRegion] =
    regions match {
      case head :: tail => if (f(head)) tail else dropUntil(tail)(f)
      case _ => Nil
    }

  @tailrec
  private def dropRegionLine(regions: List[SepRegion]): List[SepRegion] = regions match {
    case (_: RegionLine) :: rs => dropRegionLine(rs)
    case _ => regions
  }

  private def findIndent(sepRegions: List[SepRegion]): Int =
    sepRegions.find(_.indent >= 0).fold(0)(_.indent)

  @tailrec
  private def inParens(regions: List[SepRegion]): Boolean =
    regions.nonEmpty && (regions.head match {
      case r: RegionDelimNonIndented => r eq RegionParen
      case _ => inParens(regions.tail)
    })

}
