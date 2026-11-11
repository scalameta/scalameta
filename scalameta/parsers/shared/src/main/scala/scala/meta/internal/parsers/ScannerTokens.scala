package scala.meta.internal.parsers

import scala.meta._
import scala.meta.classifiers._
import scala.meta.inputs.Input
import scala.meta.internal.NullableOps._
import scala.meta.internal.trees._
import scala.meta.prettyprinters._
import scala.meta.tokenizers._
import scala.meta.tokens.Token._
import scala.meta.tokens._

import scala.annotation.tailrec

private[meta] final class ScannerTokens(
    val input: Input,
    val tokens: Tokens,
    val treeType: Class[_ <: Tree],
)(implicit val dialect: Dialect) {

  import ScannerTokens._

  // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
  // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
  // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
  // This leads to extremely dirty and seriously crazy code.
  implicit class XtensionTokenClass(token: Token) {

    def isClassOrObject = token.isAny[KwClass, KwObject]
    def isClassOrObjectOrEnum = isClassOrObject || token.is[Ident] && dialect.allowEnums

    def asString: String =
      s"[${token.getClass.getSimpleName}@${token.end}]${token.syntax.replace("\n", "")}"

  }

  // https://github.com/lampepfl/dotty/blob/4e7ab609/compiler/src/dotty/tools/dotc/parsing/Scanners.scala#L435
  def canBeLeadingInfixArg(argToken: Token, argTokenPos: Int): Boolean =
    isExprIntro(argToken, argTokenPos) &&
      (argToken match {
        case x: Ident => x.value.isUnaryOp || !x.isIdentSymbolicInfixOperator
        case _ => true
      })

  val soft = new SoftKeywords(dialect)

  object TypeIntro extends Function[Token, Boolean] {
    def apply(token: Token): Boolean = token match {
      case _: Ident | _: KwSuper | _: KwThis | _: LeftParen | _: At | _: Underscore | _: Unquote =>
        true
      case _: Literal => dialect.allowLiteralTypes
      case _ => false
    }

    def unapply(token: Token) = apply(token)
  }

  @inline
  private def isPrecededByNL(index: Int): Boolean = prevNonHTrivia(index).is[AtEOLorF]
  @inline
  private def isFollowedByNL(index: Int): Boolean = nextNonHTrivia(index).is[AtEOLorF]

  @tailrec
  final def isPrecededByDetachedComment(idx: Int, end: Int): Boolean = idx >= end &&
    (tokens(idx) match {
      case _: Comment => isPrecededByNL(idx)
      case _: Whitespace => isPrecededByDetachedComment(idx - 1, end)
      case _ => false
    })

  @inline
  private def isEndMarkerIdentifier(token: Token) = soft.KwEnd(token)

  private def isEndMarkerSpecifier(token: Token) = token match {
    case _: EndMarkerSpecifier => true
    case _ => false
  }

  def isEndMarkerIntro(token: Token, fNextIndex: => Int) = isEndMarkerIdentifier(token) && {
    val nextIndex = fNextIndex
    isEndMarkerSpecifier(tokens(nextIndex)) && isFollowedByNL(nextIndex)
  }

  def isEndMarkerIntro(index: Int): Boolean = isEndMarkerIntro(tokens(index), nextNonHTriviaAt(index))

  def isExprIntro(token: Token, fIndex: => Int): Boolean = isExprIntroImpl(token) {
    val index = fIndex
    !isSoftModifier(index) && !isEndMarkerIntro(index)
  }

  def isIdentOrExprIntro(token: Token): Boolean = isExprIntroImpl(token)(true)

  private def isExprIntroImpl(token: Token)(isIdentOK: => Boolean): Boolean = token match {
    case t: Ident => isIdentOK ||
      dialect.allowSpliceAndQuote && t.value.nonEmpty && t.value.charAt(0) == '$'
    case _: ExprBegToken | _: Unquote => true
    case _: LeftBracket => dialect.allowPolymorphicFunctions
    case _ => false
  }

  def isSoftModifier(index: Int): Boolean = {
    @inline
    def nextIsDclIntroOrModifierOr(f: Token => Boolean): Boolean = {
      val next = nextNonTriviaAt(index)
      isDclIntro(next) || isModifier(next) || f(tokens(next))
    }
    @inline
    def nextIsClassOrObjectOrOpaqueType: Boolean = {
      val next = nextNonTriviaAt(index)
      tokens(next) match {
        case _: KwClass | _: KwType | _: KwTrait | _: KwEnum => true
        case _: KwCase => nextNonTrivia(next).is[KwClass]
        case _ => isModifier(next)
      }
    }

    tokens(index).text match {
      case soft.KwTransparent() => nextIsDclIntroOrModifierOr(_.isAny[KwTrait, KwClass])
      case soft.KwOpaque() => nextIsDclIntroOrModifierOr(_ => false)
      case soft.KwInto() => nextIsClassOrObjectOrOpaqueType
      case soft.KwInline() => nextIsDclIntroOrModifierOr(matchesAfterInlineMatchMod)
      case soft.KwOpen() | soft.KwInfix() | soft.KwErased() | soft.KwTracked() =>
        isDefIntro(nextNonTriviaAt(index))
      case _ => false
    }
  }

  def matchesAfterInlineMatchMod(token: Token): Boolean = token match {
    case _: LeftParen | _: LeftBrace | _: KwNew | _: Ident | _: Literal | _: Interpolation.Id |
        _: Xml.Start | _: KwSuper | _: KwThis | _: MacroQuote => true
    case _ => false
  }

  @tailrec
  final def isDefIntro(index: Int): Boolean = tokens(index) match {
    case _: Unquote | _: Ellipsis => isDefIntro(nextNonTriviaAt(index))
    case _ => isDclIntro(index) || isModifier(index) || isTemplateIntro(index)
  }

  @tailrec
  final def isTemplateIntro(index: Int): Boolean = tokens(index) match {
    case _: At | _: TmplBegKeyword => true
    case _: Unquote => isTemplateIntro(nextNonTriviaAt(index))
    case _: KwCase => nextNonTrivia(index).isClassOrObjectOrEnum
    case _ => isModifier(index)
  }

  @tailrec
  final def isDclIntro(index: Int): Boolean = tokens(index) match {
    case _: DeclBegKeyword => true
    case _: Unquote => isDclIntro(nextNonTriviaAt(index))
    case _ => isKwExtension(index)
  }

  // Logic taken from the Scala 3 parser
  def isKwExtension(index: Int): Boolean = soft.KwExtension(tokens(index)) && nextNonTrivia(index)
    .isAny[LeftParen, LeftBracket]

  def isModifier(index: Int): Boolean = tokens(index).is[ModifierKeyword] || isSoftModifier(index)

  object ParamsModifier {
    def unapply(token: Token): Boolean = matches(token.text)
    def matches(text: String): Boolean = text match {
      case soft.KwInline() | soft.KwErased() | soft.KwTracked() => true
      case _ => false
    }
  }

  def isNonlocalModifier(token: Token): Boolean = token match {
    case _: KwPrivate | _: KwProtected | _: KwOverride | soft.KwOpen() => true
    case _ => false
  }

  object StatSeqEnd extends Function[Token, Boolean] {
    def apply(token: Token): Boolean = token match {
      case _: RightBrace | _: EOF | _: Indentation.Outdent => true
      case _ => false
    }
    def unapply(token: Token) = apply(token)
  }

  def statDelimOrSeqEnd(token: Token): Boolean = token.is[StatDelim] || StatSeqEnd(token)

  def mightStartStat(token: Token, closeDelimOK: Boolean = false): Boolean = token match {
    case _: ContKeyword | _: Comma | _: Dot | _: StatDelim | _: EOF => false
    case _: CloseDelim => closeDelimOK
    case _ => true
  }

  def canEndStat(token: Token): Boolean = token.isAny[StatEndToken, Ellipsis, Unquote]

  object Wildcard {
    def unapply(token: Token): Boolean = token.is[Underscore] || isStar(token)

    def isStar(token: Token): Boolean = dialect.allowStarWildcardImport && token.syntax == "*"
  }

  /**
   * When token on `tokenPosition` is not a whitespace and is a first non-whitespace character in a
   * current line then a result is a number of whitespace characters counted. Otherwise
   * {{{(-1, -1)}}} is returned.
   *
   * Returns the indentation level and the `LF` token index packed into a single `Long` (high 32
   * bits = indentation, low 32 bits = index), to avoid a per-token `Tuple2` allocation on this hot
   * path. Use [[Tokens.countOf]] / [[Tokens.indexOf]] to unpack.
   */
  def countIndentAndNewlineIndex(tokenPosition: Int): Long = {
    @tailrec
    def countIndentInternal(pos: Int, acc: Int = 0): Long =
      if (pos < 0) Tokens.pack(acc, pos)
      else {
        val token = tokens(pos)
        token match {
          case _: AtEOL | _: BOF => Tokens.pack(acc, pos)
          case c: Comment =>
            if (AsMultilineComment.isMultiline(c)) Tokens.pack(multilineCommentIndent(c), pos)
            else countIndentInternal(pos - 1)
          case t: HSpace => countIndentInternal(pos - 1, acc + t.len)
          case _ => Tokens.pack(-1, -1)
        }
      }

    if (tokenPosition < 0 || isWhitespace(tokens(tokenPosition))) Tokens.pack(-1, -1)
    else countIndentInternal(tokenPosition - 1)
  }

  def countIndent(tokenPosition: Int): Int = Tokens.countOf(countIndentAndNewlineIndex(tokenPosition))

  // the newlines at which an indentation region opened, and those at which one closed
  private val indentAt = new scala.collection.mutable.BitSet
  private val outdentAt = new scala.collection.mutable.BitSet
  def isIndentAt(idx: Int): Boolean = indentAt.contains(idx)
  def isOutdentAt(idx: Int): Boolean = outdentAt.contains(idx)

  // the last token, EOF, stops the search
  def nextAt(idx: Int, f: Token => Boolean): Int = {
    val last = tokens.length - 1
    if (idx < last) tokens.skipIf(f, idx + 1, last) else last
  }
  def nextNonHTrivia(idx: Int): Token = tokens.getOrNull(nextNonHTriviaAt(idx))
  def nextNonTrivia(idx: Int): Token = tokens.getOrNull(nextNonTriviaAt(idx))
  def nextNonWhitespace(idx: Int): Token = tokens.getOrNull(nextNonWhitespaceAt(idx))
  def nextNonHTriviaAt(idx: Int): Int = nextAt(idx, isHTrivia)
  def nextNonTriviaAt(idx: Int): Int = nextAt(idx, isTrivia)
  def nextNonWhitespaceAt(idx: Int): Int = nextAt(idx, isWhitespace)

  def prevAt(idx: Int, f: Token => Boolean): Int = tokens.rskipIf(f, idx - 1)
  def prevNonHTrivia(idx: Int): Token = tokens.getOrNull(prevNonHTriviaAt(idx))
  def prevNonTrivia(idx: Int): Token = tokens.getOrNull(prevNonTriviaAt(idx))
  def prevNonWhitespace(idx: Int): Token = tokens.getOrNull(prevNonWhitespaceAt(idx))
  def prevNonHTriviaAt(idx: Int): Int = prevAt(idx, isHTrivia)
  def prevNonTriviaAt(idx: Int): Int = prevAt(idx, isTrivia)
  def prevNonWhitespaceAt(idx: Int): Int = prevAt(idx, isWhitespace)

  // the last token of a statement before a newline
  private val statEolAfter = new scala.collection.mutable.BitSet(tokens.length)
  def isStatEolAfter(idx: Int): Boolean = statEolAfter.contains(idx)
  def unmarkStatEolAfter(idx: Int): Unit = statEolAfter -= idx

  def mkIndentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    indentAt += pointPos
    new Indentation.Indent(token.input, token.dialect, token.start, token.start)
  }

  def mkOutdentToken(pointPos: Int): Token = {
    val token = tokens(pointPos)
    outdentAt += pointPos
    new Indentation.Outdent(token.input, token.dialect, token.start, token.start)
  }

  def findOutdentPos(prevPos: Int, currPos: Int, outdent: Int, okBlank: Boolean): Int = {
    @tailrec
    def iter(i: Int, pos: Int, indent: Int, numEOL: Int = 0): Int =
      if (i >= currPos) if (pos < currPos) pos else currPos - 1
      else tokens(i) match {
        case t: AtEOL => iter(i + 1, i, 0, numEOL + t.newlines)
        case t: HSpace if indent >= 0 => iter(i + 1, pos, indent + t.len, numEOL)
        case _: Whitespace => iter(i + 1, pos, indent, numEOL)
        case _: Comment
            if indent < 0 || outdent < indent || outdent == indent && (okBlank || numEOL < 2) =>
          iter(i + 1, i + 1, -1)
        case _ => pos
      }

    val iterPos = 1 + prevPos
    if (iterPos < currPos) iter(iterPos, prevPos, if (tokens(prevPos).is[AtEOL]) 0 else -1)
    else if (tokens(currPos).is[EOF]) currPos
    else prevPos
  }

  @tailrec
  def isAheadNewLine(currentPosition: Int): Boolean = {
    val nextPos = currentPosition + 1
    nextPos < tokens.length && {
      val nextToken = tokens(nextPos)
      nextToken.is[AtEOL] || isTrivia(nextToken) && isAheadNewLine(nextPos)
    }
  }

  def nextToken(ref: TokenRef): TokenRef = ref.next match {
    case null =>
      val pos = ref.nextPos
      val next =
        if (pos < tokens.length) nextToken(ref.token, ref.pos, pos, ref.regions)
        else {
          val next = TokenRef(Nil, null, pos, pos, pos)
          next.next = next
          next
        }
      ref.next = next
      next
    case nref => nref
  }

  @tailrec
  private def nextToken(
      prevToken: Token,
      prevPos: Int,
      currPos: Int,
      sepRegionsOrig: List[SepRegion],
  ): TokenRef = {
    val prev = if (prevPos >= 0) tokens(prevPos) else null
    val curr = tokens(currPos)
    val currNonTrivial = !isTrivia(curr)
    val nextPos = tokens.indexWhere(!isTrivia(_), currPos + 1)
    val next = if (nextPos >= 0) tokens(nextPos) else null
    // Computed eagerly into an immutable `val` (packed into a Long) on purpose:
    // a `lazy val (nextIndent, indentPos)` tuple pattern is captured by the
    // nested newline/eof closures below, so its LazyRef/LazyInt holder(s) + the
    // Tuple2 would be allocated on *every* token advance (a hot path) even when
    // unused. An eager primitive `val` needs no holder and no boxing when
    // captured, trading those per-token allocations for a short backward scan.
    val packedIndent: Long = countIndentAndNewlineIndex(nextPos)
    def nextIndent: Int = Tokens.countOf(packedIndent)
    def indentPos: Int = Tokens.indexOf(packedIndent)

    // relax requirement that close delim is on separate line
    def isTrailingComma: Boolean = dialect.allowTrailingCommas && curr.is[Comma] &&
      next.is[CloseDelim] // && next.pos.startLine > curr.pos.endLine

    def mkIndent(pos: Int, pointPos: Int, rs: List[SepRegion], next: TokenRef = null): TokenRef =
      TokenRef(rs, mkIndentToken(pointPos), pos, nextPos, pointPos, next)

    def mkOutdentTo(region: SepRegionIndented, regions: List[SepRegion]) = region match {
      // end the lambda before a comment on a later line
      case _: RegionColonLambda => mkOutdentAt(Int.MaxValue, regions)
      case _ => mkOutdentAt(region.indent, regions)
    }

    def mkOutdentAt(outdent: Int, regions: List[SepRegion]) = {
      val maxPointPos = if (nextPos < 0 || currNonTrivial) currPos else nextPos
      val pointPos = findOutdentPos(prevPos, maxPointPos, outdent, isIndented(regions, outdent))
      val (nextPrevPos, nextCurrPos) =
        if (pointPos > currPos) (currPos, pointPos) else (prevPos, currPos)
      TokenRef(regions, mkOutdentToken(pointPos), nextPrevPos, nextCurrPos, pointPos)
    }

    @tailrec
    def mkOutdentsOpt(
        xs: List[SepRegion],
        wasDone: Boolean = false,
        head: TokenRef = null,
        last: TokenRef = null,
        multiEOL: Boolean,
    )(implicit f: List[SepRegion] => OutdentInfo): Either[List[SepRegion], (TokenRef, TokenRef)] =
      if (!wasDone) f(xs) match {
        case null => mkOutdentsOpt(xs, true, head, last, multiEOL)
        case OutdentInfo(outdent, rs, done) =>
          val isDone = done || (rs eq xs)
          if (outdent eq null) mkOutdentsOpt(rs, isDone, head, last, multiEOL)
          else {
            val tr = mkOutdentTo(outdent, rs)
            val newHead =
              if (head eq null) tr
              else {
                last.next = tr
                head
              }
            mkOutdentsOpt(rs, isDone, newHead, tr, multiEOL)
          }
      }
      else if (head eq null) Left(xs)
      else {
        if (currNonTrivial) last.next = currRef(xs)
        else if (multiEOL && mightStartStat(next) && canProduceLF(xs))
          last.next = eolRefAt(xs, last.pointPos, multiEOL = true)
        Right((head, last))
      }

    def mkOutdentsT[A](xs: List[SepRegion], multiEOL: Boolean = false)(lt: List[SepRegion] => A)(
        rt: (TokenRef, TokenRef) => A,
    )(implicit f: List[SepRegion] => OutdentInfo): A = mkOutdentsOpt(xs, multiEOL = multiEOL)
      .fold(lt, rt.tupled)

    def mkOutdents(xs: List[SepRegion], multiEOL: Boolean = false)(implicit
        f: List[SepRegion] => OutdentInfo,
    ): TokenRef = mkOutdentsT(xs, multiEOL)(currRef)((x, _) => x)

    def currRef(regions: List[SepRegion]): TokenRef = currAndNextRef(regions, null)
    def currAndNextRef(regions: List[SepRegion], next: TokenRef): TokenRef =
      TokenRef(regions, curr, currPos, next)

    def eolRefFor(rs: List[SepRegion], out: Token, eolPos: Int) =
      TokenRef(rs, out, eolPos, nextPos, eolPos, null)
    def eolRefAt(rs: List[SepRegion], eolPos: Int, multiEOL: Boolean): TokenRef = {
      val token = tokens(eolPos)
      statEolAfter += prevPos
      val out =
        if (!multiEOL || token.is[MultiEOL]) token
        else LFLF(token.input, token.dialect, token.start, tokens(indentPos).end)
      eolRefFor(rs, out, eolPos)
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
          // Scala 3 sub-cases: a `match` in a case guard yields a single sub-case that
          // must collapse at the enclosing level (no same-level body-line leniency),
          // unlike the catch/match-body single-liner; use a deeper indent for it. The
          // guard region may sit under `RegionIf`s from a chain of boolean `if` guards.
          @tailrec
          def getExtraIndent(rs: List[SepRegion]): Int = rs match {
            case (_: RegionCaseExprGuard) :: _ => 1
            case (_: RegionIf | RegionThen | _: RegionLine) :: tail => getExtraIndent(tail)
            case _ => 0
          }
          val indent = findIndent(sepRegions) + getExtraIndent(sepRegions)
          val nextRegions = RegionIndent(indent) :: regions
          currAndNextRef(regions, mkIndent(currPos, nextPos, nextRegions))
        case _: KwCase | _: LeftBrace => currRef(markRegions())
        case _ => currRef(sepRegions)
      }
    }

    def getTemplateInherit(sepRegions: List[SepRegion]): TokenRef = currRef(sepRegions match {
      case (_: RegionTemplateMark) :: rs => RegionTemplateInherit :: rs
      case _ => sepRegions
    })

    def isPrevEndMarker(): Boolean = prevPos > 0 && isEndMarkerIdentifier(prev) &&
      isPrecededByNL(prevPos)

    def getAtEof(sepRegions: List[SepRegion]) = mkOutdents(sepRegions) {
      case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
      case _ :: rs => OutdentInfo(null, rs)
      case _ => null
    }

    def afterControlRef(
        regions: List[SepRegion],
        body: List[SepRegion] => List[SepRegion] = identity,
    ) = {
      val dropped = dropControl(curr, regions)
      if (dropped.isEmpty) currRef(body(regions))
      // only line regions are nested in it: no outdent, and no closure to allocate
      else if (dropped eq dropRegionLine(regions)) currRef(body(dropped.tail))
      else mkOutdents(regions) {
        case (r: SepRegionIndented) :: xs => OutdentInfo(r, xs)
        case xs @ _ :: tail =>
          if (xs eq dropped) OutdentInfo(null, body(tail), done = true) else OutdentInfo(null, tail)
        case _ => null
      }
    }

    /** spec: whether the colon starts a lambda argument that ends on its line */
    def startsColonLambda(sepRegions: List[SepRegion]): Boolean = dialect.allowFewerBraces &&
      dialect.allowSignificantIndentation && isFewerBracesPrefix(prev) &&
      (dropRegionLine(sepRegions) match {
        // an ascription or a type parameter, where a type follows the colon
        case (_: RegionParen | _: RegionBracket) :: _ => false
        // a declaration, a template or a pattern, where a type follows the colon
        case (_: RegionHasParamMark | _: RegionTemplateDecl | _: RegionCaseExpr) :: _ => false
        case Nil => (treeType ne classOf[Term.Param]) && (treeType ne classOf[Type.Param])
        case _ => true
      }) && !precedesTypedName(prevNonTrivia(prevPos)) &&
      isColonLambdaSameLine(nextNonHTriviaAt(currPos)) && !isInTypeDecl(prevPos)

    def nonTrivial(sepRegions: List[SepRegion]) = curr match {
      case _: EOF => getAtEof(sepRegions)
      case _: Comma =>
        if (inParens(sepRegions)) {
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
          mkOutdentsT(sepRegions)(xs => swapWithOutdents(currRef(xs))) { (head, last) =>
            last.next = swapWithOutdents(last.next)
            head
          } {
            case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
            case (_: RegionNonDelimNonIndented) :: rs => OutdentInfo(null, rs)
            case _ => null
          }
        } else currRef(sepRegions)
      case _: KwEnum => currRef(RegionTemplateMark :: sepRegions)
      case _: KwGiven if !prevToken.isAny[Dot, KwCase] =>
        currRef(dropRegionLine(sepRegions) match {
          case rs @ (Nil | (_: RegionBrace | _: RegionIndent) :: _) => new RegionGivenDecl(curr) ::
              rs
          case _ => sepRegions
        })
      case _: KwWith => currRef(dropRegionLine(sepRegions) match {
          case (_: RegionGivenDecl) :: rs => rs
          case _ => sepRegions
        })
      case _: KwPackage if dialect.allowSignificantIndentation && !next.is[KwObject] =>
        currRef(RegionTemplateMark :: sepRegions)
      case _: KwObject | _: KwClass | _: KwTrait if dialect.allowSignificantIndentation =>
        currRef(RegionTemplateMark :: sepRegions)
      case _: KwNew if dialect.allowSignificantIndentation => currRef(RegionNewMark :: sepRegions)
      case _: KwTry if !isPrevEndMarker() => currRef(RegionTry :: dropRegionLine(sepRegions))
      case _: KwMatch if !isPrevEndMarker() => getCaseIntro(sepRegions)
      case _: KwCatch => getCaseIntro(dropWhile(sepRegions)(_ ne RegionTry))
      case _: KwCase if !next.isClassOrObject =>
        def expr() = new RegionCaseExprPat(countIndent(currPos))
        currRef {
          dropRegionLine(sepRegions) match {
            // `case` follows the body of a previous case
            case (_: RegionCaseBody) :: rs => expr() :: rs
            // head could be RegionIndent or RegionBrace
            case x :: RegionCaseMark :: rs => expr() :: x :: rs
            case (_: RegionDelim) :: (_: RegionFor | RegionTemplateBody) :: _ => sepRegions
            // partial function
            case (_: RegionBrace) :: _ => expr() :: sepRegions
            // partial function in assignment or fewer braces
            case (_: SepRegionIndented) :: rs
                if prevToken.is[Indentation.Indent] &&
                  (prev match {
                    case _: Equals | _: Colon => true
                    case _: RightArrow => rs.headOrNull.isInstanceOf[RegionCaseBody]
                    case _ => false
                  }) => expr() :: sepRegions
            // `case` is at top-level (likely quasiquote)
            case Nil if prevPos == 0 => expr() :: sepRegions
            case _ => sepRegions
          }
        }
      case _: KwFinally => dropRegionLine(sepRegions) match {
          // covers case when finally follows catch case without a newline
          // otherwise, these two regions would have been removed already
          case (_: RegionCaseBody) :: (r: RegionIndent) :: rs =>
            val ref = mkOutdentTo(r, rs)
            ref.next = currRef(dropUntil(rs)(_ eq RegionTry))
            ref
          case rs => currRef(dropUntil(rs)(_ eq RegionTry))
        }
      case _: LeftBrace =>
        val lbRegions = sepRegions match {
          case (r: RegionGivenDecl) :: rs if r.kw ne prevToken => RegionTemplateBody :: rs
          case (_: RegionTemplateMark) :: rs => RegionTemplateBody :: rs
          case RegionTemplateInherit :: rs if !prev.is[KwExtends] => RegionTemplateBody :: rs
          case _ => sepRegions
        }
        currRef(RegionBrace(nextIndent) :: lbRegions)
      case _: RightBrace =>
        // produce outdent for every indented region before RegionBrace|RegionEnum
        mkOutdents(sepRegions) {
          case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
          case (_: RegionBrace) :: (RegionTemplateBody | RegionCaseMark) :: rs =>
            OutdentInfo(null, rs, true)
          case (_: RegionBrace) :: rs => OutdentInfo(null, rs, true)
          case _ :: rs => OutdentInfo(null, rs)
          case _ => null
        }
      case _: LeftBracket => currRef(RegionBracket(nextIndent) :: sepRegions)
      case _: RightBracket => mkOutdents(sepRegions) {
          case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
          case (_: RegionBracket) :: rs => OutdentInfo(null, rs, done = true)
          case _ :: rs => OutdentInfo(null, rs)
          case _ => null
        }
      case _: LeftParen => currRef(RegionParen(nextIndent) :: sepRegions)
      case _: RightParen => mkOutdents(sepRegions) {
          case (r: SepRegionIndented) :: rs => OutdentInfo(r, rs)
          case (_: RegionParen) :: rs => OutdentInfo(null, rs, true)
          case _ :: rs => OutdentInfo(null, rs)
          case _ => null
        }
      case _: RightArrow => currRef(
          sepRegions match {
            case (_: RegionCaseExpr) :: rs =>
              // add case region for `match {` to calculate proper indentation
              // for statements in indentation dialects
              val bodyIndent = rs match {
                case (ri: SepRegionIndented) :: _ => ri.indent
                case _ => nextIndent
              }
              new RegionCaseBody(bodyIndent, curr) :: rs
            case RegionColonMaybeFewerBraces :: rs => rs
            case _ => sepRegions
          },
        )
      case _: KwFor if !isPrevEndMarker() => currRef(RegionFor(next) :: sepRegions)
      case _: KwWhile if dialect.allowQuietSyntax && !isPrevEndMarker() =>
        currRef(RegionWhile(next) :: sepRegions)
      case _: KwIf if dialect.allowQuietSyntax && !isPrevEndMarker() =>
        currRef(dropRegionLine(sepRegions) match {
          case (r: RegionCaseExprPat) :: rs => new RegionCaseExprGuard(r.indent) :: rs
          case rs @ (_: RegionFor) :: _ => rs
          case rs @ (_: RegionDelim) :: (_: RegionFor) :: _ => rs
          // a guard after a generator that left a region, such as that of `new` or `try`
          case rs if canEndStat(prev) && isForGuard(rs) => rs
          case _ => RegionIf(next) :: sepRegions
        })
      case _: KwThen => afterControlRef(sepRegions, RegionThen :: _)
      case _: KwElse if dialect.allowQuietSyntax =>
        dropRegionLine(sepRegions) match {
          case (_: RegionIf | RegionThen) :: rs => currRef(rs)
          case _ => currRef(sepRegions)
        }
      case _: KwDo | _: KwYield => afterControlRef(sepRegions)
      case _: KwDef | _: KwVal | _: KwVar
          if dialect.allowSignificantIndentation && !isPrevEndMarker() =>
        currRef(RegionDefMark :: sepRegions)
      case _: Colon => sepRegions match {
          case RegionDefMark :: rs => currRef(RegionDefType :: rs)
          case (_: RegionCaseExprGuard) :: _ if dialect.allowFewerBraces =>
            // to distinguish `=>` from lambda and end of case expr
            currRef(RegionColonMaybeFewerBraces :: sepRegions)
          case _ if startsColonLambda(sepRegions) =>
            val nextRegions = new RegionColonLambda(findIndent(sepRegions)) :: sepRegions
            currAndNextRef(sepRegions, mkIndent(currPos, nextPos, nextRegions))
          case _ => currRef(sepRegions)
        }
      case _: Equals => sepRegions match {
          case (_: RegionDefDecl | _: RegionGivenDecl) :: rs => currRef(rs)
          case _ => currRef(sepRegions)
        }
      case _: KwExtends => getTemplateInherit(sepRegions)
      case _: Ident => curr.text match {
          case soft.KwDerives() => getTemplateInherit(sepRegions)
          case soft.KwExtension() if (prevToken match {
                case _: BOF | _: Indentation | _: LeftBrace | _: RightArrow | _: StatDelim => next
                    .isAny[LeftParen, LeftBracket]
                case _ => false
              }) => currRef(RegionExtensionMark :: sepRegions)
          case _ => currRef(sepRegions)
        }
      case _ => currRef(sepRegions)
    }

    /** whether `curr` continues the class or the template of a `new` expression */
    def continuesNew: Boolean = curr match {
      case _: Ident | _: Unquote | _: KwThis | _: KwSuper => prev match {
          case _: KwNew | _: Dot | _: Hash | _: KwWith | _: At | _: Ellipsis => true
          case _ => false
        }
      case _: Dot => prev match {
          case _: Ident | _: Unquote | _: KwThis | _: KwSuper => true
          case _: RightBracket => // the qualifier of `super[T]` is one name
            val open = prevNonTriviaAt(prevNonTriviaAt(prevPos))
            tokens(open).is[LeftBracket] && prevNonTrivia(open).is[KwSuper]
          case _ => false
        }
      case _: Colon => isAheadNewLine(currPos) // a colon at the line end starts a template body
      // in a quasiquote, `..$a` stands for names, and `..@$a` for annotations
      case _: OpenDelim | _: Hash | _: KwWith | _: At | _: Ellipsis => true
      case _ => false
    }

    def getNonTrivialRegions(regions: List[SepRegion]) = dropRegionLine(regions) match {
      case RegionNewMark :: rs if !continuesNew => rs
      case RegionExtensionMark :: rs => curr match {
          case _: LeftBrace => RegionTemplateBody :: rs
          case _: LeftParen | _: LeftBracket => regions
          case _ => rs
        }
      case (r: RegionControl) :: rs if r.isNotTerminatingTokenIfOptional(curr) =>
        r match {
          case rc: RegionControlMaybeCond if prev.is[RightParen] =>
            val cond = rc.asCond()
            if (isCondContinued(cond, currPos)) cond :: rs else withBody(rc, rs)
          case RegionForMaybeParens if prev.is[RightParen] =>
            curr match {
              case _: LeftArrow => RegionForOther :: rs
              case _ => rs
            }
          case r: RegionFor if r.isClosingConditionToken(prev) => rs
          case _ => if (prevToken.is[AtEOL] || curr.is[CloseDelim]) rs else regions
        }
      case (r: RegionParen) :: RegionForMaybeParens :: rs if curr.is[LeftArrow] =>
        r :: RegionForParens :: rs
      case _ => regions
    }

    if (nextPos < 0) getAtEof(getNonTrivialRegions(sepRegionsOrig))
    else if (currNonTrivial)
      if (isTrailingComma) nextToken(curr, currPos, currPos + 1, sepRegionsOrig)
      else nonTrivial(getNonTrivialRegions(sepRegionsOrig))
    else {
      @tailrec
      def findFirstEOL(pos: Int): Int =
        if (pos > indentPos) -1 else if (tokens(pos).is[AtEOL]) pos else findFirstEOL(pos + 1)
      @tailrec
      def hasBlank(pos: Int, hadEOL: Boolean = false): Boolean =
        if (pos > indentPos) false
        else tokens(pos) match {
          case _: MultiNL => true
          case _: AtEOL => hadEOL || hasBlank(pos + 1, true)
          case _: Whitespace => hasBlank(pos + 1, hadEOL)
          case _ => hasBlank(pos + 1)
        }

      val hasLF = indentPos > prevPos // includes indentPos = -1
      val eolPos = if (hasLF) findFirstEOL(prevPos + 1) else -1
      val multiEOL = eolPos >= 0 && hasBlank(eolPos)

      def lastWhitespaceToken(rs: List[SepRegion], lineIndent: Int) = {
        val addRegionLine = lineIndent >= 0 && isIndented(rs, lineIndent)
        val regions = if (addRegionLine) RegionLine(lineIndent) :: rs else rs
        eolRefAt(regions, eolPos, multiEOL)
      }

      def stripIfCanProduceLF(regions: List[SepRegion]) = {
        @inline
        def derives(token: Token) = soft.KwDerives(token)
        @inline
        def blankBraceOr(ok: => Boolean): Boolean = if (next.is[LeftBrace]) multiEOL else ok
        def isEndMarker() = isEndMarkerSpecifier(prev) && {
          val prevPrevPos = prevNonHTriviaAt(prevPos)
          isEndMarkerIdentifier(tokens(prevPrevPos)) && isPrecededByNL(prevPrevPos)
        }
        @tailrec
        def strip(rs: List[SepRegion]): List[SepRegion] = rs match {
          // `[`, `=` and `#` are covered by CantStartStat
          // a definition without `=`, such as a procedure or a declaration, ends here
          case (r: RegionDefDecl) :: xs =>
            if ((r eq RegionDefMark) && next.is[LeftParen]) null else strip(xs)
          // `extends` and `with` are covered by canEndStat() and CantStartStat
          case (_: RegionTemplateMark) :: xs =>
            if (blankBraceOr(!derives(next))) strip(xs) else null
          case RegionTemplateInherit :: xs =>
            if (blankBraceOr(!derives(next) && !derives(prev))) strip(xs) else null
          case RegionTry :: xs
              if !next.isAny[KwCatch, KwFinally] && canBeLeadingInfix != LeadingInfix.Yes &&
                !isIndented(xs, nextIndent) => strip(xs)
          case (_: RegionGivenDecl) :: xs => strip(xs)
          case _ => if (canProduceLF(rs)) rs else null
        }
        val ok = eolPos >= 0 && mightStartStat(next, closeDelimOK = true) &&
          (canEndStat(prev) || isEndMarker())
        if (ok) strip(regions) else null
      }

      def getIfCanProduceLF(regions: List[SepRegion], lineIndent: Int = -1) = {
        val rs = stripIfCanProduceLF(regions)
        if (null eq rs) null else Right(lastWhitespaceToken(rs, lineIndent))
      }

      // https://dotty.epfl.ch/docs/reference/changed-features/operators.html#syntax-change-1
      def isLeadingInfix(regions: List[SepRegion]) = regions match {
        case Nil | (_: CanProduceLF) :: _ => canBeLeadingInfix
        case _ => LeadingInfix.No
      }

      lazy val canBeLeadingInfix =
        if (!multiEOL && eolPos >= 0 && dialect.allowInfixOperatorAfterNL &&
          next.isSymbolicInfixOperator) isLeadingInfixArg(nextPos + 1, nextIndent)
        else LeadingInfix.No

      def getInfixLFIfNeeded(regions: List[SepRegion]) = {
        def getInfixLF(invalid: Option[String]) = Right {
          val lf = tokens(indentPos)
          val out = InfixLF(lf.input, lf.dialect, lf.start, lf.end, invalid)
          eolRefFor(sepRegionsOrig, out, indentPos)
        }
        isLeadingInfix(regions) match {
          case LeadingInfix.Yes => getInfixLF(None)
          case LeadingInfix.InvalidArg if (sepRegionsOrig match {
                // see in `ieLeadingInfix` above, `x` is guaranteed to be CanProduceLF
                case x :: _ => x.indent >= 0 && x.indent < nextIndent
                case _ => false
              }) => getInfixLF(Some("Invalid indented leading infix operator found"))
          case _ => null
        }
      }

      val result =
        if (!hasLF) null
        else if (!dialect.allowSignificantIndentation) {
          val infixLF = getInfixLFIfNeeded(sepRegionsOrig)
          if (null ne infixLF) infixLF else getIfCanProduceLF(sepRegionsOrig)
        } else {
          def noOutdent(sepRegions: List[SepRegion]): Boolean = {
            val r = sepRegions.findOrNull(_.isIndented)
            (r eq null) || r.indent <= nextIndent
          }

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
          def getOutdentOnIndent(
              r: SepRegionIndented,
              rs: List[SepRegion],
              ctrlOnly: Boolean,
          ): OutdentInfo = dropRegionLine(nextIndent + 1, rs) match {
            case RegionTry :: xs =>
              if (nextIndent < r.indent || nextIndent == r.indent && next.isAny[KwCatch, KwFinally]) {
                val done = noOutdent(xs)
                OutdentInfo(r, if (done) rs else xs, done)
              } else null
            case _ if nextIndent >= r.indent => null // we stop here
            // a condition continues to its `then` or `do`
            case (_: RegionControlCond) :: _ => OutdentInfo(r, rs)
            case (rc: RegionControl) :: xs =>
              OutdentInfo(r, if (rc.isTerminatingToken(next)) rs else xs)
            case _ if ctrlOnly => null // we stop here
            case RegionTemplateBody :: xs => OutdentInfo(r, xs)
            case xs if (prev match {
                  // then  [else]  [do]  catch  [finally]  [yield]  match
                  case _: KwElse | _: KwDo | _: KwFinally | _: KwYield => false
                  // exclude leading infix op
                  case _ => !isIndented(xs, nextIndent) || canBeLeadingInfix != LeadingInfix.Yes
                }) => OutdentInfo(r, xs)
            case _ => null
          }

          /** dotc: whether a line end inside `region` keeps the lambda open */
          def holdsColonLambda(region: SepRegion): Boolean = region match {
            case _: RegionDelim | _: RegionCaseExpr | _: RegionControlCond | RegionForOther => true
            // keep the lambda open if the case arrow ends the line, as in `getOutdentInfo`
            case r: RegionCaseBody => r.arrow eq prev
            case _ => false
          }

          /** the regions from the innermost lambda that ends at this line end, or null */
          @tailrec
          def closingColonLambda(rs: List[SepRegion]): List[SepRegion] = rs match {
            case (_: RegionColonLambda) :: _ => rs
            case r :: xs if !holdsColonLambda(r) => closingColonLambda(xs)
            case _ => null
          }
          def getOutdentInfo(sepRegions: List[SepRegion]) = sepRegions match {
            case (rc: RegionCaseBody) :: (r: RegionIndent) :: rs =>
              if (nextIndent > r.indent) null
              else if (next.is[KwFinally]) OutdentInfo(r, rs, noOutdent(rs))
              else if (nextIndent < r.indent ||
                // only if outer is at the same indentation level
                findIndent(rs) == nextIndent &&
                // https://docs.scala-lang.org/scala3/reference/other-new-features/indentation.html#special-treatment-of-case-clauses
                (rc.arrow.ne(prev) && !next.is[KwCase] || nextNonTrivia(nextPos).isClassOrObject))
                OutdentInfo(r, dropRegionLine(nextIndent, rs))
              else getOutdentOnIndent(r, rs, ctrlOnly = true)
            case RegionTry :: rs => if (noOutdent(rs)) null else OutdentInfo(null, rs)
            case (_: RegionNonDelimNonIndented) :: rs if (prev match {
                  // [then]  else  do  [catch]  finally  yield  [match]
                  case _: KwThen | _: KwCatch | _: KwMatch => false
                  case _ => !noOutdent(rs)
                }) => OutdentInfo(null, rs)
            case (r: SepRegionIndented) :: rs => getOutdentOnIndent(r, rs, ctrlOnly = false)
            case _ => null
          }

          /**
           * Indent is needed in the following cases:
           *   - Indentation on new line is greater and previous token can start indentation and
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
          def getIndentIfNeeded(sepRegions: List[SepRegion]): Either[List[SepRegion], TokenRef] = {
            def exceedsIndent = isIndented(sepRegions, nextIndent)
            def emitIndentWith(ri: SepRegionIndented, rs: List[SepRegion], next: TokenRef) =
              Right(mkIndent(prevPos, indentPos, ri :: rs, next))
            def emitIndent(regions: List[SepRegion], next: TokenRef = null) =
              emitIndentWith(RegionIndent(nextIndent), regions, next)
            def emitIndentAndOutdent(regions: List[SepRegion]) =
              emitIndent(regions, mkOutdentAt(nextIndent, regions))
            // !next.is[RightBrace] - braces can sometimes have -1 and we can start indent on }
            prev match {
              case _ if nextIndent < 0 || next.is[RightBrace] => null
              // if does not work with indentation in pattern matches
              case _: KwIf if !sepRegions.headOrNull.isInstanceOf[RegionIf] => null
              case _: KwCatch | _: KwMatch =>
                // always add indent for indented `match` block
                // check the previous token to avoid infinity loop
                val ok = next.is[KwCase] && (sepRegions.headOrNull eq RegionCaseMark)
                if (ok) emitIndent(sepRegions) else null
              /*
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
              case _: Colon =>
                def isFewerBraces(indented: Boolean): Boolean = dialect.allowFewerBraces && {
                  val maybeRefineDcl = indented && next.isAny[KwVal, KwDef, KwType, Semicolon]
                  maybeRefineDcl ||
                  (sepRegions match {
                    case (_: RegionBracket | _: RegionHasParamMark) :: _ => false
                    case (_: RegionParen) :: (_: RegionHasParamMark) :: _ => false
                    case _ => true
                  }) && isFewerBracesPrefix(prevNonTrivia(prevPos))
                }
                sepRegions match {
                  case (_: RegionTemplateDecl) :: rs =>
                    if (exceedsIndent) emitIndent(RegionTemplateBody :: rs)
                    else if (soft.KwEnd(next)) emitIndentAndOutdent(rs)
                    else Right(lastWhitespaceToken(rs, nextIndent))
                  case (_: RegionDefDecl) :: _ => null
                  case _ if !exceedsIndent =>
                    if (!isFewerBraces(indented = false)) null
                    else Right(lastWhitespaceToken(sepRegions, nextIndent))
                  case (_: RegionGivenDecl) :: rs => emitIndent(RegionTemplateBody :: rs)
                  case RegionColonMaybeFewerBraces :: rs => emitIndent(rs)
                  case _ => if (isFewerBraces(indented = true)) emitIndent(sepRegions) else null
                }
              case _ if !exceedsIndent => null
              case _: RightArrow => dropRegionLine(sepRegions) match {
                  case (_: RegionGivenDecl) :: _ => null
                  case (rc: RegionCaseBody) :: (_: RegionBrace) :: _ if rc.arrow eq prev => null
                  case _ if isEndMarkerIntro(nextPos) => null
                  case _ => emitIndent(sepRegions)
                }
              case _: KwTry => emitIndent(sepRegions)
              case _ => dropRegionLine(sepRegions) match {
                  case (r: RegionFor) :: rs if r.isClosingConditionToken(prev) =>
                    val ko = r.isTerminatingToken(next)
                    if (ko) null else emitIndent(rs)
                  case RegionForMaybeParens :: rs if prev.is[RightParen] =>
                    val ko = next.is[LeftArrow] || RegionForMaybeParens.isTerminatingToken(next)
                    if (ko) null else emitIndent(rs)
                  case (x: RegionControlMaybeCond) :: rs if prev.is[RightParen] =>
                    val ko = next.is[Dot] || !x.isNotTerminatingTokenIfOptional(next) ||
                      canBeLeadingInfix == LeadingInfix.Yes
                    if (ko) null else emitIndent(withBody(x, rs))
                  case (x: RegionControlMaybeCond) :: rs if x.isControlKeyword(prev) =>
                    emitIndent(x.asCond() :: rs)
                  case (_: RegionTemplateDecl) :: _ if next.is[LeftParen] => Left(sepRegions) // skip this newline
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
                    if (ok) emitIndent(sepRegions) else null
                }
            }
          }

          /** the outdent of the lambda at the head of `lambda`, if any, or else of `sepRegions` */
          def getOutdentInfoWith(lambda: List[SepRegion], sepRegions: List[SepRegion]) =
            lambda match {
              case (r: RegionColonLambda) :: rs => OutdentInfo(r, rs)
              case _ => getOutdentInfo(sepRegions)
            }

          val regionsToRes: List[SepRegion] => Either[List[SepRegion], TokenRef] =
            if (prevToken.is[Indentation]) _ => null
            else rs => {
              val lambda = closingColonLambda(rs)
              // if an indent opens here, keep the lambda open
              val indent = if (lambda eq null) null else getIndentIfNeeded(rs)
              if (null ne indent) indent
              else mkOutdentsT(rs, multiEOL)(getIndentIfNeeded)((x, _) => Right(x))(xs =>
                getOutdentInfoWith(if (xs eq rs) lambda else closingColonLambda(xs), xs),
              )
            }

          def onlyWithoutLF() = sepRegionsOrig match {
            case (ri: SepRegionIndented) :: _ if ri.indent < nextIndent =>
              Left(RegionLine(nextIndent) :: sepRegionsOrig)
            case _ => null
          }

          @tailrec
          def iter(regions: List[SepRegion]): Either[List[SepRegion], TokenRef] = {
            val byRegions = regionsToRes(regions)
            val res = if (null ne byRegions) byRegions else getInfixLFIfNeeded(regions)
            if (null eq res) regions match {
              case Nil if prev.is[BOF] => Left(RegionLine(nextIndent) :: Nil)
              case (r: RegionLine) :: rs if r.indent >= nextIndent => iter(rs)
              case (r: RegionControl) :: rs
                  if !r.isControlKeyword(prev) && r.isNotTerminatingTokenIfOptional(next) =>
                r match {
                  case r: RegionControlMaybeCond if prev.is[RightParen] =>
                    if (next.is[Dot]) null
                    else r.asBody() match {
                      case x: RegionControl =>
                        val lf = getIfCanProduceLF(x :: rs, nextIndent)
                        if (null ne lf) lf else onlyWithoutLF()
                      case null => iter(rs)
                    }
                  case _ => iter(rs)
                }
              case rs =>
                val xs = stripIfCanProduceLF(rs)
                if (null eq xs) onlyWithoutLF()
                else if (xs eq rs) Right(lastWhitespaceToken(xs, nextIndent))
                else iter(xs)
            }
            else res
          }

          iter(sepRegionsOrig)
        }
      result match {
        case x: Right[_, TokenRef] => x.value
        case x: Left[List[SepRegion], _] => nextToken(prevToken, prevPos, nextPos, x.value)
        case null => nextToken(prevToken, prevPos, nextPos, sepRegionsOrig)
      }
    }
  }

  /**
   * Returns true if a line end, or an outdent in its place, can end a statement in `regions`
   * @param regions
   *   the regions that enclose the line end, the innermost first
   */
  @tailrec
  def canProduceLF(regions: List[SepRegion]): Boolean = regions match {
    // a condition continues past a line end (see dotty's `InCond`)
    case (_: RegionControlCond) :: _ => false
    // `try` and an inline `then` body defer to the enclosing region
    case (RegionTry | RegionThen) :: rs => canProduceLF(rs)
    case Nil | (_: CanProduceLF) :: _ => true
    case _ => false
  }

  /** Returns true if `cond` continues past `)` to the token at `pos`, as dotty's `toBeContinued` */
  private def isCondContinued(cond: RegionControlCond, pos: Int): Boolean = {
    val token = tokens(pos)
    // a token that cannot start a statement continues cond
    !mightStartStat(token) ||
    (token match {
      // might continue cond or start body
      case _: Ident | _: LeftBrace | _: LeftBracket | _: LeftParen | _: Underscore =>
        dialect.allowQuietSyntax && condEndAt(cond, pos) >= 0
      case _ => false
    })
  }

  /** Returns true if `regions`, those of a `)`, end the condition before the body at `pos` */
  private[parsers] def isAtControlCondEnd(regions: List[SepRegion], pos: Int): Boolean =
    regions match {
      case (rc: RegionControlMaybeCond) :: _ => !isCondContinued(rc.asCond(), pos)
      case _ => false
    }

  // the index of the delimiter that closes the one around `pos`, or -1
  private[parsers] def findCloseDelim(pos: Int): Int = {
    @tailrec
    def iter(i: Int, depth: Int): Int = tokens(i) match {
      case _: OpenDelim => iter(i + 1, depth + 1)
      case _: CloseDelim => if (depth == 0) i else iter(i + 1, depth - 1)
      case _: EOF => -1
      case _ => iter(i + 1, depth)
    }
    iter(pos, 0)
  }

  private def isFewerBracesPrefix(token: Token): Boolean = token.isAny[Ident, CloseDelim, KwThis]

  /** whether `name: T` after `token` types `name`: a self type, a parameter, a pattern */
  private def precedesTypedName(token: Token): Boolean = token
    .isAny[LeftBrace, Colon, KwImplicit, KwCase]

  /** whether the line to `pos` declares a type outside delimiters, so a colon is a context bound */
  private def isInTypeDecl(pos: Int): Boolean = {
    @tailrec
    def iter(i: Int, depth: Int): Boolean = i >= 0 &&
      (tokens(i) match {
        case _: CloseDelim => iter(i - 1, depth + 1)
        case _: OpenDelim => depth > 0 && iter(i - 1, depth - 1)
        case _ if depth > 0 => iter(i - 1, depth)
        case _: AtEOLorF | _: Semicolon => false
        case _: KwType => !prevNonTrivia(i).is[Dot]
        case _ => iter(i - 1, depth)
      })
    iter(pos, 0)
  }

  /** the index after the delimiter that closes the one at `pos`, or -1 */
  private def skipDelims(pos: Int): Int = {
    @tailrec
    def iter(i: Int, depth: Int): Int = tokens(i) match {
      case _: OpenDelim => iter(i + 1, depth + 1)
      case _: CloseDelim => if (depth == 1) i + 1 else iter(i + 1, depth - 1)
      case _: EOF => -1
      case _ => iter(i + 1, depth)
    }
    iter(pos, 0)
  }

  /** the index of the arrow after the parameters of a lambda at `pos`, or -1 */
  private def colonLambdaArrowAt(pos: Int): Int = {
    val end = tokens(pos) match {
      case _: Ident | _: Underscore => pos + 1
      case _: LeftParen | _: LeftBracket => skipDelims(pos)
      case _ => -1
    }
    if (end < 0) end
    else {
      val arrow = nextNonHTriviaAt(end - 1)
      if (tokens(arrow).is[FunctionArrow]) arrow else -1
    }
  }

  /** whether the arrow at `arrow` has its body, or a curried arrow, on its line */
  @tailrec
  private def hasBodySameLine(arrow: Int): Boolean = {
    val body = nextNonHTriviaAt(arrow)
    !tokens(body).is[AtEOLorF] && {
      val nextArrow = colonLambdaArrowAt(body)
      nextArrow < 0 || hasBodySameLine(nextArrow)
    }
  }

  /** whether `pos` starts a `case`, or a lambda that ends on its line */
  private def isColonLambdaSameLine(pos: Int): Boolean = tokens(pos).is[KwCase] || {
    val arrow = colonLambdaArrowAt(pos)
    arrow >= 0 && hasBodySameLine(arrow)
  }

  /* the index of the token that ends `cond` if the condition continues to it from `pos`, or -1;
   * as dotty's `followedByToken`, the scan stops at a statement outside braces */
  private def condEndAt(cond: RegionControlCond, pos: Int): Int = {
    @tailrec
    def iter(i: Int, braces: Int): Int = tokens(i) match {
      case _: Trivia => iter(i + 1, braces)
      case _: LeftBrace => iter(i + 1, braces + 1)
      case _: RightBrace => if (braces > 0) iter(i + 1, braces - 1) else -1
      case _: EOF | _: Xml.Start => -1
      case _ if braces > 0 => iter(i + 1, braces)
      case t if cond.isTerminatingToken(t) => i
      case _: StatBegKeyword | _: ModifierKeyword | _: CtrlKeyword | _: Semicolon => -1 // dotty's `stopScanTokens`
      case _ => iter(i + 1, braces)
    }
    iter(pos, 0)
  }

  private def isLeadingInfixArg(afterOpPos: Int, nextIndent: Int) = {
    // we don't check for pos to be within bounds since we would exit on EOF first
    @tailrec
    def iter(pos: Int, indent: Int, prevNoNL: Boolean): LeadingInfix = tokens(pos) match {
      case _: EOL => if (prevNoNL) iter(pos + 1, 0, false) else LeadingInfix.No
      case t: HSpace => iter(pos + 1, if (prevNoNL) indent else indent + t.len, prevNoNL)
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

private[meta] object ScannerTokens {

  private sealed trait LeadingInfix
  private object LeadingInfix {
    object No extends LeadingInfix
    object Yes extends LeadingInfix
    object InvalidArg extends LeadingInfix
  }

  def apply(input: Input, treeType: Class[_ <: Tree])(implicit
      dialect: Dialect,
      tokenize: Tokenize,
  ): ScannerTokens = new ScannerTokens(input, tokenize(input, dialect).get, treeType)

  case class OutdentInfo(outdent: SepRegionIndented, regions: List[SepRegion], done: Boolean = false)

  val isWhitespace: Token => Boolean = _.is[Whitespace]
  val isTrivia: Token => Boolean = _.is[Trivia]
  val isHTrivia: Token => Boolean = _.is[HTrivia]

  def multilineCommentIndent(t: Comment): Int = {
    val str: String = t.value
    @tailrec
    def loop(idx: Int, indent: Int): Int =
      if (idx <= 0) -1
      else str.charAt(idx) match {
        case '\n' | '\r' => indent
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

  // a control-region terminating token closes nested regions as well, like a closing delim
  private def dropControl(token: Token, regions: List[SepRegion]): List[SepRegion] = {
    @tailrec
    def iter(rs: List[SepRegion]): List[SepRegion] = rs match {
      case (r: RegionControl) :: _ if r.isTerminatingToken(token) => rs
      case Nil | (_: RegionDelimNonIndented | _: RegionFor) :: _ => Nil
      case _ :: xs => iter(xs)
    }
    iter(regions)
  }

  // an `if` after `regions` is a guard: a `for` region comes before any delimiter or indented one
  @tailrec
  private def isForGuard(regions: List[SepRegion]): Boolean = regions match {
    case (_: RegionFor) :: _ | (_: RegionDelim) :: (_: RegionFor) :: _ => true
    case Nil | (_: RegionDelim | _: SepRegionIndented) :: _ => false
    case _ :: rs => isForGuard(rs)
  }

  @tailrec
  private def dropRegionLine(regions: List[SepRegion]): List[SepRegion] = regions match {
    case (_: RegionLine) :: rs => dropRegionLine(rs)
    case _ => regions
  }

  @tailrec
  private def dropRegionLine(indent: Int, regions: List[SepRegion]): List[SepRegion] = regions match {
    case (r: RegionLine) :: rs if r.indent >= indent => dropRegionLine(indent, rs)
    case _ => regions
  }

  private def findIndent(sepRegions: List[SepRegion]): Int = sepRegions.findOrNull(_.indent >= 0)
    .nnFold(0)(_.indent)

  @inline
  private def isIndented(sepRegions: List[SepRegion], curIndent: Int): Boolean =
    findIndent(sepRegions) < curIndent

  private def withBody(rc: RegionControlMaybeCond, rs: List[SepRegion]): List[SepRegion] =
    rc.asBody() ?:: rs

  @tailrec
  private def inParens(regions: List[SepRegion]): Boolean = regions.nonEmpty &&
    (regions.head match {
      case r: RegionDelimNonIndented => r.isInstanceOf[RegionParen]
      case _ => inParens(regions.tail)
    })

}
