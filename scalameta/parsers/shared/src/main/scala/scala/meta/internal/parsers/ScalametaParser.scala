package scala.meta
package internal
package parsers

import scala.meta.Term.EndMarker
import scala.meta.Term.QuotedMacroExpr
import scala.meta.Term.SplicedMacroExpr
import scala.language.implicitConversions
import scala.compat.Platform.EOL
import scala.reflect.{ClassTag, classTag}
import scala.collection.mutable
import mutable.ListBuffer
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.meta.internal.parsers.Location._
import scala.meta.internal.parsers.Absolutize._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokens.Token._
import scala.meta.internal.tokens._
import scala.meta.internal.trees._
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.prettyprinters._
import scala.meta.classifiers._
import scala.meta.internal.classifiers._
import org.scalameta._
import org.scalameta.invariants._
import scala.meta.internal.tokenizers.Chars
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal

class ScalametaParser(input: Input)(implicit dialect: Dialect) { parser =>
  require(Set("", EOL).contains(dialect.toplevelSeparator))
  private val soft = new SoftKeywords(dialect)

  /* ------------- PARSER ENTRY POINTS -------------------------------------------- */

  /** Utility for tracking a context, like whether we are inside a pattern or a quote. */
  private trait NestedContextBase {
    private var nested = 0
    def within[T](body: => T): T = {
      nested += 1
      try {
        body
      } finally {
        nested -= 1
      }
    }
    def isInside() = nested > 0
  }

  private object QuoteSpliceContext extends NestedContextBase

  private object QuotedPatternContext extends NestedContextBase

  def parseRule[T <: Tree](rule: this.type => T): T = {
    parseRule(rule(this))
  }

  def parseRule[T <: Tree](rule: => T): T = {
    // NOTE: can't require in.tokenPos to be at -1, because TokIterator auto-rewinds when created
    // require(in.tokenPos == -1 && debug(in.tokenPos))
    accept[BOF]
    val start = in.prevTokenPos
    val t = rule
    // NOTE: can't have in.prevTokenPos here
    // because we need to subsume all the trailing trivia
    val end = in.tokenPos
    accept[EOF]
    atPos(start, end)(t)
  }

  // Entry points for Parse[T]
  def parseStat(): Stat = parseRule {
    if (dialect.allowUnquotes) quasiquoteStat() else entrypointStat()
  }

  def parseTerm(): Term = parseRule {
    if (dialect.allowUnquotes) quasiquoteExpr() else entrypointExpr()
  }

  def parseUnquoteTerm(): Term = parseRule(unquoteExpr())

  def parseTermParam(): Term.Param = parseRule {
    if (dialect.allowUnquotes) quasiquoteTermParam() else entrypointTermParam()
  }

  def parseType(): Type = parseRule {
    if (dialect.allowUnquotes) quasiquoteType() else entrypointType()
  }

  def parseTypeParam(): Type.Param = parseRule {
    if (dialect.allowUnquotes) quasiquoteTypeParam() else entrypointTypeParam()
  }

  def parsePat(): Pat = parseRule {
    if (dialect.allowUnquotes) quasiquotePattern() else entrypointPattern()
  }

  def parseUnquotePat(): Pat = parseRule(unquotePattern())

  def parseCase(): Case = parseRule {
    if (dialect.allowUnquotes) quasiquoteCase() else entrypointCase()
  }

  def parseCtor(): Ctor = parseRule {
    if (dialect.allowUnquotes) quasiquoteCtor() else entrypointCtor()
  }

  def parseInit(): Init = parseRule {
    if (dialect.allowUnquotes) quasiquoteInit() else entrypointInit()
  }

  def parseSelf(): Self = parseRule {
    if (dialect.allowUnquotes) quasiquoteSelf() else entrypointSelf()
  }

  def parseTemplate(): Template = parseRule {
    if (dialect.allowUnquotes) quasiquoteTemplate() else entrypointTemplate()
  }

  def parseMod(): Mod = parseRule {
    if (dialect.allowUnquotes) quasiquoteModifier() else entrypointModifier()
  }

  def parseEnumerator(): Enumerator = parseRule {
    if (dialect.allowUnquotes) quasiquoteEnumerator() else entrypointEnumerator()
  }

  def parseImporter(): Importer = parseRule {
    if (dialect.allowUnquotes) quasiquoteImporter() else entrypointImporter()
  }

  def parseImportee(): Importee = parseRule {
    if (dialect.allowUnquotes) quasiquoteImportee() else entrypointImportee()
  }

  def parseSource(): Source = parseRule {
    if (dialect.allowUnquotes) quasiquoteSource() else entrypointSource()
  }

  /* ------------- TOKEN STREAM HELPERS -------------------------------------------- */

  // NOTE: This is a cache that's necessary for reasonable performance of prev/next for tokens.
  // It maps character positions in input's content into indices in the scannerTokens vector.
  // One complication here is that there can be multiple tokens that sort of occupy a given position,
  // so the clients of this cache need to be wary of that!
  private val scannerTokenCache: Array[Int] = {
    val result = new Array[Int](input.chars.length)
    var i = 0
    while (i < scannerTokens.length) {
      val token = scannerTokens(i)
      var j = token.start
      while (j < token.end) {
        result(j) = i
        j += 1
      }
      i += 1
    }
    result
  }
  implicit class XtensionTokenIndex(token: Token) {
    def index: Int = {
      @tailrec
      def lurk(roughIndex: Int): Int = {
        require(roughIndex >= 0 && debug(token))
        val scannerToken = scannerTokens(roughIndex)
        def exactMatch = scannerToken eq token
        def originMatch =
          (token.is[LFLF] || token.is[Indentation.Outdent] || token.is[Indentation.Indent]) &&
            scannerToken.start == token.start && scannerToken.end == token.end
        if (exactMatch || originMatch) roughIndex
        else lurk(roughIndex - 1)
      }
      if (token.start == input.chars.length) scannerTokens.length - 1
      else lurk(scannerTokenCache(token.start))
    }
    def prev: Token = {
      val prev = scannerTokens.apply(Math.max(token.index - 1, 0))
      if (prev.is[Whitespace] || prev.is[Comment]) prev.prev
      else prev
    }

    def nextSafe: Token = scannerTokens.apply(Math.min(token.index + 1, scannerTokens.length - 1))

    def next: Token = {
      val next = nextSafe
      if (next.is[Whitespace] || next.is[Comment]) next.next
      else next
    }
    def strictNext: Token = {
      val next = nextSafe
      if (next.is[Space] || next.is[Tab] || next.is[Comment]) next.strictNext
      else next
    }
  }

  def isMultilinePos(p: Position): Boolean = p.endLine > p.startLine

  def isColonEol(token: Token): Boolean = {

    @tailrec
    def isNextEOL(t: Token): Boolean = {
      val next = t.nextSafe
      if (next.is[LineEnd] || next.is[MultilineComment]) true
      else if ((next.is[Whitespace] && !next.is[LineEnd]) || next.is[Comment]) isNextEOL(next)
      else false
    }

    dialect.allowSignificantIndentation && token.is[Colon] && isNextEOL(token)
  }

  /* ------------- PARSER-SPECIFIC TOKENS -------------------------------------------- */

  sealed trait SepRegion {
    def indent = -1
    def closeOnNonCase = false
    def indentOnArrow = true
    def isIndented = false
  }
  case class RegionIndent(override val indent: Int, override val closeOnNonCase: Boolean)
      extends SepRegion {
    override def isIndented: Boolean = true
  }
  case class RegionParen(canProduceLF: Boolean) extends SepRegion
  case object RegionBracket extends SepRegion
  case class RegionBrace(override val indent: Int, override val indentOnArrow: Boolean)
      extends SepRegion
  case class RegionCase(override val indent: Int) extends SepRegion
  case class RegionEnum(override val indent: Int) extends SepRegion
  case class RegionIndentEnum(override val indent: Int) extends SepRegion {
    override def isIndented: Boolean = true
  }
  case object RegionArrow extends SepRegion
  // NOTE: Special case for Enum region is needed because parsing of 'case' statement is done differently
  case object RegionEnumArtificialMark extends SepRegion

  // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
  // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
  // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
  // This leads to extremely dirty and seriously crazy code.
  lazy val scannerTokens = input.tokenize match {
    case Tokenized.Success(tokens) => tokens
    case Tokenized.Error(_, _, details) => throw details
  }

  // NOTE: public methods of TokenIterator return scannerTokens-based positions
  trait TokenIterator extends Iterator[Token] {
    def prevTokenPos: Int
    def tokenPos: Int
    def currentIndentation: Int
    def token: Token
    def fork: TokenIterator
    def observeIndented(): Boolean
    def observeOutdented(): Boolean
    def observeIndentedEnum(): Boolean
    def undoIndent(): Unit
  }
  var in: TokenIterator = {
    new LazyTokenIterator(
      scannerTokens,
      List.empty,
      TokenRef(scannerTokens(0), 0, 1, 0),
      -1
    )
  }

  case class TokenRef(
      token: Token,
      pos: Int,
      nextPos: Int,
      pointPos: Int
  )

  private def mkIndentToken(token: Token): Token =
    new Indentation.Indent(token.input, token.dialect, token.start, token.end)

  private def mkOutdentToken(token: Token): Token =
    new Indentation.Outdent(token.input, token.dialect, token.start, token.end)

  class LazyTokenIterator(
      scannerTokens: Tokens,
      var sepRegions: List[SepRegion],
      var curr: TokenRef,
      var prevPos: Int
  ) extends TokenIterator {

    override def hasNext: Boolean = curr.token.isNot[EOF]

    override def next(): Token = {
      val (newSepRegions, newTokenRef) = nextToken(curr.pos, curr.nextPos, sepRegions)
      prevPos = curr.pointPos
      curr = newTokenRef
      sepRegions = newSepRegions
      curr.token
    }

    private def observeIndented0(f: (Int, List[SepRegion]) => List[SepRegion]): Boolean = {
      if (!dialect.allowSignificantIndentation) false
      else {
        val existingIndent = sepRegions.find(_.isIndented).fold(0)(_.indent)
        val (expected, pointPos) = countIndentAndNewlineIndex(tokenPos)
        if (expected > existingIndent) {
          sepRegions = f(expected, sepRegions)
          val indent = mkIndentToken(token)
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
        case region :: others if region.isIndented && curr.token.is[Indentation.Indent] =>
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
        val undoRegionChange =
          prev.headOption match {
            case Some(RegionParen(_)) if token.is[LeftParen] => prev.tail
            case Some(RegionEnumArtificialMark) if token.is[KwEnum] => prev.tail
            case Some(_: RegionBrace) if token.is[LeftBrace] => prev.tail
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

    def observeOutdented(): Boolean = {
      if (!dialect.allowSignificantIndentation) false
      else {

        def canEndIndentation(token: Token) = token.is[KwElse] || token.is[KwThen] ||
          token.is[KwDo] || token.is[KwCatch] || token.is[KwFinally] || token.is[KwYield] ||
          token.is[KwMatch]

        sepRegions match {
          case region :: tail if region.isIndented && canEndIndentation(curr.token) =>
            sepRegions = tail
            val outdent = mkOutdentToken(curr.token)
            val outdentPos = findOutdentPos(prevPos, curr.pos, region)
            curr = TokenRef(outdent, curr.pos, curr.pos, outdentPos)
            true
          case _ => false
        }
      }
    }

    private def findOutdentPos(prevPos: Int, currPos: Int, region: SepRegion): Int = {
      val outdent = region.indent
      @tailrec
      def iter(i: Int, pos: Int, indent: Int): Int = {
        if (i >= currPos) {
          if (pos < currPos) pos else currPos - 1
        } else {
          scannerTokens(i) match {
            case _: LF | _: FF =>
              iter(i + 1, if (pos == prevPos) i else pos, 0)
            case _: Space | _: Tab if indent >= 0 =>
              iter(i + 1, pos, indent + 1)
            case Whitespace() =>
              iter(i + 1, pos, indent)
            case _: Comment if indent < 0 || outdent <= indent =>
              iter(i + 1, i + 1, -1)
            case _ => pos
          }
        }
      }

      val iterPos = 1 + prevPos
      if (iterPos < currPos) iter(iterPos, prevPos, -1)
      else if (scannerTokens(currPos).is[EOF]) currPos
      else prevPos
    }

    @tailrec
    private def nextToken(
        prevPos: Int,
        currPos: Int,
        sepRegions: List[SepRegion]
    ): (List[SepRegion], TokenRef) = {
      val prev = if (prevPos >= 0) scannerTokens(prevPos) else null
      val curr = scannerTokens(currPos)
      val nextPos = {
        var i = currPos + 1
        while (i < scannerTokens.length && scannerTokens(i).is[Trivia]) i += 1
        if (i == scannerTokens.length) i = -1
        i
      }
      val next = if (nextPos != -1) scannerTokens(nextPos) else null

      def isTrailingComma: Boolean =
        dialect.allowTrailingCommas &&
          curr.is[Comma] &&
          next.is[CloseDelim] &&
          next.pos.startLine > curr.pos.endLine

      def mkIndent(pointPos: Int): TokenRef =
        TokenRef(mkIndentToken(curr), prevPos, currPos, pointPos)

      def mkOutdent(region: SepRegion): TokenRef =
        mkOutdentTo(region, currPos)

      def mkOutdentTo(region: SepRegion, maxPointPos: Int): TokenRef = {
        val pointPos = findOutdentPos(prevPos, maxPointPos, region)
        TokenRef(mkOutdentToken(curr), prevPos, currPos, pointPos)
      }

      def currRef: TokenRef = TokenRef(curr, currPos, currPos + 1, currPos)

      def indentationWithinParenRegion: Option[SepRegion] = {
        def isWithinParenRegion =
          sepRegions.tail
            .collectFirst {
              case RegionParen(_) => true
              case other if !other.isIndented => false
            }
            .contains(true)
        sepRegions.headOption.filter(_.isIndented && isWithinParenRegion)
      }

      if (isTrailingComma) {
        nextToken(currPos, currPos + 1, sepRegions)
      } else if (curr.isNot[Trivia]) {
        if (curr.is[LeftParen]) (RegionParen(false) :: sepRegions, currRef)
        else if (curr.is[LeftBracket]) (RegionBracket :: sepRegions, currRef)
        else if (curr.is[Comma]) {
          indentationWithinParenRegion.fold {
            (sepRegions, currRef)
          } { region =>
            (sepRegions.tail, mkOutdent(region))
          }
        } else if (curr.is[LeftBrace]) {
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
        } else if (curr.is[KwEnum]) {
          (RegionEnumArtificialMark :: sepRegions, currRef)
        } else if (curr.is[CaseIntro]) {
          val nextRegions = sepRegions.headOption match {
            case Some(_: RegionEnum | _: RegionIndentEnum) => sepRegions
            case Some(_: RegionCase) => RegionArrow :: sepRegions.tail
            case _ => RegionArrow :: sepRegions
          }
          (nextRegions, currRef)
        } else if (curr.is[RightBrace]) {
          // produce outdent for every indented region before RegionBrace|RegionEnum
          @tailrec
          def nextRegions(in: List[SepRegion]): (List[SepRegion], TokenRef) = {
            in match {
              case (_: RegionBrace | _: RegionEnum) :: xs =>
                (xs, currRef)
              case x :: xs if x.isIndented =>
                (xs, mkOutdent(x))
              case _ :: xs =>
                nextRegions(xs)
              case Nil =>
                (Nil, currRef)
            }
          }
          nextRegions(sepRegions)
        } else if (curr.is[RightBracket]) {
          val nextRegions =
            if (sepRegions.headOption.contains(RegionBracket)) sepRegions.tail
            else sepRegions
          (nextRegions, currRef)
        } else if (curr.is[EOF]) {
          sepRegions match {
            case x :: xs if x.isIndented => (xs, mkOutdent(x))
            case other => (other, currRef)
          }
        } else if (curr.is[RightParen]) {
          sepRegions match {
            case x :: xs if x.isIndented => (xs, mkOutdent(x))
            case RegionParen(_) :: xs => (xs, currRef)
            case _ => (sepRegions, currRef)
          }
        } else if (curr.is[LeftArrow]) {
          val nextRegions =
            if (sepRegions.headOption.contains(RegionArrow)) sepRegions.tail
            else sepRegions
          (nextRegions, currRef)
        } else if (curr.is[RightArrow]) {
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
        } else if (dialect.allowSignificantIndentation && curr.is[KwFor]) {
          val updatedSepRegions = sepRegions match {
            case RegionParen(_) :: tail => RegionParen(true) :: tail
            case _ => sepRegions
          }
          (updatedSepRegions, currRef)
        } else {
          (sepRegions, currRef)
        }
      } else {
        var i = prevPos + 1
        var lastNewlinePos = -1
        var newlineStreak = false
        var newlines = false
        var hasMultilineComment = false
        while (i < nextPos) {
          val token = scannerTokens(i)
          if (token.is[LF] || token.is[FF]) {
            lastNewlinePos = i
            if (newlineStreak) newlines = true
            newlineStreak = true
          }
          hasMultilineComment |= token.is[MultilineComment]
          newlineStreak &= token.is[Whitespace]
          i += 1
        }

        def lastWhitespaceToken = {
          val token = scannerTokens(lastNewlinePos)
          val out =
            if (newlines) LFLF(token.input, token.dialect, token.start, token.end) else token
          TokenRef(out, lastNewlinePos, lastNewlinePos + 1, lastNewlinePos)
        }

        def canProduceLF: Boolean = {
          lastNewlinePos != -1 &&
          prev != null && (prev.is[CanEndStat] || token.is[Indentation.Outdent]) &&
          next != null && next.isNot[CantStartStat] && sepRegions.headOption.forall {
            case _: RegionBrace | _: RegionCase | _: RegionEnum => true
            case _: RegionIndent | _: RegionIndentEnum => true
            case x: RegionParen => x.canProduceLF
            case _ => false
          }
        }

        def getIfCanProduceLF =
          if (canProduceLF) Some((sepRegions, lastWhitespaceToken))
          else None

        val resOpt = if (dialect.allowSignificantIndentation) {
          val hasLF = lastNewlinePos != -1 || hasMultilineComment
          if (hasLF && next != null && !isLeadingInfixOperator(next)) {

            val nextIndent = countIndent(nextPos)

            /**
             * Outdent is needed in following cases:
             *   - If indentation on next line is less than current and previous token can't
             *     continue expr on the next line
             *   - At the end of `match` block even if indentation level is not changed Example:
             *     ```
             *     x match
             *     case 1 =>
             *     case 2 => // <- produce outdent
             *     foo()
             *     ```
             */
            def getOutdentIfNeeded() = sepRegions.headOption
              .filter { r =>
                r.isIndented && {
                  // need to check prev.prev in case of `end match`
                  if (nextIndent < r.indent)
                    prev.isNot[CanContinueOnNextLine] || prev.prev.is[soft.KwEnd]
                  else r.closeOnNonCase && next.isNot[KwCase] && nextIndent == r.indent
                }
              }
              .map { region => (sepRegions.tail, mkOutdentTo(region, nextPos)) }

            /**
             * Indent is needed in the following cases:
             *   - Indetation on new line is greater and previous token can start indentation and
             *     token can start indentation
             *   - Indentation on the new line is the same and the next token is the first `case`
             *     clause in match Example:
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
                  indentOnArrow && next.isNot[RightBrace] && next.isNot[EndMarkerIntro]
                } else if (nextIndent > currIndent) {
                  // if does not work with indentation in pattern matches
                  val shouldNotIndentIf =
                    prev.is[KwIf] && sepRegions.headOption.contains(RegionArrow)
                  !shouldNotIndentIf && prev.is[CanStartIndent] && !next.is[RightBrace]
                } else
                  // always add indent for indented `match` block
                  // check the previous token to avoid infinity loop
                  (!prev.prev.is[soft.KwEnd] && (prev.is[KwMatch] || prev.is[KwCatch])) &&
                  next.is[KwCase] && token.isNot[Indentation.Indent]
              }
              if (ok) Some {
                val region = RegionIndent(nextIndent, prev.is[KwMatch])
                (region :: sepRegions, mkIndent(lastNewlinePos))
              }
              else None
            }

            getOutdentIfNeeded()
              .orElse { getIndentIfNeeded }
              .orElse { getIfCanProduceLF }
          } else None
        } else {
          getIfCanProduceLF
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

  }

  private def multineCommentIndent(t: Comment): Int = {
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
    def isWhitespace(token: Token): Boolean = token.is[Space] || token.is[Tab]

    @tailrec
    def countIndentInternal(pos: Int, acc: Int = 0): (Int, Int) = {
      if (pos < 0) (acc, pos)
      else {
        val token = scannerTokens(pos)
        token match {
          case _: LF => (acc, pos)
          case c: Comment if isMultilinePos(c.pos) =>
            (multineCommentIndent(c), pos)
          case _: Comment => countIndentInternal(pos - 1)
          case other if isWhitespace(other) => countIndentInternal(pos - 1, acc + 1)
          case _ => (-1, -1)
        }
      }
    }

    if (scannerTokens(tokenPosition).is[Whitespace]) (-1, -1)
    else countIndentInternal(tokenPosition - 1)
  }

  private def countIndent(tokenPosition: Int): Int =
    countIndentAndNewlineIndex(tokenPosition)._1

  private def currentIndentation: Int = {
    in.currentIndentation
  }

  @tailrec
  private def isAheadNewLine(currentPosition: Int): Boolean = {
    val nextPos = currentPosition + 1
    if (nextPos >= scannerTokens.length) false
    else if (scannerTokens(nextPos).is[LF]) true
    else scannerTokens(nextPos).is[Trivia] && isAheadNewLine(nextPos)
  }

  private def isLeadingInfixOperator(tkn: Token): Boolean = {
    lazy val parts = tkn.text.split("_")
    dialect.allowSignificantIndentation &&
    parts.nonEmpty &&
    parts.last.forall(Chars.isOperatorPart) &&
    !tkn.text.startsWith("@") &&
    tkn.nextSafe.is[Whitespace] && (
      tkn.strictNext.is[Ident] || tkn.strictNext.is[Interpolation.Id] ||
        tkn.strictNext.is[Literal] || tkn.strictNext.is[LeftParen] ||
        tkn.strictNext.is[LeftBrace]
    )
  }
  def token = in.token
  def next() = in.next()
  def nextOnce() = next()
  def nextTwice() = { next(); next() }
  def nextThrice() = { next(); next(); next() }

  /* ------------- PARSER COMMON -------------------------------------------- */

  /**
   * Scoping operator used to temporarily look into the future. Backs up token iterator before
   * evaluating a block and restores it after.
   */
  @inline final def ahead[T](body: => T): T = {
    val forked = in.fork
    next()
    try body
    finally in = forked
  }

  /**
   * Methods inParensOrError and similar take a second argument which, should the next token not be
   * the expected opener (e.g. token.LeftParen) will be returned instead of the contents of the
   * groupers. However in all cases accept[LeftParen] will be called, so a parse error will still
   * result. If the grouping is optional, token should be tested before calling these methods.
   */
  @inline final def inParens[T](body: => T): T = {
    accept[LeftParen]
    val ret = body
    acceptOpt[LF]
    accept[RightParen]
    ret
  }
  @inline final def inParensOrError[T](body: => T, alt: T): T =
    if (token.is[LeftParen]) inParens(body)
    else {
      accept[LeftParen]; alt
    }

  @inline final def inParensOrUnit[T, Ret >: Lit](body: => Ret): Ret =
    inParensOrError(body, Lit.Unit())
  @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

  @inline final def inBraces[T](body: => T): T = {
    accept[LeftBrace]
    val ret = body
    acceptOpt[LF]
    accept[RightBrace]
    ret
  }
  @inline final def inBracesOrError[T](body: => T, alt: T): T = {
    if (token.is[LF]) { accept[LF] }
    if (token.is[LeftBrace]) inBraces(body)
    else {
      accept[LeftBrace]; alt
    }
  }

  @inline final def indented[T](body: => T): T = {
    accept[Indentation.Indent]
    val ret = body
    newLinesOpt()
    if (token.is[Indentation.Outdent])
      next()
    else {
      in.observeOutdented()
      accept[Indentation.Outdent]
    }
    ret
  }

  @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
  @inline final def inBracesOrUnit[T](body: => Term): Term = inBracesOrError(body, Lit.Unit())
  @inline final def dropAnyBraces[T](body: => T): T =
    if (token.is[LeftBrace]) inBraces(body)
    else body

  def dropTrivialBlock(term: Term): Term =
    term match {
      case b: Term.Block => dropOuterBlock(b)
      case _ => term
    }

  private def dropOuterBlock(term: Term.Block): Term =
    term.stats match {
      case (stat: Term) :: Nil => stat
      case _ => term
    }

  @inline final def inBrackets[T](body: => T): T = {
    accept[LeftBracket]
    val ret = body
    accept[RightBracket]
    ret
  }

  /* ------------- POSITION HANDLING ------------------------------------------- */

  // NOTE: `startTokenPos` and `endTokenPos` are BOTH INCLUSIVE.
  // This is at odds with the rest of scala.meta, where ends are non-inclusive.
  sealed trait StartPos {
    def startTokenPos: Int
  }
  sealed trait EndPos {
    def endTokenPos: Int
  }
  sealed trait Pos extends StartPos with EndPos
  case class IndexPos(index: Int) extends Pos {
    def startTokenPos = index
    def endTokenPos = startTokenPos
  }
  case class TokenPos(token: Token) extends Pos {
    def startTokenPos = token.index
    def endTokenPos = startTokenPos
  }
  case class TreePos(tree: Tree) extends Pos {
    val (startTokenPos, endTokenPos) = tree.origin match {
      case Origin.Parsed(_, _, pos) => (pos.start, pos.end - 1)
      case _ =>
        sys.error(s"internal error: unpositioned prototype ${tree.syntax}: ${tree.structure}")
    }
  }
  case object AutoPos extends Pos {
    def startTokenPos = in.tokenPos
    def endTokenPos = in.prevTokenPos
  }
  case object StartPosPrev extends StartPos {
    def startTokenPos = in.prevTokenPos
  }
  case object EndPosPreOutdent extends EndPos {
    def endTokenPos = if (in.token.is[Indentation.Outdent]) in.tokenPos else in.prevTokenPos
  }
  implicit def intToIndexPos(index: Int): IndexPos = IndexPos(index)
  implicit def tokenToTokenPos(token: Token): TokenPos = TokenPos(token)
  implicit def treeToTreePos(tree: Tree): TreePos = TreePos(tree)
  implicit def optionTreeToPos(tree: Option[Tree]): Pos = tree.fold[Pos](AutoPos)(TreePos)
  implicit def modsToPos(mods: List[Mod]): Pos = mods.headOption
  def auto = AutoPos

  def atPos[T <: Tree](tree: Tree)(body: => T): T = {
    body.withOrigin(tree.origin)
  }

  def atPos[T <: Tree](token: Token)(body: => T): T = {
    atPos(token.startTokenPos, token.endTokenPos)(body)
  }
  def atPos[T <: Tree](start: StartPos, end: EndPos)(body: => T): T = {
    val startTokenPos = start.startTokenPos
    atPosWithBody(startTokenPos, body, end)
  }

  def atPosWithBody[T <: Tree](startTokenPos: Int, body: T, endPos: EndPos): T = {
    var endTokenPos = endPos.endTokenPos
    if (endTokenPos < startTokenPos) endTokenPos = startTokenPos - 1

    def skipBy(range: Range, f: Token => Boolean): Int =
      range.dropWhile(i => f(scannerTokens(i))).headOption.getOrElse(range.start)
    @inline def skipTrivia(range: Range): Int = skipBy(range, _.is[Trivia])
    @inline def skipWhitespace(range: Range): Int = skipBy(range, _.is[Whitespace])

    val rangeFromStart = startTokenPos to endTokenPos
    val (start, end) =
      if (rangeFromStart.isEmpty) (startTokenPos, endTokenPos)
      else (skipTrivia(rangeFromStart), skipWhitespace(rangeFromStart.reverse))

    val endExcl = if (start == end && scannerTokens(start).is[Trivia]) end else end + 1
    val pos = TokenStreamPosition(start, endExcl)
    body.withOrigin(Origin.Parsed(input, dialect, pos))
  }

  def atPosTry[T <: Tree](start: StartPos, end: EndPos)(body: => Try[T]): Try[T] = {
    val startTokenPos = start.startTokenPos
    body.map(atPosWithBody(startTokenPos, _, end))
  }

  def autoPos[T <: Tree](body: => T): T = atPos(start = auto, end = auto)(body)

  def autoPosTry[T <: Tree](body: => Try[T]): Try[T] =
    atPosTry(start = auto, end = auto)(body)

  /* ------------- ERROR HANDLING ------------------------------------------- */

  lazy val reporter = Reporter()
  import reporter._

  private var inFunReturnType = false
  @inline private def fromWithinReturnType[T](body: => T): T = {
    val saved = inFunReturnType
    inFunReturnType = true
    try body
    finally inFunReturnType = saved
  }

  def syntaxErrorExpected[T <: Token: TokenInfo]: Nothing =
    syntaxError(s"${implicitly[TokenInfo[T]].name} expected but ${token.name} found", at = token)

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept[T <: Token: TokenInfo]: Unit =
    if (token.is[T](implicitly[TokenInfo[T]])) {
      if (token.isNot[EOF]) next()
    } else syntaxErrorExpected[T]

  /** If current token is T consume it. */
  def acceptOpt[T <: Token: TokenInfo]: Unit =
    if (token.is[T]) next()

  def acceptStatSep(): Unit = token match {
    case LF() | LFLF() => next()
    case _ if in.observeOutdented() =>
    case t if t.is[EndMarkerIntro] =>
    case _ => accept[Semicolon]
  }
  def acceptStatSepOpt() =
    if (!token.is[StatSeqEnd])
      acceptStatSep()

  /* ------------- MODIFIER VALIDATOR --------------------------------------- */

  def rejectMod[M <: Mod](
      mods: List[Mod],
      errorMsg: String
  )(implicit classifier: Classifier[Mod, M], tag: ClassTag[M]) = {
    mods.getAll[M].foreach(m => syntaxError(errorMsg, at = m))
  }

  def rejectModCombination[M1 <: Mod, M2 <: Mod](
      mods: List[Mod],
      culpritOpt: Option[String] = None
  )(
      implicit invalidMod: InvalidModCombination[M1, M2],
      classifier1: Classifier[Mod, M1],
      tag1: ClassTag[M1],
      classifier2: Classifier[Mod, M2],
      tag2: ClassTag[M2]
  ) = {
    val errorMsg = invalidMod.errorMessage
    val forCulprit = culpritOpt.fold("")(formatCulprit)
    val enrichedErrorMsg = errorMsg + forCulprit
    mods.getIncompatible[M1, M2].foreach { m => syntaxError(enrichedErrorMsg, at = m._1) }
  }

  private def formatCulprit(culprit: String): String = s" for: $culprit"

  def onlyAllowedMods[M1 <: Mod, M2 <: Mod](mods: List[Mod], culprit: String)(
      implicit classifier1: Classifier[Mod, M1],
      classifier2: Classifier[Mod, M2]
  ): Unit = {
    onlyAllowedMods(mods, List(classifier1.apply, classifier2.apply), culprit)
  }

  def onlyAllowedMods(mods: List[Mod], matchers: List[Mod => Boolean], culprit: String): Unit = {
    mods
      .foreach {
        case m if matchers.exists(f => f(m)) =>
        case m => syntaxError(s" Invalid modifier ${m}${formatCulprit(culprit)}", at = m)
      }
  }

  def summonClassifierFunc[A, B](implicit v: Classifier[A, B]): A => Boolean = v.apply

  def onlyAcceptMod[M <: Mod: ClassTag, T <: Token: TokenInfo](
      mods: List[Mod],
      errorMsg: String
  )(implicit classifier: Classifier[Mod, M]) = {
    if (token.isNot[T]) {
      mods.getAll[M].foreach(m => syntaxError(errorMsg, at = m))
    }
  }

  class InvalidModCombination[M1 <: Mod, M2 <: Mod](m1: M1, m2: M2) {
    def errorMessage: String = Messages.IllegalCombinationModifiers(m1, m2)
  }

  implicit object InvalidOpenFinal extends InvalidModCombination(Mod.Open(), Mod.Final())
  implicit object InvalidOpenSealed extends InvalidModCombination(Mod.Open(), Mod.Sealed())
  implicit object InvalidCaseImplicit extends InvalidModCombination(Mod.Case(), Mod.Implicit())
  implicit object InvalidFinalAbstract extends InvalidModCombination(Mod.Final(), Mod.Abstract())
  implicit object InvalidFinalSealed extends InvalidModCombination(Mod.Final(), Mod.Sealed())
  implicit object InvalidOverrideAbstract
      extends InvalidModCombination(Mod.Override(), Mod.Abstract())
  implicit object InvalidPrivateProtected
      extends InvalidModCombination(Mod.Private(Name.Anonymous()), Mod.Protected(Name.Anonymous()))
  implicit object InvalidProtectedPrivate
      extends InvalidModCombination(Mod.Protected(Name.Anonymous()), Mod.Private(Name.Anonymous()))

  /* -------------- TOKEN CLASSES ------------------------------------------- */

  private def isIdentAnd(token: Token, pred: String => Boolean): Boolean = token match {
    case Ident(value) => pred(value.stripPrefix("`").stripSuffix("`"))
    case _ => false
  }

  def followedByToken[T <: Token: TokenInfo]: Boolean = {
    def startBlock = token.is[LeftBrace] || token.is[Indentation.Indent]
    def endBlock = token.is[RightBrace] || token.is[Indentation.Outdent]
    @tailrec
    def lookForToken(braces: Int = 0): Boolean = {
      if (braces == 0 && token.is[T]) true
      else if (braces == 0 && (token.is[StopScanToken] || endBlock)) false
      else if (token.is[EOF]) false
      else {
        val newBraces =
          if (startBlock) braces + 1
          else if (endBlock) braces - 1
          else braces
        next()
        lookForToken(newBraces)
      }
    }
    val fork = in.fork
    val isFollowedBy = lookForToken()
    in = fork
    isFollowedBy
  }
  def isIdentAnd(pred: String => Boolean): Boolean = isIdentAnd(token, pred)
  def isUnaryOp: Boolean = isIdentAnd(name => Term.Name(name).isUnaryOp)
  def isIdentExcept(except: String) = isIdentAnd(_ != except)
  def isIdentOf(name: String) = isIdentAnd(_ == name)
  def isIdent: Boolean = isIdentAnd(_ => true)
  def isStar: Boolean = isIdentOf("*")
  def isBar: Boolean = isIdentOf("|")
  def isAmpersand: Boolean = isIdentOf("&")
  def isColonWildcardStar: Boolean = token.is[Colon] && ahead(token.is[Underscore] && ahead(isStar))
  def isSpliceFollowedBy(check: => Boolean): Boolean =
    token.is[Ellipsis] && ahead(token.is[Unquote] && ahead(token.is[Ident] || check))
  def isBackquoted: Boolean = token.syntax.startsWith("`") && token.syntax.endsWith("`")
  def isVarargStarParam(allowRepeated: Boolean) =
    dialect.allowPostfixStarVarargSplices && isStar && token.next.is[RightParen] && allowRepeated
  private implicit class XtensionTokenClass(token: Token) {
    def isClassOrObject = token.is[KwClass] || token.is[KwObject]
    def isClassOrObjectOrEnum = isClassOrObject || (token.is[Ident] && dialect.allowEnums)
  }

  @classifier
  trait MacroSplicedIdent {
    def unapply(token: Token): Boolean = {
      dialect.allowSpliceAndQuote && QuoteSpliceContext.isInside() && isIdentAnd(_.head == '$')
    }
  }

  @classifier
  trait MacroQuotedIdent {
    def unapply(token: Token): Boolean = {
      dialect.allowSpliceAndQuote && QuoteSpliceContext.isInside() && token.is[Constant.Symbol]
    }
  }

  @classifier
  trait CloseDelim {
    def unapply(token: Token): Boolean = {
      token.is[RightBrace] || token.is[RightBracket] || token.is[RightParen] || token
        .is[Indentation.Outdent]
    }
  }

  @classifier
  trait TypeIntro {
    def unapply(token: Token): Boolean = {
      token.is[Ident] || token.is[KwSuper] || token.is[KwThis] ||
      token.is[LeftParen] || token.is[At] || token.is[Underscore] ||
      token.is[Unquote] || (token.is[Literal] && dialect.allowLiteralTypes)
    }
  }

  @classifier
  trait EndMarkerWord {
    def unapply(token: Token): Boolean = {
      token.is[Ident] || token.is[KwIf] || token.is[KwWhile] || token.is[KwFor] ||
      token.is[KwMatch] || token.is[KwTry] || token.is[KwNew] || token.is[KwThis] ||
      token.is[KwGiven] || token.is[KwVal]
    }
  }

  @classifier
  trait StopScanToken {
    def unapply(token: Token): Boolean = {
      token.is[KwPackage] || token.is[KwExport] || token.is[KwImport] ||
      token.is[KwIf] || token.is[KwElse] || token.is[KwWhile] || token.is[KwDo] ||
      token.is[KwFor] || token.is[KwYield] || token.is[KwNew] || token.is[KwTry] ||
      token.is[KwCatch] || token.is[KwFinally] || token.is[KwThrow] || token.is[KwReturn] ||
      token.is[KwMatch] || token.is[EOF] || token.is[Semicolon] || token.is[Modifier] || token
        .is[DefIntro]
    }
  }

  @classifier
  trait EndMarkerIntro {
    def unapply(token: Token): Boolean = {
      dialect.allowEndMarker &&
      token.is[Ident] &&
      token.text == "end" &&
      token.strictNext.is[EndMarkerWord] &&
      (token.next.strictNext.is[LineEnd] || token.next.strictNext.is[EOF])
    }
  }
  // then  else  do  catch  finally  yield  match
  @classifier
  trait CanContinueOnNextLine {
    def unapply(token: Token): Boolean = {
      token.is[KwThen] || token.is[KwElse] || token.is[KwDo] ||
      token.is[KwCatch] || token.is[KwFinally] || token.is[KwYield] ||
      token.is[KwMatch]
    }
  }

  @classifier
  trait ExprIntro {
    def unapply(token: Token): Boolean = {
      (token.is[Ident] && !token.is[SoftModifier] && !token.is[EndMarkerIntro]) ||
      token.is[Literal] || token.is[Interpolation.Id] || token.is[Xml.Start] ||
      token.is[KwDo] || token.is[KwFor] || token.is[KwIf] ||
      token.is[KwNew] || token.is[KwReturn] || token.is[KwSuper] ||
      token.is[KwThis] || token.is[KwThrow] || token.is[KwTry] || token.is[KwWhile] ||
      token.is[LeftParen] || token.is[LeftBrace] || token.is[Underscore] ||
      token.is[Unquote] || token.is[MacroSplice] || token.is[MacroQuote] ||
      token.is[Indentation.Indent] || (token.is[LeftBracket] && dialect.allowPolymorphicFunctions)
    }
  }

  @classifier
  trait SoftModifier {
    def unapply(token: Token): Boolean = {
      (
        (token.is[soft.KwInline] || token.is[soft.KwOpaque] || token.is[soft.KwTransparent]) &&
          (token.next.is[DclIntro] || token.next.is[Modifier])
      ) || (token.is[soft.KwTransparent] && token.next.is[KwTrait]) ||
      ((token.is[soft.KwOpen] || token.is[soft.KwInfix]) && token.next.is[DefIntro]) ||
      token.is[InlineMatchMod]

    }
  }

  @classifier
  trait InlineMatchMod {
    def unapply(token: Token): Boolean = {
      token.is[soft.KwInline] && {
        val nextToken = token.next
        nextToken.is[LeftParen] || nextToken.is[LeftBrace] || nextToken.is[KwNew] ||
        nextToken.is[Ident] || nextToken.is[Literal] || nextToken.is[Interpolation.Id] ||
        nextToken.is[Xml.Start] || nextToken.is[KwSuper] || nextToken.is[KwThis] ||
        nextToken.is[MacroSplice] || nextToken.is[MacroQuote]
      }
    }
  }

  @classifier
  trait CaseIntro {
    def unapply(token: Token): Boolean = {
      token.is[KwCase] && !token.next.isClassOrObject
    }
  }

  @classifier
  trait DefIntro {
    def unapply(token: Token): Boolean = {
      token.is[Modifier] || token.is[At] ||
      token.is[TemplateIntro] || token.is[DclIntro] ||
      (token.is[Unquote] && token.next.is[DefIntro]) ||
      (token.is[Ellipsis] && token.next.is[DefIntro]) ||
      (token.is[KwCase] && token.next.isClassOrObjectOrEnum)
    }
  }

  @classifier
  trait TemplateIntro {
    def unapply(token: Token): Boolean = {
      token.is[Modifier] || token.is[At] ||
      token.is[KwClass] || token.is[KwObject] || token.is[KwTrait] ||
      (token.is[Unquote] && token.next.is[TemplateIntro]) ||
      (token.is[KwCase] && token.next.isClassOrObjectOrEnum)
    }
  }

  @classifier
  trait DclIntro {
    def unapply(token: Token): Boolean = {
      token.is[KwDef] || token.is[KwType] || token.is[KwEnum] ||
      token.is[KwVal] || token.is[KwVar] || token.is[KwGiven] ||
      token.is[KwExtension] || (token.is[Unquote] && token.next.is[DclIntro])
    }
  }

  @classifier
  trait KwExtension {
    def unapply(token: Token) = {
      // Logic taken from the Scala 3 parser
      token.is[soft.KwExtension] && (token.next.is[LeftParen] || token.next.is[LeftBracket])

    }
  }

  @classifier
  trait Modifier {
    def unapply(token: Token): Boolean = {
      token.is[KwAbstract] || token.is[KwFinal] ||
      token.is[KwSealed] || token.is[KwImplicit] ||
      token.is[KwLazy] || token.is[KwPrivate] ||
      token.is[KwProtected] || token.is[KwOverride] ||
      token.is[SoftModifier]
    }
  }

  @classifier
  trait NonParamsModifier {
    def unapply(token: Token): Boolean = {
      token.is[soft.KwOpen] || token.is[soft.KwOpaque] || token.is[soft.KwTransparent] || token
        .is[soft.KwInfix]
    }
  }

  @classifier
  trait NonlocalModifier {
    def unapply(token: Token): Boolean = {
      token.is[KwPrivate] || token.is[KwProtected] ||
      token.is[KwOverride] || token.is[soft.KwOpen]
    }
  }

  @classifier
  trait LocalModifier {
    def unapply(token: Token): Boolean = {
      token.is[Modifier] && !token.is[NonlocalModifier]
    }
  }

  @classifier
  trait StatSeqEnd {
    def unapply(token: Token): Boolean = {
      token.is[RightBrace] || token.is[EOF] || token.is[Indentation.Outdent]
    }
  }

  @classifier
  trait CaseDefEnd {
    def unapply(token: Token): Boolean = {
      token.is[RightBrace] || token.is[EOF] || token.is[Indentation.Outdent] ||
      (token.is[KwCase] && !token.next.isClassOrObject) || token.is[RightParen] ||
      (token.is[Ellipsis] && token.next.is[KwCase])
    }
  }

  @classifier
  trait CanStartIndent {
    def unapply(token: Token): Boolean = {
      token.is[KwYield] || token.is[KwTry] || token.is[KwCatch] || token.is[KwFinally] ||
      token.is[KwMatch] || token.is[KwDo] || token.is[KwFor] || token.is[KwThen] ||
      token.is[KwElse] || token.is[Equals] || token.is[KwWhile] || token.is[KwIf] ||
      token.is[RightArrow] || token.is[KwReturn] || token.is[LeftArrow] ||
      token.is[ContextArrow] || (
        token.is[KwWith] && {
          val next = token.next
          next.is[DefIntro] || next.is[KwImport] || next.is[KwExport]
        }
      )
    }
  }

  @classifier
  trait CantStartStat {
    def unapply(token: Token): Boolean = {
      token.is[KwCatch] || token.is[KwElse] || token.is[KwExtends] ||
      token.is[KwFinally] || token.is[KwForsome] || token.is[KwMatch] ||
      token.is[KwWith] || token.is[KwYield] ||
      token.is[RightParen] || token.is[LeftBracket] || token.is[RightBracket] ||
      token.is[RightBrace] || token.is[Comma] || token.is[Colon] ||
      token.is[Dot] || token.is[Equals] || token.is[Semicolon] ||
      token.is[Hash] || token.is[RightArrow] || token.is[LeftArrow] ||
      token.is[Subtype] || token.is[Supertype] || token.is[Viewbound] ||
      token.is[LF] || token.is[LFLF] || token.is[EOF]

    }
  }

  @classifier
  trait CanEndStat {
    def unapply(token: Token): Boolean = {
      token.is[Ident] || token.is[KwGiven] || token.is[Literal] ||
      token.is[Interpolation.End] || token.is[Xml.End] ||
      token.is[KwReturn] || token.is[KwThis] || token.is[KwType] ||
      token.is[RightParen] || token.is[RightBracket] || token.is[RightBrace] ||
      token.is[Underscore] || token.is[Ellipsis] || token.is[Unquote] ||
      token.prev.is[EndMarkerIntro]
    }
  }

  @classifier
  trait StatSep {
    def unapply(token: Token): Boolean = {
      token.is[Semicolon] || token.is[LF] || token.is[LFLF] || token.is[EOF]
    }
  }

  @classifier
  trait Literal {
    def unapply(token: Token): Boolean = token match {
      case Constant.Int(_) => true
      case Constant.Long(_) => true
      case Constant.Float(_) => true
      case Constant.Double(_) => true
      case Constant.Char(_) => true
      case Constant.Symbol(_) => true
      case Constant.String(_) => true
      case KwTrue() => true
      case KwFalse() => true
      case KwNull() => true
      case _ => false
    }
  }

  @classifier
  trait NumericLiteral {
    def unapply(token: Token): Boolean = token match {
      case Constant.Int(_) => true
      case Constant.Long(_) => true
      case Constant.Float(_) => true
      case Constant.Double(_) => true
      case _ => false
    }
  }

  @classifier
  trait Whitespace {
    def unapply(token: Token): Boolean = {
      token.is[Space] || token.is[Tab] ||
      token.is[LineEnd] || token.is[FF]
    }
  }

  @classifier
  trait LineEnd {
    def unapply(token: Token): Boolean = {
      token.is[LF] || token.is[LFLF] || token.is[CR]
    }
  }

  @classifier
  trait Trivia {
    def unapply(token: Token): Boolean = {
      token.is[Whitespace] || token.is[Comment]
    }
  }

  @classifier
  trait CanStartColonEol {
    def unapply(token: Token): Boolean = {
      token.is[KwTrait] || token.is[KwClass] ||
      token.is[KwObject] || token.is[KwEnum] ||
      token.is[KwType] || token.is[KwPackage] ||
      token.is[KwGiven] || token.is[KwNew]
    }
  }

  @classifier
  trait MultilineComment {
    def unapply(token: Token): Boolean =
      token.is[Comment] && isMultilinePos(token.pos)
  }

  /* ---------- TREE CONSTRUCTION ------------------------------------------- */

  def ellipsis[T <: Tree: AstInfo](rank: Int, astInfo: AstInfo[T], extraSkip: => Unit = {}): T =
    autoPos {
      token match {
        case ellipsis: Ellipsis =>
          if (dialect.allowUnquotes) {
            if (ellipsis.rank != rank) {
              syntaxError(Messages.QuasiquoteRankMismatch(ellipsis.rank, rank), at = ellipsis)
            } else {
              next()
              extraSkip
              val tree = {
                val result = token match {
                  case LeftParen() => inParens(unquote[T])
                  case LeftBrace() => inBraces(unquote[T])
                  case Unquote() => unquote[T]
                  case _ => syntaxError(s"$$, ( or { expected but ${token.name} found", at = token)
                }
                result match {
                  case quasi: Quasi =>
                    // NOTE: In the case of an unquote nested directly under ellipsis, we get a bit of a mixup.
                    // Unquote's pt may not be directly equal unwrapped ellipsis's pt, but be its refinement instead.
                    // For example, in `new { ..$stats }`, ellipsis's pt is List[Stat], but quasi's pt is Term.
                    // This is an artifact of the current implementation, so we just need to keep it mind and work around it.
                    require(
                      classTag[T].runtimeClass.isAssignableFrom(quasi.pt) &&
                        debug(ellipsis, result, result.structure)
                    )
                    atPos(quasi, quasi)(astInfo.quasi(quasi.rank, quasi.tree))
                  case other =>
                    other
                }
              }
              astInfo.quasi(rank, tree)
            }
          } else {
            syntaxError(s"$dialect doesn't support ellipses", at = ellipsis)
          }
        case _ =>
          unreachable(debug(token))
      }
    }

  def unquote[T <: Tree: AstInfo](advance: Boolean = true): T = autoPos {
    token match {
      case unquote: Unquote =>
        require(unquote.input.chars(unquote.start + 1) != '$')
        if (dialect.allowUnquotes) {
          // NOTE: I considered having Input.Slice produce absolute positions from the get-go,
          // but then such positions wouldn't be usable with Input.Slice.chars.
          val unquotedTree = {
            try {
              val unquoteInput = Input.Slice(input, unquote.start + 1, unquote.end)
              val unquoteDialect = dialect.copy(
                allowTermUnquotes = false,
                allowPatUnquotes = false,
                allowMultilinePrograms = true
              )
              val unquoteParser = new ScalametaParser(unquoteInput)(unquoteDialect)
              if (dialect.allowTermUnquotes) unquoteParser.parseUnquoteTerm()
              else if (dialect.allowPatUnquotes) unquoteParser.parseUnquotePat()
              else unreachable
            } catch {
              case ex: Exception => throw ex.absolutize
            }
          }
          if (advance) {
            next()
            implicitly[AstInfo[T]].quasi(0, unquotedTree)
          } else
            ahead {
              implicitly[AstInfo[T]].quasi(0, unquotedTree)
            }
        } else {
          syntaxError(s"$dialect doesn't support unquotes", at = unquote)
        }
      case _ =>
        unreachable(debug(token))
    }
  }

  def unquote[T <: Tree: AstInfo]: T =
    unquote[T](advance = true) // to write `unquote[T]` without round braces

  final def tokenSeparated[Sep <: Token: TokenInfo, T <: Tree: AstInfo](
      sepFirst: Boolean,
      part: => T
  ): List[T] = {
    def partOrEllipsis =
      if (token.is[Ellipsis]) ellipsis(1, astInfo[T])
      else part
    val ts = new ListBuffer[T]
    if (!sepFirst)
      ts += partOrEllipsis
    while (token.is[Sep] || token.is[Ellipsis]) {
      if (token.is[Sep]) next()
      ts += partOrEllipsis
    }
    ts.toList
  }

  @inline final def commaSeparated[T <: Tree: AstInfo](part: => T): List[T] =
    tokenSeparated[Comma, T](sepFirst = false, part)

  def makeTuple[T <: Tree](body: List[T], zero: () => T, tuple: List[T] => T): T = body match {
    case Nil => zero()
    case only :: Nil =>
      only match {
        case q: Quasi if q.rank == 1 => tuple(body)
        case _ => only
      }
    case _ => tuple(body)
  }

  def makeTupleTerm(body: List[Term]): Term = {
    // NOTE: we can't make this autoPos
    // see comments to makeTupleType for discussion
    body match {
      case List(q @ Term.Quasi(1, _)) => atPos(q, q)(Term.Tuple(body))
      case _ => makeTuple[Term](body, () => Lit.Unit(), Term.Tuple(_))
    }
  }

  def makeTupleType(body: List[Type]): Type = {
    def invalidLiteralUnitType =
      syntaxError("illegal literal type (), use Unit instead", at = token.pos)
    // NOTE: we can't make this autoPos
    // because, by the time control reaches this method, we're already past the closing parenthesis
    // therefore, we'll rely on our callers to assign positions to the tuple we return
    // we can't do atPos(body.first, body.last) either, because that wouldn't account for parentheses
    body match {
      case List(q @ Type.Quasi(1, _)) => atPos(q, q)(Type.Tuple(body))
      case _ => makeTuple[Type](body, () => invalidLiteralUnitType, Type.Tuple(_))
    }
  }

  def inParensOrTupleOrUnit(location: Location, allowRepeated: Boolean): Term = {
    // NOTE: we can't make this autoPos
    // see comments to makeTupleType for discussion
    val maybeTupleArgs = inParens({
      if (token.is[RightParen]) Nil
      else commaSeparated(expr(location = location, allowRepeated = allowRepeated))
    })
    maybeTupleArgs match {
      case List(Term.Quasi(1, _)) =>
        makeTupleTerm(maybeTupleArgs)
      case List(singleArg) =>
        singleArg
      case multipleArgs =>
        val repeatedArgs = multipleArgs.collect { case repeated: Term.Repeated => repeated }
        repeatedArgs.foreach(arg =>
          syntaxError("repeated argument not allowed here", at = arg.tokens.last.prev)
        )
        makeTupleTerm(multipleArgs)
    }
  }

  /* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

  /**
   * Methods which implicitly propagate the context in which they were called: either in a pattern
   * context or not. Formerly, this was threaded through numerous methods as boolean isPattern.
   */
  trait PatternContextSensitive {
    private def tupleInfixType(allowFunctionType: Boolean = true): Type = autoPos {
      require(token.is[LeftParen] && debug(token))
      val openParenPos = in.tokenPos

      // NOTE: This is a really hardcore disambiguation caused by introduction of Type.Method.
      // We need to accept `(T, U) => W`, `(x: T): x.U` and also support unquoting.
      var hasParams = false
      var hasImplicits = false
      var hasTypes = false
      var secondOpenParenPos = -1
      var closeParenPos = -1
      val rawtss: List[List[Tree]] = {

        def typedParam() = autoPos {
          val name = typeName()
          accept[Colon]
          Type.TypedParam(name, typ())
        }
        @tailrec
        def paramOrType(): Tree = token match {
          case Ellipsis(rank) =>
            ellipsis(rank, astInfo[Tree])
          case Unquote() =>
            unquote[Tree]
          case KwImplicit() if !hasImplicits =>
            next()
            hasImplicits = true
            paramOrType()
          case Ident(_) if ahead(token.is[Colon]) =>
            if (hasTypes)
              syntaxError(
                "can't mix function type and dependent function type syntaxes",
                at = token
              )
            hasParams = true
            typedParam()
          case _ =>
            if (hasParams)
              syntaxError(
                "can't mix function type and dependent function type syntaxes",
                at = token
              )
            hasTypes = true
            paramType()
        }

        val rawtss = new ListBuffer[List[Tree]]
        while (!hasTypes && !hasImplicits && token.is[LeftParen]) {
          if (openParenPos != in.tokenPos && secondOpenParenPos == 0)
            secondOpenParenPos = in.tokenPos
          accept[LeftParen]
          val rawts = new ListBuffer[Tree]
          if (!token.is[RightParen]) {
            rawts += paramOrType()
            while (token.is[Comma] || token.is[Ellipsis]) {
              if (token.is[Comma]) next()
              rawts += paramOrType()
            }
          }
          closeParenPos = in.tokenPos
          accept[RightParen]
          // NOTE: can't have this, because otherwise we run into #312
          // newLineOptWhenFollowedBy[LeftParen]
          rawtss += rawts.toList
        }
        rawtss.toList
      }

      def ts: List[Type] = {
        if (hasParams && !dialect.allowDependentFunctionTypes)
          syntaxError("dependent function types are not supported", at = token)
        if (rawtss.length != 1) {
          val message = "can't have multiple parameter lists in function types"
          syntaxError(message, at = scannerTokens(secondOpenParenPos))
        }
        rawtss.head.map({
          case q: Quasi => q.become[Type.Quasi]
          case t: Type => t
          case other => unreachable(debug(other.syntax, other.structure))
        })
      }

      if (allowFunctionType && token.is[RightArrow]) {
        next()
        Type.Function(ts, typeIndentedOpt())
      } else if (allowFunctionType && token.is[ContextArrow]) {
        next()
        Type.ContextFunction(ts, typeIndentedOpt())
      } else {
        val tuple = atPos(openParenPos, closeParenPos)(makeTupleType(ts map {
          case t: Type.ByName => syntaxError("by name type not allowed here", at = t)
          case t: Type.Repeated => syntaxError("repeated type not allowed here", at = t)
          case t: Type => t
        }))
        infixTypeRest(
          compoundTypeRest(Some(annotTypeRest(simpleTypeRest(tuple)))),
          InfixMode.FirstOp
        )
      }
    }

    private def typeLambdaOrPoly(): Type = {
      val quants = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
      if (token.is[TypeLambdaArrow]) {
        accept[TypeLambdaArrow]
        val tpe = typeIndentedOpt()
        Type.Lambda(quants, tpe)
      } else if (token.is[RightArrow]) {
        accept[RightArrow]
        val tpe = typeIndentedOpt()
        if (tpe.is[Type.Function])
          Type.PolyFunction(quants, tpe)
        else if (tpe.is[Type.ContextFunction])
          Type.PolyFunction(quants, tpe)
        else
          syntaxError("polymorphic function types must have a value parameter", at = token)
      } else {
        syntaxError("expected =>> or =>", at = token)
      }
    }

    def typeIndentedOpt(): Type = {
      if (token.is[Indentation.Indent]) {
        indented(typ())
      } else {
        typ()
      }
    }

    def typ(): Type = autoPos {
      val t: Type =
        if (token.is[LeftBracket] && dialect.allowTypeLambdas) typeLambdaOrPoly()
        else infixTypeOrTuple()

      token match {
        case RightArrow() => next(); Type.Function(List(t), typeIndentedOpt())
        case ContextArrow() => next(); Type.ContextFunction(List(t), typeIndentedOpt())
        case KwForsome() => next(); Type.Existential(t, existentialStats())
        case KwMatch() if dialect.allowTypeMatch => next(); Type.Match(t, typeCaseClauses())
        case _ => t
      }
    }

    def typeCaseClauses(): List[TypeCase] = {
      def cases() = {
        val allCases = new ListBuffer[TypeCase]
        while (token.is[KwCase]) {
          allCases += typeCaseClause()
          newLinesOpt()
        }
        allCases.toList
      }
      if (token.is[LeftBrace]) {
        inBraces {
          cases()
        }
      } else if (token.is[Indentation.Indent])
        indented {
          cases()
        }
      else {
        syntaxError("Expected braces or indentation", at = token.pos)
      }

    }

    def typeCaseClause(): TypeCase = autoPos {
      accept[KwCase]
      val pat = infixTypeOrTuple(allowFunctionType = false)
      accept[RightArrow]
      val tpe = typeIndentedOpt()
      TypeCase(
        pat,
        tpe
      )
    }

    def quasiquoteType(): Type = entrypointType()

    def entrypointType(): Type = paramType()

    def typeArgs(): List[Type] = inBrackets(types())

    def infixTypeOrTuple(allowFunctionType: Boolean = true): Type = {
      if (token.is[LeftParen]) tupleInfixType(allowFunctionType)
      else infixType(InfixMode.FirstOp)
    }

    def infixType(mode: InfixMode.Value): Type =
      infixTypeRest(compoundType(), mode)

    def infixTypeRest(t: Type, mode: InfixMode.Value): Type = atPos(t, auto) {
      if (isIdent || token.is[Unquote]) {
        if (isStar && ahead(
            token.is[RightParen] || token.is[Comma] || token.is[Equals] ||
              token.is[RightBrace] || token.is[EOF]
          )) {
          // we assume that this is a type specification for a vararg parameter
          t
        } else {
          val name = termName(advance = false)
          val leftAssoc = name.isLeftAssoc
          if (mode != InfixMode.FirstOp) checkAssoc(name, leftAssoc = mode == InfixMode.LeftOp)
          if (isAmpersand && dialect.allowAndTypes) {
            next()
            newLineOptWhenFollowing(_.is[TypeIntro])
            val t1 = compoundType()
            infixTypeRest(atPos(t, t1)(Type.And(t, t1)), InfixMode.LeftOp)
          } else if (isBar && dialect.allowOrTypes) {
            next()
            newLineOptWhenFollowing(_.is[TypeIntro])
            val t1 = compoundType()
            infixTypeRest(atPos(t, t1)(Type.Or(t, t1)), InfixMode.LeftOp)
          } else {
            val op = typeName()
            newLineOptWhenFollowing(_.is[TypeIntro])
            def mkOp(t1: Type) = atPos(t, t1)(Type.ApplyInfix(t, op, t1))
            if (leftAssoc)
              infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
            else
              mkOp(infixType(InfixMode.RightOp))
          }
        }
      } else {
        t
      }
    }

    def compoundType(): Type = compoundTypeRest {
      if (token.is[LeftBrace])
        None
      else
        Some(annotType())
    }

    def compoundTypeRest(t0: Option[Type]): Type = atPos(t0, auto) {
      t0 match {
        case Some(t0) =>
          var t = t0
          while (token.is[KwWith]) {
            next()
            val rhs = annotType()
            t = atPos(t, rhs)(Type.With(t, rhs))
          }
          newLineOptWhenFollowedBy[LeftBrace]
          if (token.is[LeftBrace]) refinement(innerType = Some(t))
          else t
        case None =>
          refinement(innerType = None)
      }
    }

    def annotType(): Type = annotTypeRest(simpleType())

    def annotTypeRest(t: Type): Type = atPos(t, auto) {
      val annots = ScalametaParser.this.annots(skipNewLines = false)
      if (annots.isEmpty) t
      else Type.Annotate(t, annots)
    }

    private def allowPlusMinusUnderscore: Boolean =
      dialect.allowPlusMinusUnderscoreAsIdent || dialect.allowPlusMinusUnderscoreAsPlaceholder

    def simpleType(): Type = {
      simpleTypeRest(autoPos(token match {
        case LeftParen() => autoPos(makeTupleType(inParens(types())))
        case Underscore() => next(); atPos(in.prevTokenPos, auto)(Type.Placeholder(typeBounds()))
        case MacroSplicedIdent() =>
          Type.Macro(macroSplicedIdent())
        case MacroSplice() =>
          Type.Macro(macroSplice())
        case Ident("?") if dialect.allowQuestionMarkPlaceholder =>
          next(); atPos(in.prevTokenPos, auto)(Type.Placeholder(typeBounds()))
        case ident: Ident
            if (ident.value == "+" || ident.value == "-") &&
              ahead(token.is[Underscore]) &&
              allowPlusMinusUnderscore =>
          autoPos {
            accept[Ident]
            accept[Underscore]
            if (dialect.allowPlusMinusUnderscoreAsPlaceholder)
              Type.Placeholder(typeBounds())
            else
              Type.Name(s"${ident.value}_")
          }
        case Literal() =>
          if (dialect.allowLiteralTypes) literal()
          else syntaxError(s"$dialect doesn't support literal types", at = path())
        case Ident("-") if ahead { token.is[NumericLiteral] } && dialect.allowLiteralTypes =>
          val term = termName()
          atPos(term, auto)(literal(isNegated = true))
        case _ =>
          val ref = path() match {
            case q: Quasi => q.become[Term.Ref.Quasi]
            case ref => ref
          }
          if (token.isNot[Dot]) {
            ref match {
              case q: Quasi =>
                q.become[Type.Quasi]
              case Term.Select(qual: Term.Quasi, name: Term.Name.Quasi) =>
                val newQual = qual.become[Term.Ref.Quasi]
                val newName = name.become[Type.Name.Quasi]
                atPos(ref, ref)(Type.Select(newQual, newName))
              case Term.Select(qual: Term.Ref, name) =>
                val newName = name match {
                  case q: Quasi => q.become[Type.Name.Quasi]
                  case _ => atPos(name, name)(Type.Name(name.value))
                }
                atPos(ref, ref)(Type.Select(qual, newName))
              case name: Term.Name =>
                atPos(name, name)(Type.Name(name.value))
              case _ =>
                syntaxError("identifier expected", at = ref)
            }
          } else {
            next()
            accept[KwType]
            Type.Singleton(ref)
          }
      }))
    }

    def simpleTypeRest(t: Type): Type = token match {
      case Hash() => next(); simpleTypeRest(atPos(t, auto)(Type.Project(t, typeName())))
      case LeftBracket() => simpleTypeRest(atPos(t, auto)(Type.Apply(t, typeArgs())))
      case _ => t
    }

    def types(): List[Type] = commaSeparated(typ())

    def patternTyp(allowInfix: Boolean, allowImmediateTypevars: Boolean): Type = {
      def loop(tpe: Type, convertTypevars: Boolean): Type = tpe match {
        case q: Quasi =>
          q
        case tpe @ Type.Name(value) if convertTypevars && value(0).isLower =>
          atPos(tpe, tpe)(Type.Var(tpe))
        case tpe: Type.Name =>
          tpe
        case tpe: Type.Select =>
          tpe
        case Type.Project(qual, name) =>
          val qual1 = loop(qual, convertTypevars = false)
          val name1 = name
          atPos(tpe, tpe)(Type.Project(qual1, name1))
        case tpe: Type.Singleton =>
          tpe
        case Type.Apply(underlying, args) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val args1 = args.map(arg => loop(arg, convertTypevars = true))
          atPos(tpe, tpe)(Type.Apply(underlying1, args1))
        case Type.ApplyInfix(lhs, op, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val op1 = op
          val rhs1 = loop(rhs, convertTypevars = false)
          atPos(tpe, tpe)(Type.ApplyInfix(lhs1, op1, rhs1))
        case Type.ContextFunction(params, res) =>
          val params1 = params.map(p => loop(p, convertTypevars = true))
          val res1 = loop(res, convertTypevars = false)
          atPos(tpe, tpe)(Type.ContextFunction(params1, res1))
        case Type.Function(params, res) =>
          val params1 = params.map(p => loop(p, convertTypevars = true))
          val res1 = loop(res, convertTypevars = false)
          atPos(tpe, tpe)(Type.Function(params1, res1))
        case Type.PolyFunction(tparams, res) =>
          val res1 = loop(res, convertTypevars = false)
          atPos(tpe, tpe)(Type.PolyFunction(tparams, res1))
        case Type.Tuple(elements) =>
          val elements1 = elements.map(el => loop(el, convertTypevars = true))
          atPos(tpe, tpe)(Type.Tuple(elements1))
        case Type.With(lhs, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val rhs1 = loop(rhs, convertTypevars = false)
          atPos(tpe, tpe)(Type.With(lhs1, rhs1))
        case Type.And(lhs, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val rhs1 = loop(rhs, convertTypevars = false)
          atPos(tpe, tpe)(Type.And(lhs1, rhs1))
        case Type.Or(lhs, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val rhs1 = loop(rhs, convertTypevars = false)
          atPos(tpe, tpe)(Type.Or(lhs1, rhs1))
        case Type.Refine(tpe, stats) =>
          val tpe1 = tpe.map(tpe => loop(tpe, convertTypevars = false))
          val stats1 = stats
          atPos(tpe, tpe)(Type.Refine(tpe1, stats1))
        case Type.Existential(underlying, stats) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val stats1 = stats
          atPos(tpe, tpe)(Type.Existential(underlying1, stats1))
        case Type.Annotate(underlying, annots) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val annots1 = annots
          atPos(tpe, tpe)(Type.Annotate(underlying1, annots1))
        case Type.Placeholder(bounds) =>
          val bounds1 = bounds
          atPos(tpe, tpe)(Type.Placeholder(bounds1))
        case tpe: Lit =>
          tpe
      }
      val t: Type = {
        if (allowInfix) {
          val t = if (token.is[LeftParen]) tupleInfixType() else compoundType()
          def mkOp(t1: Type) = atPos(t, t1)(Type.ApplyInfix(t, typeName(), t1))
          token match {
            case KwForsome() => next(); atPos(t, t)(Type.Existential(t, existentialStats()))
            case Unquote() | Ident(_) if !isBar =>
              infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
            case _ => t
          }
        } else {
          compoundType()
        }
      }
      loop(t, convertTypevars = allowImmediateTypevars)
    }

    def patternTypeArgs() =
      inBrackets(commaSeparated(patternTyp(allowInfix = true, allowImmediateTypevars = true)))
  }

  private trait AllowedName[T]
  private object AllowedName {
    implicit object AllowedTermName extends AllowedName[Term.Name]
    implicit object AllowedTypeName extends AllowedName[Type.Name]
  }
  private def name[T <: Tree: AllowedName: AstInfo](ctor: String => T, advance: Boolean): T =
    token match {
      case Ident(value) =>
        val name = value.stripPrefix("`").stripSuffix("`")
        val res = atPos(in.tokenPos, in.tokenPos)(ctor(name))
        if (advance) next()
        res
      case Unquote() =>
        unquote[T](advance)
      case _ =>
        syntaxErrorExpected[Ident]
    }

  def termName(advance: Boolean = true): Term.Name = name(Term.Name(_), advance)
  def typeName(advance: Boolean = true): Type.Name = name(Type.Name(_), advance)

  def path(thisOK: Boolean = true): Term.Ref = {
    val startsAtBof = token.prev.is[BOF]
    def endsAtEof = token.is[EOF]
    def stop = token.isNot[Dot] || ahead {
      token.isNot[KwThis] && token.isNot[KwSuper] && token.isNot[Ident] && token.isNot[Unquote]
    }
    if (token.is[KwThis]) {
      val anonqual = atPos(in.tokenPos, in.prevTokenPos)(Name.Anonymous())
      next()
      val thisp = atPos(in.prevTokenPos, auto)(Term.This(anonqual))
      if (stop && thisOK) thisp
      else {
        accept[Dot]
        selectors(thisp)
      }
    } else if (token.is[KwSuper]) {
      val anonqual = autoPos(Name.Anonymous())
      next()
      val superp = atPos(in.prevTokenPos, auto)(Term.Super(anonqual, mixinQualifier()))
      if (startsAtBof && endsAtEof && dialect.allowUnquotes) return superp
      accept[Dot]
      val supersel = atPos(superp, auto)(Term.Select(superp, termName()))
      if (stop) supersel
      else {
        next()
        selectors(supersel)
      }
    } else {
      val name = termName()
      if (stop) name
      else {
        next()
        if (token.is[KwThis]) {
          next()
          val qual = name match {
            case q: Quasi => q.become[Name.Quasi]
            case name => atPos(name, name)(Name.Indeterminate(name.value))
          }
          val thisp = atPos(name, auto)(Term.This(qual))
          if (stop && thisOK) thisp
          else {
            accept[Dot]
            selectors(thisp)
          }
        } else if (token.is[KwSuper]) {
          next()
          val qual = name match {
            case q: Quasi => q.become[Name.Quasi]
            case name => atPos(name, name)(Name.Indeterminate(name.value))
          }
          val superp = atPos(name, auto)(Term.Super(qual, mixinQualifier()))
          if (startsAtBof && endsAtEof && dialect.allowUnquotes) return superp
          accept[Dot]
          val supersel = atPos(superp, auto)(Term.Select(superp, termName()))
          if (stop) supersel
          else {
            next()
            selectors(supersel)
          }
        } else {
          selectors(name match {
            case q: Quasi => q.become[Term.Quasi]
            case name => name
          })
        }
      }
    }
  }

  def selector(t: Term): Term.Select = atPos(t, auto)(Term.Select(t, termName()))
  def selectors(t: Term): Term.Ref = {
    val t1 = selector(t)
    if (token.is[Dot] && ahead { token.is[Ident] }) {
      next()
      selectors(t1)
    } else t1
  }

  def mixinQualifier(): Name = {
    if (token.is[LeftBracket]) {
      inBrackets {
        typeName() match {
          case q: Quasi => q.become[Name.Quasi]
          case name => atPos(name, name)(Name.Indeterminate(name.value))
        }

      }
    } else {
      autoPos(Name.Anonymous())
    }
  }

  def stableId(): Term.Ref =
    path(thisOK = false)

  def qualId(): Term.Ref = {
    val name = termName() match {
      case q: Quasi => q.become[Term.Ref.Quasi]
      case ref => ref
    }
    if (token.isNot[Dot]) name
    else {
      next()
      selectors(name)
    }
  }

  def literal(isNegated: Boolean = false): Lit = autoPos {
    def isHex = token.syntax.startsWith("0x") || token.syntax.startsWith("0X")
    val res = token match {
      case Constant.Int(rawValue) =>
        val value = if (isNegated) -rawValue else rawValue
        val min = if (isHex) BigInt(Int.MinValue) * 2 + 1 else BigInt(Int.MinValue)
        val max = if (isHex) BigInt(Int.MaxValue) * 2 + 1 else BigInt(Int.MaxValue)
        if (value > max) syntaxError("integer number too large", at = token)
        else if (value < min) syntaxError("integer number too small", at = token)
        Lit.Int(value.toInt)
      case Constant.Long(rawValue) =>
        val value = if (isNegated) -rawValue else rawValue
        val min = if (isHex) BigInt(Long.MinValue) * 2 + 1 else BigInt(Long.MinValue)
        val max = if (isHex) BigInt(Long.MaxValue) * 2 + 1 else BigInt(Long.MaxValue)
        if (value > max) syntaxError("integer number too large", at = token)
        else if (value < min) syntaxError("integer number too small", at = token)
        Lit.Long(value.toLong)
      case Constant.Float(rawValue) =>
        val value = if (isNegated) -rawValue else rawValue
        if (value > Float.MaxValue) syntaxError("floating point number too large", at = token)
        else if (value < Float.MinValue) syntaxError("floating point number too small", at = token)
        Lit.Float(value.toString)
      case Constant.Double(rawValue) =>
        val value = if (isNegated) -rawValue else rawValue
        if (value > Double.MaxValue) syntaxError("floating point number too large", at = token)
        else if (value < Double.MinValue) syntaxError("floating point number too small", at = token)
        Lit.Double(value.toString)
      case Constant.Char(value) =>
        Lit.Char(value)
      case Constant.String(value) =>
        Lit.String(value)
      case _: Constant.Symbol if !dialect.allowSymbolLiterals =>
        syntaxError("Symbol literals are no longer allowed", at = token)
      case Constant.Symbol(value) =>
        Lit.Symbol(value)
      case KwTrue() =>
        Lit.Boolean(true)
      case KwFalse() =>
        Lit.Boolean(false)
      case KwNull() =>
        Lit.Null()
      case _ =>
        unreachable(debug(token))
    }
    next()
    res
  }

  def interpolate[Ctx <: Tree, Ret <: Tree](
      arg: () => Ctx,
      result: (Term.Name, List[Lit], List[Ctx]) => Ret
  ): Ret = autoPos {
    val interpolator = {
      val name = token match {
        case Xml.Start() => Term.Name("xml")
        case Interpolation.Id(value) => Term.Name(value)
        case _ => unreachable(debug(token))
      }
      atPos(in.tokenPos, in.tokenPos)(name)
    }
    if (token.is[Interpolation.Id]) next()
    val partsBuf = new ListBuffer[Lit]
    val argsBuf = new ListBuffer[Ctx]
    @tailrec
    def loop(): Unit = token match {
      case Interpolation.Start() | Xml.Start() =>
        next()
        loop()
      case Interpolation.Part(value) =>
        partsBuf += atPos(in.tokenPos, in.tokenPos)(Lit.String(value))
        next()
        loop()
      case Xml.Part(value) =>
        partsBuf += atPos(in.tokenPos, in.tokenPos)(Lit.String(value))
        next()
        loop()
      case Interpolation.SpliceStart() | Xml.SpliceStart() =>
        next()
        argsBuf += arg()
        loop()
      case Interpolation.SpliceEnd() | Xml.SpliceEnd() =>
        next()
        loop()
      case Interpolation.End() | Xml.End() =>
        next(); // simply return
      case _ =>
        unreachable(debug(token, token.structure))
    }
    loop()
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  def interpolateTerm(): Term.Interpolate = {
    interpolate[Term, Term.Interpolate](unquoteExpr _, Term.Interpolate.apply _)
  }

  def xmlTerm(): Term.Xml =
    interpolate[Term, Term.Xml](unquoteXmlExpr _, (_, parts, args) => Term.Xml.apply(parts, args))

  def interpolatePat(): Pat.Interpolate =
    interpolate[Pat, Pat.Interpolate](unquotePattern _, Pat.Interpolate.apply _)

  def xmlPat(): Pat.Xml =
    interpolate[Pat, Pat.Xml](unquoteXmlPattern _, (_, parts, args) => Pat.Xml.apply(parts, args))

  /* ------------- NEW LINES ------------------------------------------------- */

  def newLineOpt(): Unit = {
    if (token.is[LF]) next()
  }

  def newLinesOpt(): Unit = {
    if (token.is[LF] || token.is[LFLF])
      next()
  }

  def newLineOptWhenFollowedBy[T <: Token: TokenInfo]: Unit = {
    // note: next is defined here because current is token.LF
    if (token.is[LF] && ahead { token.is[T] }) newLineOpt()
  }

  def newLineOptWhenFollowedBySignificantIndentationAnd(cond: Token => Boolean): Unit = {
    def nextLineHasGreaterIndentation = {
      val prev = currentIndentation
      prev >= 0 && ahead { cond(token) && currentIndentation > prev }
    }
    if (token.is[LF] && nextLineHasGreaterIndentation)
      newLineOpt()
  }

  def newLineOptWhenFollowing(p: Token => Boolean): Unit = {
    // note: next is defined here because current is token.LF
    if (token.is[LF] && ahead { p(token) }) newLineOpt()
  }

  /* ------------- TYPES ---------------------------------------------------- */

  def typedOpt(): Option[Type] =
    if (token.is[Colon]) {
      next()
      if (token.is[At] && ahead(token.is[Ident])) {
        Some(outPattern.annotTypeRest(autoPos(Type.AnonymousName())))
      } else {
        Some(typ())
      }
    } else None

  def typeOrInfixType(location: Location): Type =
    if (location == NoStat) typ()
    else startInfixType()

  /* ----------- EXPRESSIONS ------------------------------------------------ */

  def condExpr(): Term = {
    accept[LeftParen]
    val r = expr()
    accept[RightParen]
    r
  }

  def expr(): Term = expr(location = NoStat, allowRepeated = false)

  def quasiquoteExpr(): Term = expr(location = NoStat, allowRepeated = true)

  def entrypointExpr(): Term = expr(location = NoStat, allowRepeated = false)

  def unquoteExpr(): Term =
    token match {
      case Ident(_) => termName()
      case LeftBrace() => dropTrivialBlock(expr(location = NoStat, allowRepeated = true))
      case KwThis() =>
        val qual = autoPos { next(); Name.Anonymous() }
        atPos(in.prevTokenPos, auto)(Term.This(qual))
      case _ =>
        syntaxError(
          "error in interpolated string: identifier, `this' or block expected",
          at = token
        )
    }

  def unquoteXmlExpr(): Term = {
    unquoteExpr()
  }

  private def exprMaybeIndented(): Term = {
    if (token.is[Indentation.Indent]) {
      blockExpr()
    } else {
      expr()
    }
  }

  private def tryAcceptWithOptLF[T <: Token: TokenInfo](
      implicit classifier: Classifier[Token, T]
  ): Boolean = {
    if (token.is[T] || (token.is[LF] && ahead(token.is[T]))) {
      acceptOpt[LF]
      accept[T]
      true
    } else {
      false
    }
  }

  /**
   * Deals with Scala 3 concept of {{{inline x match { ...}}}. Since matches can also be chained in
   * Scala 3 we need to create the Match first and only then add the the inline modifier.
   */
  def inlineMatchClause(inlineMods: List[Mod]) = {
    atPos(inlineMods, auto)(postfixExpr(allowRepeated = false)) match {
      case t: Term.Match =>
        t.setMods(inlineMods)
        t
      case other =>
        syntaxError("`inline` must be followed by an `if` or a `match`", at = other.pos)
    }
  }

  def matchClause(t: Term) = {
    if (token.is[Indentation.Indent]) {
      atPos(t, auto)(Term.Match(t, indented(caseClauses())))
    } else {
      atPos(t, auto)(Term.Match(t, inBracesOrNil(caseClauses())))
    }
  }

  def ifClause(mods: List[Mod] = Nil) = {
    accept[KwIf]
    val (cond, thenp) = if (token.isNot[LeftParen] && dialect.allowSignificantIndentation) {
      val cond = exprMaybeIndented()
      acceptOpt[LF]
      accept[KwThen]
      (cond, exprMaybeIndented())
    } else {
      val cond = condExprInParens[KwThen]
      newLinesOpt()
      if (!tryAcceptWithOptLF[KwThen])
        in.observeIndented()
      (cond, exprMaybeIndented())
    }

    if (tryAcceptWithOptLF[KwElse]) {
      Term.If(cond, thenp, exprMaybeIndented(), mods)
    } else if (token.is[Semicolon] && ahead { token.is[KwElse] }) {
      next(); next(); Term.If(cond, thenp, expr(), mods)
    } else {
      Term.If(cond, thenp, atPos(in.tokenPos, in.prevTokenPos)(Lit.Unit()), mods)
    }
  }

  def condExprInParens[T <: Token: TokenInfo]: Term = {
    def isFollowedBy[U <: Token: TokenInfo] = {
      if (token.is[LF] || token.is[LFLF] || token.is[EOF]) {
        false
      } else { followedByToken[U] }
    }
    if (dialect.allowSignificantIndentation) {
      val forked = in.fork
      val simpleExpr = condExpr()
      if ((token.is[Ident] && isLeadingInfixOperator(token)) || token.is[Dot] || isFollowedBy[T]) {
        in = forked
        val exprCond = expr()
        val nextIsDelimiterKw =
          if (token.is[LF] || token.is[LFLF]) ahead { newLineOpt(); token.is[T] }
          else token.is[T]
        if (nextIsDelimiterKw)
          exprCond
        else
          syntaxErrorExpected[T]
      } else simpleExpr
    } else condExpr()
  }

  // FIXME: when parsing `(2 + 3)`, do we want the ApplyInfix's position to include parentheses?
  // if yes, then nothing has to change here
  // if no, we need eschew autoPos here, because it forces those parentheses on the result of calling prefixExpr
  // see https://github.com/scalameta/scalameta/issues/1083 and https://github.com/scalameta/scalameta/issues/1223
  def expr(location: Location, allowRepeated: Boolean): Term = {
    def inlineMod() = autoPos {
      accept[Ident]
      Mod.Inline()
    }
    autoPos(token match {
      case soft.KwInline() if ahead(token.is[KwIf]) =>
        ifClause(List(inlineMod()))
      case InlineMatchMod() =>
        inlineMatchClause(List(inlineMod()))
      case KwIf() =>
        ifClause()
      case KwTry() =>
        next()
        val body: Term = token match {
          case _ if dialect.allowTryWithAnyExpr => expr()
          case LeftParen() => inParensOrUnit(expr())
          case LeftBrace() | Indentation.Indent() => block()
          case _ => expr()
        }
        def caseClausesOrExpr = caseClausesIfAny().getOrElse(expr())
        val catchopt =
          if (tryAcceptWithOptLF[KwCatch]) Some {
            if (token.is[CaseIntro]) { accept[KwCase]; caseClause(true) }
            else if (token.is[Indentation.Indent]) indented(caseClausesOrExpr)
            else if (token.is[LeftBrace]) inBraces(caseClausesOrExpr)
            else expr()
          }
          else { None }

        val finallyopt =
          if (tryAcceptWithOptLF[KwFinally]) {
            Some(exprMaybeIndented())
          } else {
            None
          }

        catchopt match {
          case None => Term.Try(body, Nil, finallyopt)
          case Some(c: Case) => Term.Try(body, List(c), finallyopt)
          case Some(cases: List[_]) => Term.Try(body, cases.require[List[Case]], finallyopt)
          case Some(term: Term) => Term.TryWithHandler(body, term, finallyopt)
          case _ => unreachable(debug(catchopt))
        }
      case KwWhile() =>
        next()
        if (token.is[LeftParen]) {
          val cond = condExprInParens[KwDo]
          newLinesOpt()
          val body = if (token.is[KwDo]) {
            accept[KwDo]
            exprMaybeIndented()
          } else {
            in.observeIndented()
            exprMaybeIndented()
          }
          Term.While(cond, body)
        } else if (token.is[Indentation.Indent]) {
          val cond = block()
          acceptOpt[LF]
          accept[KwDo]
          Term.While(cond, exprMaybeIndented())
        } else {
          val cond = expr()
          acceptOpt[LF]
          accept[KwDo]
          Term.While(cond, exprMaybeIndented())
        }
      case KwDo() if dialect.allowDoWhile =>
        next()
        val body = expr()
        while (token.is[StatSep]) next()
        accept[KwWhile]
        val cond = condExpr()
        Term.Do(body, cond)
      case KwDo() =>
        syntaxError("do {...} while (...) syntax is no longer supported", at = token)
      case KwFor() =>
        next()
        val enums: List[Enumerator] =
          if (token.is[LeftBrace]) inBraces(enumerators())
          else if (token.is[LeftParen]) {
            val forked = in.fork
            Try(inParens(enumerators())) match {
              // Dotty retry in case of `for (a,b) <- list1.zip(list2) yield (a, b)`
              case Failure(_) if dialect.allowSignificantIndentation =>
                in = forked
                enumerators()
              case Failure(exception) => throw exception
              case Success(value) =>
                value
            }
          } else if (token.is[Indentation.Indent]) {
            indented(enumerators())
          } else {
            enumerators()
          }

        newLinesOpt()
        if (token.is[KwDo]) {
          accept[KwDo]
          Term.For(enums, exprMaybeIndented())
        } else if (token.is[KwYield]) {
          accept[KwYield]
          Term.ForYield(enums, exprMaybeIndented())
        } else {
          in.observeIndented()
          Term.For(enums, exprMaybeIndented())
        }
      case KwReturn() =>
        next()
        if (token.is[ExprIntro]) Term.Return(expr())
        else Term.Return(atPos(in.tokenPos, auto)(Lit.Unit()))
      case KwThrow() =>
        next()
        Term.Throw(expr())
      case KwImplicit() =>
        next()
        implicitClosure(location)
      case _ =>
        var t: Term = autoPos(postfixExpr(allowRepeated))
        def repeatedTerm(nextTokens: () => Unit) = {
          if (allowRepeated) t = atPos(t, auto)({ nextTokens(); Term.Repeated(t) })
          else syntaxError("repeated argument not allowed here", at = token)
        }
        if (token.is[Equals]) {
          t match {
            case _: Term.Ref | _: Term.Apply | _: Quasi =>
              next()
              t = atPos(t, auto)(Term.Assign(t, expr(location = NoStat, allowRepeated = true)))
            case _ =>
          }
        } else if (token.is[Colon]) {
          next()
          if (token.is[At] || (token.is[Ellipsis] && ahead(token.is[At]))) {
            t = atPos(t, auto)(Term.Annotate(t, annots(skipNewLines = false)))
          } else if (token.is[Underscore] && ahead(isStar)) {
            repeatedTerm(() => nextTwice())
          } else {
            // this does not necessarily correspond to syntax, but is necessary to accept lambdas
            // check out the `if (token.is[RightArrow]) { ... }` block below
            t = atPos(t, auto)(Term.Ascribe(t, typeOrInfixType(location)))
          }
        } else if (isVarargStarParam(allowRepeated)) {
          repeatedTerm(() => next())
        } else if (token.is[KwMatch]) {
          next()
          t = matchClause(t)
        }

        // Note the absense of `else if` here!!
        if (token.is[RightArrow] || token.is[ContextArrow]) {
          // This is a tricky one. In order to parse lambdas, we need to recognize token sequences
          // like `(...) => ...`, `id | _ => ...` and `implicit id | _ => ...`.
          //
          // If we exclude Implicit (which is parsed elsewhere anyway), then we can see that
          // these sequences are non-trivially ambiguous with tuples and self-type annotations
          // (i.e. are not resolvable with static lookahead).
          //
          // Therefore, when we encounter RightArrow, the part in parentheses is already parsed into a Term,
          // and we need to figure out whether that term represents what we expect from a lambda's param list
          // in order to disambiguate. The term that we have at hand might wildly differ from the param list that one would expect.
          // For example, when parsing `() => x`, we arrive at RightArrow having `Lit.Unit` as the parsed term.
          // That's why we later need `convertToParams` to make sense of what the parser has produced.
          //
          // Rules:
          // 1. `() => ...` means lambda
          // 2. `x => ...` means self-type annotation, but only in template position
          // 3. `(x) => ...` means self-type annotation, but only in template position
          // 4a. `x: Int => ...` means self-type annotation in template position
          // 4b. `x: Int => ...` means lambda in block position
          // 4c. `x: Int => ...` means ascription, i.e. `x: (Int => ...)`, in expression position
          // 5a.  `(x: Int) => ...` means lambda
          // 5b. `(using x: Int) => ...` means lambda for dotty
          // 6. `(x, y) => ...` or `(x: Int, y: Int) => ...` or with more entries means lambda
          //
          // A funny thing is that scalac's parser tries to disambiguate between self-type annotations and lambdas
          // even if it's not parsing the first statement in the template. E.g. `class C { foo; x => x }` will be
          // a parse error, because `x => x` will be deemed a self-type annotation, which ends up being inapplicable there.
          val looksLikeLambda = {
            val inParens =
              t.tokens.nonEmpty &&
                t.tokens.head.is[LeftParen] &&
                t.tokens.last.is[RightParen]
            object NameLike {
              def unapply(tree: Tree): Boolean = tree match {
                case Term.Quasi(0, _) => true
                case Term.Select(Term.Name(soft.KwUsing.name), _) if dialect.allowGivenUsing =>
                  true
                case Term.Ascribe(Term.Select(Term.Name(soft.KwUsing.name), _), _)
                    if dialect.allowGivenUsing =>
                  true
                case _: Term.Name => true
                case Term.Eta(Term.Name(soft.KwUsing.name)) if dialect.allowGivenUsing => true
                case _: Term.Placeholder => true
                case _ => false
              }
            }
            object ParamLike {
              def unapply(tree: Tree): Boolean = tree match {
                case Term.Quasi(0, _) => true
                case Term.Quasi(1, ParamLike()) => true
                case NameLike() => true
                case Term.Ascribe(Term.Quasi(0, _), _) => true
                case Term.Ascribe(NameLike(), _) => true
                case _ => false
              }
            }
            t match {
              case Lit.Unit() => true // 1
              case NameLike() => location != TemplateStat // 2-3
              case ParamLike() => inParens || location == BlockStat // 4-5
              case Term.Tuple(xs) => xs.forall(ParamLike.unapply) // 6
              case _ => false
            }
          }
          if (looksLikeLambda) {
            val contextFunction = token.is[ContextArrow]
            next()
            t = atPos(t, auto)({
              def convertToParam(tree: Term): Option[Term.Param] = tree match {
                case q: Quasi =>
                  Some(q.become[Term.Param.Quasi])
                case name: Term.Name =>
                  Some(atPos(tree, tree)(Term.Param(Nil, name, None, None)))
                case name: Term.Placeholder =>
                  Some(
                    atPos(tree, tree)(
                      Term.Param(Nil, atPos(name, name)(Name.Anonymous()), None, None)
                    )
                  )
                case Term.Ascribe(quasiName: Term.Quasi, tpt) =>
                  val name = quasiName.become[Term.Name.Quasi]
                  Some(atPos(tree, tree)(Term.Param(Nil, name, Some(tpt), None)))
                case Term.Ascribe(name: Term.Name, tpt) =>
                  Some(atPos(tree, tree)(Term.Param(Nil, name, Some(tpt), None)))
                case Term.Ascribe(name: Term.Placeholder, tpt) =>
                  Some(
                    atPos(tree, tree)(
                      Term.Param(Nil, atPos(name, name)(Name.Anonymous()), Some(tpt), None)
                    )
                  )
                case Term.Select(kwUsing @ Term.Name(soft.KwUsing.name), name) =>
                  Some(
                    atPos(tree, tree)(
                      Term.Param(List(atPos(kwUsing)(Mod.Using())), name, None, None)
                    )
                  )
                case Term.Ascribe(Term.Select(kwUsing @ Term.Name(soft.KwUsing.name), name), tpt) =>
                  Some(
                    atPos(tree, tree)(
                      Term.Param(List(atPos(kwUsing)(Mod.Using())), name, Some(tpt), None)
                    )
                  )
                case Term.Ascribe(eta @ Term.Eta(kwUsing @ Term.Name(soft.KwUsing.name)), tpt) =>
                  Some(
                    atPos(tree, tree)(
                      Term.Param(
                        List(atPos(kwUsing.endTokenPos, kwUsing.endTokenPos)(Mod.Using())),
                        atPos(eta.endTokenPos, eta.endTokenPos)(Name.Anonymous()),
                        Some(tpt),
                        None
                      )
                    )
                  )
                case Lit.Unit() =>
                  None
                case other =>
                  syntaxError(s"not a legal formal parameter", at = other)
              }
              def convertToParams(tree: Term): List[Term.Param] = tree match {
                case Term.Tuple(ts) => ts.toList flatMap convertToParam
                case _ => List(convertToParam(tree)).flatten
              }
              val params = convertToParams(t)
              val trm =
                if (location != BlockStat) expr()
                else {
                  blockExpr(isBlockOptional = true) match {
                    case partial: Term.PartialFunction if token.is[Colon] =>
                      accept[Colon]
                      atPos(partial, auto)(Term.Ascribe(partial, typeOrInfixType(location)))
                    case t => t
                  }
                }

              if (contextFunction)
                Term.ContextFunction(params, trm)
              else
                Term.Function(params, trm)
            })
          } else {
            // do nothing, which will either allow self-type annotation parsing to kick in
            // or will trigger an unexpected token error down the line
          }
        }
        t
    })
  }

  def implicitClosure(location: Location): Term.Function = {
    require(token.isNot[KwImplicit] && debug(token))
    val implicitPos = in.prevTokenPos
    val paramName = termName()
    val paramTpt = if (token.is[Colon]) {
      next(); Some(typeOrInfixType(location))
    } else None
    val param = atPos(implicitPos, auto)(
      Term.Param(List(atPos(implicitPos, implicitPos)(Mod.Implicit())), paramName, paramTpt, None)
    )
    accept[RightArrow]
    atPos(implicitPos, auto)(
      Term.Function(
        List(param),
        if (location != BlockStat) expr() else blockExpr(isBlockOptional = true)
      )
    )
  }

  // Encapsulates state and behavior of parsing infix syntax.
  // See `postfixExpr` for an involved usage example.
  // Another, much less involved usage, lives in `pattern3`.
  sealed abstract class InfixContext {
    // Lhs is the type of the left-hand side of an infix expression.
    // (Lhs, op and targs form UnfinishedInfix).
    // Rhs if the type of the right-hand side.
    // FinishedInfix is the type of an infix expression.
    // The conversions are necessary to push the output of finishInfixExpr on stack.
    type Lhs
    type Rhs
    // type UnfinishedInfix (see below)
    type FinishedInfix
    def toLhs(rhs: Rhs): Lhs
    def toRhs(fin: FinishedInfix): Rhs

    // Represents an unfinished infix expression, e.g. [a * b +] in `a * b + c`.
    // 1) T is either Term for infix syntax in expressions or Pat for infix syntax in patterns.
    // 2) We need to carry lhsStart/lhsEnd separately from lhs.pos
    //    because their extent may be bigger than lhs because of parentheses or whatnot.
    case class UnfinishedInfix(
        lhsStart: StartPos,
        lhs: Lhs,
        lhsEnd: EndPos,
        op: Term.Name,
        targs: List[Type]
    ) {
      def precedence = op.precedence
      override def toString = {
        val s_lhs = lhs match {
          case tree: Tree => tree.toString
          case List(tree) => tree.toString
          case List(trees @ _*) => "(" + trees.mkString(", ") + ")"
        }
        val s_targs = if (targs.nonEmpty) "[" + targs.mkString(", ") + "]" else ""
        s"[$s_lhs $op$s_targs]"
      }
    }

    // The stack of unfinished infix expressions, e.g. Stack([a + ]) in `a + b [*] c`.
    // `push` takes `b`, reads `*`, checks for type arguments and adds [b *] on the top of the stack.
    // Other methods working on the stack are self-explanatory.
    var stack: List[UnfinishedInfix] = Nil
    def head = stack.head
    def push(lhsStart: StartPos, lhs: Lhs, lhsEnd: EndPos, op: Term.Name, targs: List[Type]): Unit =
      stack ::= UnfinishedInfix(lhsStart, lhs, lhsEnd, op, targs)
    def pop(): UnfinishedInfix =
      try head
      finally stack = stack.tail

    def reduceStack(
        stack: List[UnfinishedInfix],
        curr: Rhs,
        currEnd: EndPos,
        op: Option[Term.Name]
    ): Lhs = {
      val opPrecedence = op.fold(0)(_.precedence)
      val leftAssoc = op.forall(_.isLeftAssoc)

      def isDone = this.stack == stack
      def lowerPrecedence = !isDone && (opPrecedence < this.head.precedence)
      def samePrecedence = !isDone && (opPrecedence == this.head.precedence)
      def canReduce = lowerPrecedence || leftAssoc && samePrecedence

      if (samePrecedence) {
        checkAssoc(this.head.op, leftAssoc)
      }

      // Pop off an unfinished infix expression off the stack and finish it with the rhs.
      // Then convert the result, so that it can become someone else's rhs.
      // Repeat while precedence and associativity allow.
      @tailrec
      def loop(rhs: Rhs): Lhs = {
        if (!canReduce) {
          toLhs(rhs)
        } else {
          val lhs = pop()
          val fin = finishInfixExpr(lhs, rhs, currEnd)
          val rhs1 = toRhs(fin)
          loop(rhs1)
        }
      }

      loop(curr)
    }

    // Takes the unfinished infix expression, e.g. `[x +]`,
    // then takes the right-hand side (which can have multiple args), e.g. ` (y, z)`,
    // and creates `x + (y, z)`.
    // We need to carry endPos explicitly because its extent may be bigger than rhs because of parent of whatnot.
    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Rhs, rhsEnd: EndPos): FinishedInfix
  }

  // Infix syntax in terms is borderline crazy.
  //
  // For example, did you know that `a * b + (c, d) * (f, g: _*)` means:
  // a.$times(b).$plus(scala.Tuple2(c, d).$times(f, g: _*))?!
  //
  // Therefore, Lhs = List[Term], Rhs = List[Term], FinishedInfix = Term.
  //
  // Actually there's even crazier stuff in scala-compiler.jar.
  // Apparently you can parse and typecheck `a + (bs: _*) * c`,
  // however I'm going to error out on this.
  object termInfixContext extends InfixContext {
    type Lhs = List[Term]
    type Rhs = List[Term]
    type FinishedInfix = Term

    def toRhs(fin: FinishedInfix): Rhs = List(fin)
    def toLhs(rhs: Rhs): Lhs = rhs

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Rhs, rhsEnd: EndPos): FinishedInfix = {
      val UnfinishedInfix(lhsStart, lhses, lhsEnd, op, targs) = unf
      // `a + (b, c) * d` leads to creation of a tuple!
      val lhs = atPos(lhsStart, lhsEnd)(makeTupleTerm(lhses))
      if (lhs.is[Term.Repeated])
        syntaxError("repeated argument not allowed here", at = lhs.tokens.last.prev)

      def infixExpression() =
        atPos(lhsStart, rhsEnd)(Term.ApplyInfix(lhs, op, targs, checkNoTripleDots(rhs)))
      op match {
        case _: Quasi =>
          infixExpression()
        case Term.Name("match") =>
          if (targs.nonEmpty) {
            syntaxError("no type parameters can be added for match", at = lhs.tokens.last.prev)
          }

          rhs.headOption match {
            case Some(Term.PartialFunction(cases)) =>
              atPos(lhsStart, rhsEnd)(Term.Match(lhs, cases))
            case _ =>
              syntaxError("match statement requires cases", at = lhs.tokens.last.prev)
          }
        case _ =>
          infixExpression()
      }
    }
  }

  // In comparison with terms, patterns are trivial.
  implicit object patInfixContext extends InfixContext {
    type Lhs = Pat
    type Rhs = Pat
    type FinishedInfix = Pat

    def toRhs(fin: FinishedInfix): Rhs = fin
    def toLhs(rhs: Rhs): Lhs = rhs

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Rhs, rhsEnd: EndPos): FinishedInfix = {
      val UnfinishedInfix(lhsStart, lhs, _, op, _) = unf
      val args = rhs match {
        case Pat.Tuple(args) => args.toList; case Lit.Unit() => Nil; case _ => List(rhs)
      }
      atPos(lhsStart, rhsEnd)(Pat.ExtractInfix(lhs, op, checkNoTripleDots(args)))
    }
  }

  def checkAssoc(op: Term.Name, leftAssoc: Boolean): Unit =
    if (op.isLeftAssoc != leftAssoc)
      syntaxError(
        "left- and right-associative operators with same precedence may not be mixed",
        at = op
      )

  def postfixExpr(allowRepeated: Boolean): Term = {
    val ctx = termInfixContext
    val base = ctx.stack

    // Skip to later in the `postfixExpr` method to start mental debugging.
    // rhsStartK/rhsEndK may be bigger than then extent of rhsK,
    // so we really have to track them separately.
    @tailrec
    def loop(rhsStartK: StartPos, rhsK: ctx.Rhs, rhsEndK: EndPos): ctx.Rhs = {
      if (!token.is[Ident] && !token.is[Unquote] &&
        !(token.is[KwMatch] && dialect.allowMatchAsOperator)) {
        // Infix chain has ended.
        // In the running example, we're at `a + b[]`
        // with base = List([a +]), rhsK = List([b]).
        rhsK
      } else if (isVarargStarParam(allowRepeated)) {
        rhsK
      } else {
        // Infix chain continues.
        // In the running example, we're at `a [+] b`.
        val op = if (token.is[KwMatch]) {
          val matchName = atPos(in.tokenPos, in.tokenPos)(Term.Name("match"))
          accept[KwMatch]
          matchName
        } else {
          termName() // op = [+]
        }
        val targs = if (token.is[LeftBracket]) exprTypeArgs() else Nil // targs = Nil

        // Check whether we're still infix or already postfix by testing the current token.
        // In the running example, we're at `a + [b]` (infix).
        // If we were parsing `val c = a b`, then we'd be at `val c = a b[]` (postfix).
        newLineOptWhenFollowing(_.is[ExprIntro])
        if (token.is[ExprIntro]) {
          // There is only one case when we here with empty rhsK: Unit inside infix chain.
          // For example `xs == () :: Nil`. In this case `()` treated as empty argument list but infix chain continues
          // and now we can argue that it was Unit.
          val rhsKWithFallback =
            if (rhsK.isEmpty) atPos(rhsStartK, rhsEndK)(Lit.Unit()) :: Nil else rhsK
          // Infix chain continues, so we need to reduce the stack.
          // In the running example, base = List(), rhsK = [a].
          val lhsK = ctx.reduceStack(base, rhsKWithFallback, rhsEndK, Some(op)) // lhsK = [a]
          val lhsStartK = Math.min(rhsStartK.startTokenPos, lhsK.head.startTokenPos)
          ctx.push(lhsStartK, lhsK, rhsEndK, op, targs) // afterwards, ctx.stack = List([a +])
          val preRhsKplus1 = in.token
          var rhsStartKplus1: StartPos = IndexPos(in.tokenPos)
          val rhsKplus1 = argumentExprsOrPrefixExpr()
          var rhsEndKplus1: EndPos = IndexPos(in.prevTokenPos)
          if (preRhsKplus1.isNot[LeftBrace] && preRhsKplus1.isNot[LeftParen]) {
            rhsStartKplus1 = TreePos(rhsKplus1.head)
            rhsEndKplus1 = TreePos(rhsKplus1.head)
          }
          // Try to continue the infix chain.
          loop(rhsStartKplus1, rhsKplus1, rhsEndKplus1) // base = List([a +]), rhsKplus1 = List([b])
        } else {
          // Infix chain has ended with a postfix expression.
          // This never happens in the running example.
          val lhsQual = ctx.reduceStack(base, rhsK, rhsEndK, Some(op))
          val finQual = lhsQual match {
            case List(finQual) => finQual; case _ => unreachable(debug(lhsQual))
          }
          if (targs.nonEmpty)
            syntaxError("type application is not allowed for postfix operators", at = token)
          ctx.toRhs(atPos(finQual, op)(Term.Select(finQual, op)))
        }
      }
    }

    // Start the infix chain.
    // We'll use `a + b` as our running example.
    val rhs0 = ctx.toRhs(prefixExpr(allowRepeated))

    // Iteratively read the infix chain via `loop`.
    // rhs0 is now [a]
    // If the next token is not an ident or an unquote, the infix chain ends immediately,
    // and `postfixExpr` becomes a fallthrough.
    val rhsN = loop(rhs0.head, rhs0, rhs0.head)

    // Infix chain has ended.
    // base contains pending UnfinishedInfix parts and rhsN is the final rhs.
    // For our running example, this'll be List([a +]) and [b].
    // Afterwards, `lhsResult` will be List([a + b]).
    val lhsResult = ctx.reduceStack(base, rhsN, in.prevTokenPos, None)

    // This is something not captured by the type system.
    // When applied to a result of `loop`, `reduceStack` will produce a singleton list.
    lhsResult match { case List(finResult) => finResult; case _ => unreachable(debug(lhsResult)) }
  }

  def prefixExpr(allowRepeated: Boolean): Term =
    if (!isUnaryOp) simpleExpr(allowRepeated)
    else {
      val op = termName()
      if (op.value == "-" && token.is[NumericLiteral])
        simpleExprRest(atPos(op, auto)(literal(isNegated = true)), canApply = true)
      else {
        simpleExpr0(allowRepeated = true) match {
          case Success(result) => atPos(op, auto)(Term.ApplyUnary(op, result))
          case Failure(_) =>
            // maybe it is not unary operator but simply an ident `trait - {...}`
            // we would fail here anyway, let's try to treat it as ident
            simpleExprRest(op, canApply = true)
        }
      }
    }

  def simpleExpr(allowRepeated: Boolean): Term = simpleExpr0(allowRepeated).get

  private def simpleExpr0(allowRepeated: Boolean): Try[Term] = autoPosTry {
    var canApply = true
    val t: Try[Term] = {
      token match {
        case MacroQuote() =>
          Success(macroQuote())
        case MacroSplice() =>
          Success(macroSplice())
        case MacroQuotedIdent() =>
          Success(macroQuotedIdent())
        case MacroSplicedIdent() =>
          Success(macroSplicedIdent())
        case Literal() =>
          Success(literal())
        case Interpolation.Id(_) =>
          Success(interpolateTerm())
        case Xml.Start() =>
          Success(xmlTerm())
        case Ident(_) | KwThis() | KwSuper() | Unquote() =>
          Success(path() match {
            case q: Quasi => q.become[Term.Quasi]
            case path => path
          })
        case Underscore() =>
          next()
          Success(atPos(in.prevTokenPos, in.prevTokenPos)(Term.Placeholder()))
        case LeftParen() =>
          Success(autoPos(inParensOrTupleOrUnit(location = NoStat, allowRepeated = allowRepeated)))
        case LeftBrace() =>
          canApply = false
          Success(blockExpr())
        case KwNew() =>
          canApply = false
          next()
          Success(atPos(in.prevTokenPos, auto) {
            template(enumCaseAllowed = false, secondaryConstructorAllowed = false) match {
              case trivial @ Template(Nil, List(init), Self(Name.Anonymous(), None), Nil) =>
                if (!token.prev.is[RightBrace]) Term.New(init)
                else Term.NewAnonymous(trivial)
              case other =>
                Term.NewAnonymous(other)
            }
          })
        case LeftBracket() if dialect.allowPolymorphicFunctions =>
          Success(polyFunction())
        case Indentation.Indent() =>
          Success(blockExpr())
        case _ =>
          Failure(new ParseException(token.pos, "illegal start of simple expression"))
      }
    }
    t.map(term => simpleExprRest(term, canApply = canApply))
  }

  def polyFunction() = autoPos {
    val quants = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    accept[RightArrow]
    val term = expr()
    Term.PolyFunction(quants, term)
  }

  def macroSplice(): Term = autoPos {
    accept[MacroSplice]
    QuoteSpliceContext.within {
      if (QuotedPatternContext.isInside())
        Term.SplicedMacroPat(autoPos(inBraces(pattern())))
      else
        Term.SplicedMacroExpr(autoPos(Term.Block(inBraces(blockStatSeq()))))

    }
  }

  def macroQuote(): Term = autoPos {
    accept[MacroQuote]
    QuoteSpliceContext.within {
      if (token.is[LeftBrace]) {
        Term.QuotedMacroExpr(autoPos(Term.Block(inBraces(blockStatSeq()))))
      } else if (token.is[LeftBracket]) {
        Term.QuotedMacroType(inBrackets(typ()))
      } else {
        syntaxError("Quotation only works for expressions and types", at = token)
      }
    }
  }

  def macroQuotedIdent(): Term = autoPos {
    token match {
      case Constant.Symbol(value) =>
        val name = atPos(in.tokenPos, in.tokenPos)(Term.Name(value.name))
        next()
        Term.QuotedMacroExpr(name)
      case _ =>
        syntaxError("Expected quoted ident", at = token)
    }
  }

  def macroSplicedIdent(): Term = autoPos {
    token match {
      case Ident(value) =>
        val name = atPos(in.tokenPos, in.tokenPos)(Term.Name(value.stripPrefix("$")))
        next()
        Term.SplicedMacroExpr(name)
      case _ =>
        syntaxError("Expected quoted ident", at = token)
    }
  }

  def simpleExprRest(t: Term, canApply: Boolean): Term = atPos(t, auto) {
    if (canApply) {
      if (dialect.allowSignificantIndentation) {
        newLineOptWhenFollowedBySignificantIndentationAnd(x => x.is[LeftBrace] || x.is[LeftParen])
      } else {
        newLineOptWhenFollowedBy[LeftBrace]
      }
    }
    token match {
      case Dot() =>
        next()
        if (dialect.allowMatchAsOperator && token.is[KwMatch]) {
          next()
          val clause = matchClause(t)
          // needed if match uses significant identation
          newLineOptWhenFollowedBy[Dot]
          val expr = simpleExprRest(clause, canApply = false)
          expr
        } else {
          simpleExprRest(selector(t), canApply = true)
        }
      case LeftBracket() =>
        t match {
          case _: Quasi | _: Term.Name | _: Term.Select | _: Term.Apply | _: Term.ApplyInfix |
              _: Term.ApplyUnary | _: Term.New | _: Term.Placeholder | _: Term.ApplyUsing |
              _: Term.Interpolate =>
            var app: Term = t
            while (token.is[LeftBracket]) app = atPos(t, auto)(Term.ApplyType(app, exprTypeArgs()))

            simpleExprRest(app, canApply = true)
          case _ =>
            t
        }
      case LeftParen() | LeftBrace() if canApply =>
        val arguments = atPos(t, auto) {
          argumentExprsWithUsing() match {
            case (args, true) => Term.ApplyUsing(t, args)
            case (args, false) => Term.Apply(t, args)
          }
        }
        simpleExprRest(arguments, canApply = true)
      case Underscore() =>
        next()
        Term.Eta(t)
      case _ =>
        t
    }
  }

  def argumentExprsOrPrefixExpr(): List[Term] = {
    if (token.isNot[LeftBrace] && token.isNot[LeftParen]) prefixExpr(allowRepeated = false) :: Nil
    else {
      def argsToTerm(args: List[Term], openParenPos: Int, closeParenPos: Int): Term = {
        def badRep(rep: Term.Repeated) = syntaxError("repeated argument not allowed here", at = rep)
        def loop(args: List[Term]): List[Term] = args match {
          case Nil => Nil
          case Term.Assign(_, rep: Term.Repeated) :: _ => badRep(rep)
          case (rep: Term.Repeated) :: _ => badRep(rep)
          case (t: Term) :: rest => t :: loop(rest)
          case _ => unreachable(debug(args))
        }
        atPos(openParenPos, closeParenPos)(makeTupleTerm(loop(args)))
      }
      val openParenPos = in.tokenPos
      val args = argumentExprs()
      val closeParenPos = in.prevTokenPos
      token match {
        case Dot() | LeftBracket() | LeftParen() | LeftBrace() | Underscore() =>
          simpleExprRest(argsToTerm(args, openParenPos, closeParenPos), canApply = true) :: Nil
        case _ =>
          args match {
            case arg :: Nil => atPos(openParenPos, closeParenPos)(arg) :: Nil
            case other => other
          }
      }
    }
  }

  def argumentExpr(): Term = {
    expr(location = NoStat, allowRepeated = true)
  }

  def argumentExprsWithUsing(): (List[Term], Boolean) = token match {
    case LeftBrace() =>
      (List(blockExpr()), false)
    case LeftParen() =>
      inParens(token match {
        case RightParen() =>
          (Nil, false)
        case tok: Ellipsis if tok.rank == 2 =>
          (List(ellipsis(2, astInfo[Term])), false)
        case _ =>
          val using = if (token.is[soft.KwUsing]) {
            next(); true
          } else false
          (commaSeparated(argumentExpr()), using)
      })
    case _ =>
      (Nil, false)
  }

  def argumentExprs(): List[Term] = argumentExprsWithUsing()._1

  private def checkNoTripleDots[T <: Tree](trees: List[T]): List[T] = {
    val illegalQuasis = trees.collect { case q: Quasi if q.rank == 2 => q }
    illegalQuasis.foreach(q => syntaxError(Messages.QuasiquoteRankMismatch(q.rank, 1), at = q))
    trees
  }

  def blockExpr(isBlockOptional: Boolean = false): Term = autoPos {
    if (ahead(token.is[CaseIntro] || (token.is[Ellipsis] && ahead(token.is[KwCase])))) {
      if (token.is[LeftBrace])
        Term.PartialFunction(inBraces(caseClauses()))
      else
        Term.PartialFunction(indented(caseClauses()))
    } else block(isBlockOptional)
  }

  def block(isBlockOptional: Boolean = false): Term = autoPos {
    def blockWithStats = Term.Block(blockStatSeq())
    if (!isBlockOptional && token.is[LeftBrace]) {
      inBraces(blockWithStats)
    } else {
      val possibleBlock =
        if (token.is[Indentation.Indent])
          indented(blockWithStats)
        else blockWithStats
      dropOuterBlock(possibleBlock)
    }
  }

  def caseClause(forceSingleExpr: Boolean = false): Case = atPos(StartPosPrev, EndPosPreOutdent) {
    if (token.isNot[KwCase]) {
      def caseBody() = {
        accept[RightArrow]
        val start = auto.startTokenPos
        def parseStatSeq() = blockStatSeq() match {
          case List(q: Quasi) => q.become[Term.Quasi]
          case List(term: Term) => term
          case other => atPos(start, auto)(Term.Block(other))
        }
        if (token.is[Indentation.Indent]) indented(parseStatSeq())
        else if (forceSingleExpr) expr(location = BlockStat, allowRepeated = false)
        else parseStatSeq()
      }
      Case(pattern(), guard(), caseBody())
    } else {
      syntaxError("Unexpected `case`", at = token.pos)
    }
  }

  def quasiquoteCase(): Case = entrypointCase()

  def entrypointCase(): Case = {
    accept[KwCase]
    caseClause()
  }

  def caseClauses(): List[Case] =
    caseClausesIfAny().getOrElse(syntaxErrorExpected[KwCase])

  def caseClausesIfAny(): Option[List[Case]] = {
    val cases = new ListBuffer[Case]
    while (token.is[CaseIntro] || token.is[Ellipsis]) {
      if (token.is[Ellipsis]) {
        cases += ellipsis(1, astInfo[Case], accept[KwCase])
        while (token.is[StatSep]) next()
      } else if (token.is[KwCase] && ahead(token.is[Unquote])) {
        next()
        cases += unquote[Case]
        while (token.is[StatSep]) next()
      } else {
        next()
        cases += caseClause()
      }
      if (token.is[StatSep] && ahead(token.is[CaseIntro])) acceptStatSep()
    }
    if (cases.isEmpty) None else Some(cases.toList)
  }

  def guard(): Option[Term] =
    if (token.is[KwIf]) {
      next(); Some(autoPos(postfixExpr(allowRepeated = false)))
    } else None

  def enumerators(): List[Enumerator] = {
    val enums = new ListBuffer[Enumerator]
    enums ++= enumerator(isFirst = true)
    while (token.is[StatSep] && !ahead(token.is[Indentation.Outdent] || token.is[KwDo])) {
      next()
      enums ++= enumerator(isFirst = false)
    }
    enums.toList
  }

  def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true): List[Enumerator] = {
    if (token.is[KwIf] && !isFirst) autoPos(Enumerator.Guard(guard().get)) :: Nil
    else if (token.is[Ellipsis]) {
      ellipsis(1, astInfo[Enumerator]) :: Nil
    } else if (token.is[Unquote] &&
      ahead(!token.is[Equals] && !token.is[LeftArrow])) { // support for q"for ($enum1; ..$enums; $enum2)"
      unquote[Enumerator] :: Nil
    } else generator(!isFirst, allowNestedIf)
  }

  def quasiquoteEnumerator(): Enumerator = entrypointEnumerator()

  def entrypointEnumerator(): Enumerator = {
    enumerator(isFirst = false, allowNestedIf = false) match {
      case enumerator :: Nil => enumerator
      case other => unreachable(debug(other))
    }
  }

  def generator(eqOK: Boolean, allowNestedIf: Boolean = true): List[Enumerator] = {
    val startPos = in.tokenPos
    val hasVal = token.is[KwVal]
    if (hasVal)
      next()

    val isCase =
      if (token.is[KwCase]) { next(); true }
      else false

    val pat = noSeq.pattern1()
    val hasEq = token.is[Equals]

    if (hasVal) {
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated", at = token)
      else syntaxError("val in for comprehension must be followed by assignment", at = token)
    }

    if (hasEq && eqOK) next()
    else accept[LeftArrow]
    val rhs = expr()

    val head = {
      if (hasEq) atPos(startPos, auto)(Enumerator.Val(pat, rhs))
      else if (isCase) atPos(startPos, auto)(Enumerator.CaseGenerator(pat, rhs))
      else atPos(startPos, auto)(Enumerator.Generator(pat, rhs))
    }
    val tail = {
      def loop(): List[Enumerator] = {
        if (token.isNot[KwIf]) Nil
        else autoPos(Enumerator.Guard(guard().get)) :: loop()
      }
      if (allowNestedIf) loop()
      else Nil
    }
    head :: tail
  }

  /* -------- PATTERNS ------------------------------------------- */

  /**
   * Methods which implicitly propagate whether the initial call took place in a context where
   * sequences are allowed. Formerly, this was threaded through methods as boolean seqOK.
   */
  trait SeqContextSensitive extends PatternContextSensitive {
    // is a sequence pattern _* allowed?
    def isSequenceOK: Boolean

    // are we in an XML pattern?
    def isXML: Boolean = false

    def patterns(): List[Pat] = commaSeparated(pattern())

    def pattern(): Pat = {
      def loop(pat: Pat): Pat =
        if (!isBar) pat
        else {
          next(); atPos(pat, auto)(Pat.Alternative(pat, pattern()))
        }
      loop(pattern1())
    }

    def quasiquotePattern(): Pat = {
      // NOTE: As per quasiquotes.md
      // * p"x" => Pat.Var (ok)
      // * p"X" => Pat.Var (needs postprocessing, parsed as Term.Name)
      // * p"`x`" => Term.Name (ok)
      // * p"`X`" => Term.Name (ok)
      val nonbqIdent = isIdent && !isBackquoted
      val pat = argumentPattern()
      pat match {
        case pat: Term.Name if nonbqIdent => atPos(pat, pat)(Pat.Var(pat))
        case _ => pat
      }
    }

    def entrypointPattern(): Pat = {
      pattern()
    }

    def unquotePattern(): Pat = {
      dropAnyBraces(pattern())
    }

    def unquoteXmlPattern(): Pat = {
      dropAnyBraces(pattern())
    }

    private def isLegitimateSeqWildcard = {
      def isUnderscore = token.is[Underscore]
      def isArglistEnd = token.is[RightParen] || token.is[RightBrace] || token.is[EOF]
      def tokensMatched = isUnderscore && ahead(isStar && ahead(isArglistEnd))
      isSequenceOK && tokensMatched
    }

    def pattern1(): Pat = autoPos {
      val p = pattern2()
      if (token.isNot[Colon]) p
      else {
        p match {
          case _: Quasi =>
            nextOnce()
            Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
          case p: Pat.Var
              if dialect.allowColonForExtractorVarargs && ahead(isLegitimateSeqWildcard) =>
            nextOnce()
            val seqWildcard = autoPos({ nextTwice(); Pat.SeqWildcard() })
            Pat.Bind(p, seqWildcard)
          case p: Pat.Var if !dialect.allowColonForExtractorVarargs && isColonWildcardStar =>
            syntaxError(s"$dialect does not support var: _*", at = p)
          case p: Pat.Var =>
            nextOnce()
            Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
          case _: Pat.Wildcard
              if dialect.allowColonForExtractorVarargs && ahead(isLegitimateSeqWildcard) =>
            nextThrice()
            Pat.SeqWildcard()
          case p: Pat.Wildcard =>
            nextOnce()
            Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
          case p: Pat =>
            if (dialect.allowAllTypedPatterns) {
              nextOnce()
              Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
            } else {
              p
            }
        }
      }
    }

    def pattern2(): Pat = autoPos {
      val p = pattern3()
      if (token.isNot[At])
        p
      else
        p match {
          case _: Quasi =>
            next()
            Pat.Bind(p, pattern3())
          case p: Term.Name =>
            syntaxError(
              "Pattern variables must start with a lower-case letter. (SLS 8.1.1.)",
              at = p
            )
          case p: Pat.Var if dialect.allowAtForExtractorVarargs && ahead(isLegitimateSeqWildcard) =>
            nextOnce()
            val seqWildcard = autoPos({ nextTwice(); Pat.SeqWildcard() })
            Pat.Bind(p, seqWildcard)
          case p: Pat.Var =>
            next()
            Pat.Bind(p, pattern3())
          case _: Pat.Wildcard
              if dialect.allowAtForExtractorVarargs && ahead(isLegitimateSeqWildcard) =>
            nextThrice()
            Pat.SeqWildcard()
          case _: Pat.Wildcard =>
            next()
            pattern3()
          case p =>
            p
        }
    }

    def pattern3(): Pat = {
      val ctx = patInfixContext
      val lhs = simplePattern(badPattern3)
      val base = ctx.stack
      @tailrec
      def loop(rhs: ctx.Rhs): ctx.Rhs = {
        val op =
          if (isIdentExcept("|") || token.is[Unquote]) Some(termName())
          else None
        val lhs1 = ctx.reduceStack(base, rhs, rhs, op)
        op match {
          case Some(op) =>
            if (token.is[LeftBracket])
              syntaxError("infix patterns cannot have type arguments", at = token)
            ctx.push(lhs1, lhs1, lhs1, op, Nil)
            val rhs1 = simplePattern(badPattern3)
            loop(rhs1)
          case None =>
            lhs1
        }
      }
      loop(lhs)
    }

    def badPattern3(token: Token): Nothing = {
      import patInfixContext._
      def isComma = token.is[Comma]
      def isDelimiter = token.is[RightParen] || token.is[RightBrace]
      def isCommaOrDelimiter = isComma || isDelimiter
      val (isUnderscore, isStar) = stack match {
        case UnfinishedInfix(_, Pat.Wildcard(), _, Term.Name("*"), _) :: _ => (true, true)
        case UnfinishedInfix(_, _, _, Term.Name("*"), _) :: _ => (false, true)
        case _ => (false, false)
      }
      val preamble = "bad simple pattern:"
      val subtext = (isUnderscore, isStar, isSequenceOK) match {
        case (true, true, true) if isComma =>
          "bad use of _* (a sequence pattern must be the last pattern)"
        case (true, true, true) if isDelimiter => "bad brace or paren after _*"
        case (true, true, false) if isDelimiter => "bad use of _* (sequence pattern not allowed)"
        case (false, true, true) if isDelimiter => "use _* to match a sequence"
        case (false, true, _) if isCommaOrDelimiter => "trailing * is not a valid pattern"
        case _ => null
      }
      val msg = if (subtext != null) s"$preamble $subtext" else "illegal start of simple pattern"
      syntaxError(msg, at = token)
    }

    def simplePattern(): Pat =
      // simple diagnostics for this entry point
      simplePattern(token => syntaxError("illegal start of simple pattern", at = token))
    def simplePattern(onError: Token => Nothing): Pat =
      autoPos(token match {
        case Ident(_) | KwThis() | Unquote() =>
          val isBackquoted = parser.isBackquoted
          val sid = stableId()
          val isRepeated = sid match {
            case _: Quasi => false
            case _: Term.Name =>
              dialect.allowPostfixStarVarargSplices && isStar && token.next.isNot[Ident]
            case _ => false
          }
          val isVarPattern = sid match {
            case _: Quasi => false
            case Term.Name(value) =>
              val isNextTokenBinding = token.is[At]
              val isCapitalAllowed = dialect.allowUpperCasePatternVarBinding && isNextTokenBinding
              !isBackquoted && ((value.head.isLower && value.head.isLetter) || value.head == '_' || isCapitalAllowed)
            case _ => false
          }
          if (token.is[NumericLiteral]) {
            sid match {
              case Term.Name("-") =>
                return atPos(sid, auto)(literal(isNegated = true))
              case _ =>
            }
          }
          val targs = token match {
            case LeftBracket() => patternTypeArgs()
            case _ => Nil
          }
          (token, sid) match {
            case (LeftParen(), _) =>
              val ref = sid match {
                case q: Quasi => q.become[Term.Quasi]
                case other => other
              }
              val fun = if (targs.nonEmpty) atPos(sid, auto)(Term.ApplyType(ref, targs)) else ref
              Pat.Extract(fun, checkNoTripleDots(argumentPatterns()))
            case (_, _) if targs.nonEmpty => syntaxError("pattern must be a value", at = token)
            case (_, name: Term.Name.Quasi) => name.become[Pat.Quasi]
            case (_, name: Term.Name) if isRepeated => accept[Ident]; Pat.Repeated(name)
            case (_, name: Term.Name) if isVarPattern => Pat.Var(name)
            case (_, name: Term.Name) => name
            case (_, select: Term.Select) => select
            case _ => unreachable(debug(token, token.structure, sid, sid.structure))
          }
        case Underscore() if isLegitimateSeqWildcard =>
          nextTwice()
          Pat.SeqWildcard()
        case Underscore() =>
          next()
          Pat.Wildcard()
        case Literal() =>
          literal()
        case Interpolation.Id(_) =>
          interpolatePat()
        case Xml.Start() =>
          xmlPat()
        case LeftParen() =>
          val patterns = inParens(if (token.is[RightParen]) Nil else noSeq.patterns())
          makeTuple[Pat](patterns, () => Lit.Unit(), Pat.Tuple(_))
        case MacroQuote() =>
          QuotedPatternContext.within {
            Pat.Macro(macroQuote())
          }
        case MacroQuotedIdent() =>
          Pat.Macro(macroQuotedIdent())
        case KwGiven() =>
          accept[KwGiven]
          Pat.Given(patternTyp(allowInfix = false, allowImmediateTypevars = false))
        case _ =>
          onError(token)
      })
  }

  /** The implementation of the context sensitive methods for parsing outside of patterns. */
  object outPattern extends PatternContextSensitive {}

  /** The implementation for parsing inside of patterns at points where sequences are allowed. */
  object seqOK extends SeqContextSensitive {
    val isSequenceOK = true
  }

  /** The implementation for parsing inside of patterns at points where sequences are disallowed. */
  object noSeq extends SeqContextSensitive {
    val isSequenceOK = false
  }

  /** For use from xml pattern, where sequence is allowed and encouraged. */
  object xmlSeqOK extends SeqContextSensitive {
    val isSequenceOK = true
    override val isXML = true
  }

  /**
   * These are default entry points into the pattern context sensitive methods: they are all
   * initiated from non-pattern context.
   */
  def typ() = outPattern.typ()
  def quasiquoteType() = outPattern.quasiquoteType()
  def entrypointType() = outPattern.entrypointType()
  def startInfixType() = outPattern.infixType(InfixMode.FirstOp)
  def startModType() = outPattern.annotType()
  def exprTypeArgs() = outPattern.typeArgs()
  def exprSimpleType() = outPattern.simpleType()

  /** Default entry points into some pattern contexts. */
  def pattern(): Pat = noSeq.pattern()
  def quasiquotePattern(): Pat = seqOK.quasiquotePattern()
  def entrypointPattern(): Pat = seqOK.entrypointPattern()
  def unquotePattern(): Pat = noSeq.unquotePattern()
  def unquoteXmlPattern(): Pat = xmlSeqOK.unquoteXmlPattern()
  def seqPatterns(): List[Pat] = seqOK.patterns()
  def xmlSeqPatterns(): List[Pat] = xmlSeqOK.patterns() // Called from xml parser
  def argumentPattern(): Pat = seqOK.pattern()
  def argumentPatterns(): List[Pat] = inParens {
    if (token.is[RightParen]) Nil
    else seqPatterns()
  }
  def xmlLiteralPattern(): Pat = syntaxError("XML literals are not supported", at = in.token)
  def patternTyp() = noSeq.patternTyp(allowInfix = true, allowImmediateTypevars = false)
  def patternTypeArgs() = noSeq.patternTypeArgs()

  /* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  def accessModifier(): Mod = autoPos {
    val mod = token match {
      case KwPrivate() => (ref: Ref) => Mod.Private(ref)
      case KwProtected() => (ref: Ref) => Mod.Protected(ref)
      case other => unreachable(debug(other, other.structure))
    }
    next()
    if (in.token.isNot[LeftBracket]) mod(autoPos(Name.Anonymous()))
    else {
      accept[LeftBracket]
      val result = {
        if (token.is[KwThis]) {
          val qual = autoPos { next(); Name.Anonymous() }
          mod(atPos(in.prevTokenPos, auto)(Term.This(qual)))
        } else {
          val within = termName() match {
            case q: Quasi => q.become[Ref.Quasi]
            case name => atPos(name, name)(Name.Indeterminate(name.value))
          }
          mod(within)
        }
      }
      accept[RightBracket]
      result
    }
  }

  def modifier(): Mod =
    autoPos(token match {
      case Unquote() => unquote[Mod]
      case Ellipsis(_) => ellipsis(1, astInfo[Mod])
      case KwAbstract() => next(); Mod.Abstract()
      case KwFinal() => next(); Mod.Final()
      case KwSealed() => next(); Mod.Sealed()
      case KwImplicit() => next(); Mod.Implicit()
      case KwLazy() => next(); Mod.Lazy()
      case KwOverride() => next(); Mod.Override()
      case KwPrivate() => accessModifier()
      case KwProtected() => accessModifier()
      case soft.KwInline() => next(); Mod.Inline()
      case soft.KwInfix() => next(); Mod.Infix()
      case soft.KwOpen() => next(); Mod.Open()
      case soft.KwOpaque() => next(); Mod.Opaque()
      case soft.KwTransparent() => next(); Mod.Transparent()
      case _ => syntaxError(s"modifier expected but ${token.name} found", at = token)
    })

  def quasiquoteModifier(): Mod = entrypointModifier()

  def entrypointModifier(): Mod = autoPos {
    def annot() = annots(skipNewLines = false) match {
      case annot :: Nil => annot
      case _ :: other :: _ =>
        syntaxError(s"end of file expected but ${token.name} found", at = other)
      case Nil => unreachable
    }
    def fail() = syntaxError(s"modifier expected but ${parser.token.name} found", at = parser.token)
    token match {
      case Unquote() => unquote[Mod]
      case At() => annot()
      case KwPrivate() => accessModifier()
      case KwProtected() => accessModifier()
      case KwImplicit() => next(); Mod.Implicit()
      case KwFinal() => next(); Mod.Final()
      case KwSealed() => next(); Mod.Sealed()
      case KwOverride() => next(); Mod.Override()
      case KwCase() => next(); Mod.Case()
      case KwAbstract() => next(); Mod.Abstract()
      case Ident("+") => next(); Mod.Covariant()
      case Ident("-") => next(); Mod.Contravariant()
      case KwLazy() => next(); Mod.Lazy()
      case KwVal() if !dialect.allowUnquotes => next(); Mod.ValParam()
      case KwVar() if !dialect.allowUnquotes => next(); Mod.VarParam()
      case soft.KwOpen() => next(); Mod.Open()
      case soft.KwTransparent() => next(); Mod.Transparent()
      case soft.KwInline() => next(); Mod.Inline()
      case soft.KwInfix() => next(); Mod.Infix()
      case Ident("valparam") if dialect.allowUnquotes => next(); Mod.ValParam()
      case Ident("varparam") if dialect.allowUnquotes => next(); Mod.VarParam()
      case _ => fail()
    }
  }

  def ctorModifiers(): Option[Mod] = token match {
    case Unquote() if ahead(token.is[LeftParen]) => Some(unquote[Mod])
    case Ellipsis(_) => Some(ellipsis(1, astInfo[Mod]))
    case KwPrivate() => Some(accessModifier())
    case KwProtected() => Some(accessModifier())
    case _ => None
  }

  def tparamModifiers(): Option[Mod] = {
    if (token.is[Unquote] && !ahead(token.is[Ident] || token.is[Unquote])) None
    else
      token match {
        case Unquote() => Some(unquote[Mod])
        case Ellipsis(_) => Some(ellipsis(1, astInfo[Mod]))
        case Ident("+") => Some(autoPos({ next(); Mod.Covariant() }))
        case Ident("-") => Some(autoPos({ next(); Mod.Contravariant() }))
        case _ => None
      }
  }

  def modifiers(
      isLocal: Boolean = false,
      isParams: Boolean = false
  ): List[Mod] = {
    def appendMod(mods: List[Mod], mod: Mod): List[Mod] = {
      def validate() = {
        if (isLocal && !mod.tokens.head.is[LocalModifier]) {
          syntaxError("illegal modifier for a local definition", at = mod)
        }
        if (!mod.is[Mod.Quasi]) {
          if (mods.exists(_.productPrefix == mod.productPrefix)) {
            syntaxError("repeated modifier", at = mod)
          }
          if (mods.exists(_.isNakedAccessMod) && mod.isNakedAccessMod) {
            if (mod.is[Mod.Protected])
              rejectModCombination[Mod.Private, Mod.Protected](mods :+ mod)
            if (mod.is[Mod.Private])
              rejectModCombination[Mod.Protected, Mod.Private](mods :+ mod)
          }
          if (mods.exists(_.isQualifiedAccessMod) && mod.isQualifiedAccessMod) {
            syntaxError("duplicate private/protected qualifier", at = mod)
          }
        }
      }
      validate()
      mods :+ mod
    }
    // the only things that can come after $mod or $mods are either keywords or names; the former is easy,
    // but in the case of the latter, we need to take care to not hastily parse those names as modifiers
    def continueLoop =
      ahead(
        token.is[Colon] || token.is[Equals] || token.is[EOF] || token.is[LeftBracket] ||
          token.is[Subtype] || token.is[Supertype] || token.is[Viewbound]
      )
    @tailrec
    def loop(mods: List[Mod]): List[Mod] = token match {
      case _ if isParams && token.is[NonParamsModifier] => mods
      case Unquote() => if (continueLoop) mods else loop(appendMod(mods, modifier()))
      case Ellipsis(_) => loop(appendMod(mods, modifier()))
      case Modifier() => loop(appendMod(mods, modifier()))
      case LF() if !isLocal => next(); loop(mods)
      case _ => mods
    }
    loop(Nil)
  }

  def localModifiers(): List[Mod] = modifiers(isLocal = true)

  def annots(skipNewLines: Boolean, allowArgss: Boolean = true): List[Mod.Annot] = {
    val annots = new ListBuffer[Mod.Annot]
    while (token.is[At] || (token.is[Ellipsis] && ahead(token.is[At]))) {
      if (token.is[Ellipsis]) {
        annots += ellipsis(1, astInfo[Mod.Annot], accept[At])
      } else {
        next()
        if (token.is[Unquote]) annots += unquote[Mod.Annot]
        else annots += atPos(in.prevTokenPos, auto)(Mod.Annot(initInsideAnnotation(allowArgss)))
      }
      if (skipNewLines) newLineOpt()
    }
    annots.toList
  }

  def constructorAnnots(): List[Mod.Annot] = annots(skipNewLines = false, allowArgss = false)

  /* -------- PARAMETERS ------------------------------------------- */

  def paramClauses(ownerIsType: Boolean, ownerIsCase: Boolean = false): List[List[Term.Param]] = {
    var parsedImplicits = false
    def paramClause(first: Boolean): List[Term.Param] = token match {
      case RightParen() =>
        Nil
      case tok: Ellipsis if tok.rank == 2 =>
        List(ellipsis(2, astInfo[Term.Param]))
      case _ =>
        var parsedUsing = false
        if (token.is[KwImplicit]) {
          next()
          parsedImplicits = true
        } else if (token.is[soft.KwUsing]) {
          next()
          parsedUsing = true
        }
        commaSeparated(
          param(
            ownerIsCase && first,
            ownerIsType,
            isImplicit = parsedImplicits,
            isUsing = parsedUsing
          )
        )
    }
    var first = true
    val paramss = new ListBuffer[List[Term.Param]]
    newLineOptWhenFollowedBy[LeftParen]
    while (!parsedImplicits && token.is[LeftParen]) {
      next()
      paramss += paramClause(first)
      accept[RightParen]
      newLineOptWhenFollowedBy[LeftParen]
      first = false
    }
    paramss.toList
  }

  def paramType(): Type =
    autoPos(token match {
      case RightArrow() =>
        val t = autoPos {
          next()
          Type.ByName(typ())
        }
        if (isStar && dialect.allowByNameRepeatedParameters) {
          autoPos {
            next()
            Type.Repeated(t)
          }
        } else {
          t
        }
      case _ =>
        val t = typ()
        if (!isStar) t
        else {
          autoPos {
            next()
            Type.Repeated(t)
          }
        }
    })

  def param(
      ownerIsCase: Boolean,
      ownerIsType: Boolean,
      isImplicit: Boolean,
      isUsing: Boolean
  ): Term.Param = autoPos {
    var mods: List[Mod] = annots(skipNewLines = false)
    if (isImplicit) mods ++= List(atPos(in.tokenPos, in.prevTokenPos)(Mod.Implicit()))
    if (isUsing) mods ++= List(atPos(in.tokenPos, in.prevTokenPos)(Mod.Using()))
    rejectMod[Mod.Open](mods, "Open modifier only applied to classes")
    if (ownerIsType) {
      mods ++= modifiers(isParams = true)
      rejectMod[Mod.Lazy](
        mods,
        "lazy modifier not allowed here. Use call-by-name parameters instead."
      )
      rejectMod[Mod.Sealed](mods, "`sealed' modifier can be used only for classes")
      if (!mods.has[Mod.Override])
        rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    } else {
      if (token.is[soft.KwInline] && ahead(token.is[Ident])) {
        mods ++= List(modifier())
      }
    }

    val (isValParam, isVarParam) = (ownerIsType && token.is[KwVal], ownerIsType && token.is[KwVar])
    if (isValParam) {
      mods :+= atPos(in.tokenPos, in.tokenPos)(Mod.ValParam()); next()
    }
    if (isVarParam) {
      mods :+= atPos(in.tokenPos, in.tokenPos)(Mod.VarParam()); next()
    }
    def endParamQuasi = token.is[RightParen] || token.is[Comma]
    mods.headOption match {
      case Some(q: Mod.Quasi) if endParamQuasi =>
        q.become[Term.Param.Quasi]
      case _ =>
        var anonymousUsing = false
        val name = if (isUsing && ahead(!token.is[Colon])) { // anonymous using
          anonymousUsing = true
          autoPos(Name.Anonymous())
        } else {
          termName() match {
            case q: Quasi => q.become[Name.Quasi]
            case other => other
          }
        }
        name match {
          case q: Quasi if endParamQuasi =>
            q.become[Term.Param.Quasi]
          case _ =>
            val tpt =
              if (token.isNot[Colon] && name.is[Name.Quasi])
                None
              else {
                if (!anonymousUsing) accept[Colon]
                val tpt = paramType()
                if (tpt.is[Type.ByName]) {
                  def mayNotBeByName(subj: String) =
                    syntaxError(s"$subj parameters may not be call-by-name", at = name)
                  val isLocalToThis: Boolean = {
                    val isExplicitlyLocal = mods.exists {
                      case Mod.Private(_: Term.This) => true; case _ => false
                    }
                    if (ownerIsCase) isExplicitlyLocal
                    else isExplicitlyLocal || (!isValParam && !isVarParam)
                  }
                  if (ownerIsType && !isLocalToThis) {
                    if (isVarParam)
                      mayNotBeByName("`var'")
                    else
                      mayNotBeByName("`val'")
                  } else if (isImplicit && !dialect.allowImplicitByNameParameters)
                    mayNotBeByName("implicit")
                }
                Some(tpt)
              }
            val default =
              if (token.isNot[Equals]) None
              else {
                next()
                Some(expr())
              }
            Term.Param(mods, name, tpt, default)
        }
    }
  }

  def quasiquoteTermParam(): Term.Param = entrypointTermParam()

  def entrypointTermParam(): Term.Param =
    param(ownerIsCase = false, ownerIsType = true, isImplicit = false, isUsing = false)

  def typeParamClauseOpt(
      ownerIsType: Boolean,
      ctxBoundsAllowed: Boolean,
      allowUnderscore: Boolean = true
  ): List[Type.Param] = {
    newLineOptWhenFollowedBy[LeftBracket]
    if (token.isNot[LeftBracket]) Nil
    else inBrackets(commaSeparated(typeParam(ownerIsType, ctxBoundsAllowed, allowUnderscore)))
  }

  def typeParam(
      ownerIsType: Boolean,
      ctxBoundsAllowed: Boolean,
      allowUnderscore: Boolean = true
  ): Type.Param = autoPos {
    var mods: List[Mod] = annots(skipNewLines = false)
    if (ownerIsType) mods ++= tparamModifiers()
    def endTparamQuasi = token.is[RightBracket] || token.is[Comma]
    mods.headOption match {
      case Some(q: Mod.Quasi) if endTparamQuasi =>
        q.become[Type.Param.Quasi]
      case _ =>
        val name =
          if (token.is[Ident]) typeName()
          else if (token.is[Unquote]) unquote[Name]
          else if (token.is[Underscore] && allowUnderscore) {
            autoPos { next(); Name.Anonymous() }
          } else {
            if (allowUnderscore) syntaxError("identifier or `_' expected", at = token)
            else syntaxError("identifier expected", at = token)
          }
        name match {
          case q: Quasi if endTparamQuasi =>
            q.become[Type.Param.Quasi]
          case _ =>
            val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
            val tbounds = typeBounds()
            val vbounds = new ListBuffer[Type]
            val cbounds = new ListBuffer[Type]
            if (ctxBoundsAllowed) {
              while (token.is[Viewbound]) {
                if (!dialect.allowViewBounds) {
                  val msg = "Use an implicit parameter instead.\n" +
                    "Example: Instead of `def f[A <% Int](a: A)` " +
                    "use `def f[A](a: A)(implicit ev: A => Int)`."
                  syntaxError(s"View bounds are not supported. $msg", at = token)
                }
                next()
                if (token.is[Ellipsis]) vbounds += ellipsis(1, astInfo[Type])
                else vbounds += typ()
              }
              while (token.is[Colon]) {
                next()
                if (token.is[Ellipsis]) cbounds += ellipsis(1, astInfo[Type])
                else cbounds += typ()
              }
            }
            Type.Param(mods, name, tparams, tbounds, vbounds.toList, cbounds.toList)
        }
    }
  }

  def quasiquoteTypeParam(): Type.Param = entrypointTypeParam()

  def entrypointTypeParam(): Type.Param = typeParam(ownerIsType = true, ctxBoundsAllowed = true)

  def typeBounds() =
    autoPos(Type.Bounds(bound[Supertype], bound[Subtype]))

  def bound[T <: Token: TokenInfo]: Option[Type] =
    if (token.is[T]) {
      next(); Some(typ())
    } else None

  /* -------- DEFS ------------------------------------------- */

  def exportStmt(): Stat = autoPos {
    accept[KwExport]
    Export(commaSeparated(importer()))
  }

  def importStmt(): Import = autoPos {
    accept[KwImport]
    Import(commaSeparated(importer()))
  }

  def isStarWildcard: Boolean = isStarWildcard(token)
  def isStarWildcard(token: Token): Boolean = dialect.allowStarWildcardImport && token.syntax == "*"

  def importer(): Importer = autoPos {
    val sid = stableId() match {
      case q: Quasi => q.become[Term.Ref.Quasi]
      case sid @ Term.Select(q: Quasi, name) =>
        atPos(sid, sid)(Term.Select(q.become[Term.Ref.Quasi], name))
      case path => path
    }
    def dotselectors = { accept[Dot]; Importer(sid, importees()) }
    def name(tn: Term.Name) = atPos(tn, tn)(Name.Indeterminate(tn.value))
    sid match {
      case Term.Select(sid: Term.Ref, tn: Term.Name) if sid.isStableId =>
        if (token.is[Dot]) dotselectors
        else if (token.is[soft.KwAs]) {
          next()
          Importer(sid, importeeRename(name(tn)) :: Nil)
        } else if (isStarWildcard(tn.tokens.head)) {
          Importer(sid, atPos(tn, tn)(Importee.Wildcard()) :: Nil)
        } else {
          Importer(sid, atPos(tn, tn)(Importee.Name(name(tn))) :: Nil)
        }
      case tn: Term.Name if token.is[soft.KwAs] =>
        next()
        Importer(autoPos(Term.Anonymous()), importeeRename(name(tn)) :: Nil)
      case _ =>
        dotselectors
    }
  }

  def quasiquoteImporter(): Importer = entrypointImporter()

  def entrypointImporter(): Importer = importer()

  def importees(): List[Importee] =
    if (token.isNot[LeftBrace]) {
      List(importWildcardOrName())
    } else {
      val importees = inBraces(commaSeparated(importee()))

      def lastIsGiven = importees.last.is[Importee.Given] || importees.last.is[Importee.GivenAll]
      val imp =
        if (importees.nonEmpty && lastIsGiven) importees.init else importees
      if (imp.nonEmpty) {
        imp.init.foreach {
          case importee: Importee.Wildcard =>
            syntaxError("Wildcard import must be in the last position", importee.pos)

          case _ => ()
        }
      }
      def importeesHaveWildcard = importees.exists {
        case Importee.Wildcard() => true
        case _ => false
      }
      if (dialect.allowGivenImports)
        importees.map {
          case importee @ Importee.Name(nm) if nm.value == "given" && importeesHaveWildcard =>
            atPos(importee.name, importee.name)(Importee.GivenAll())
          case i => i
        }
      else importees
    }

  def importWildcardOrName(): Importee = autoPos {
    if (token.is[Underscore] || isStarWildcard) {
      next(); Importee.Wildcard()
    } else if (token.is[KwGiven]) {
      next()
      if (token.is[Ident])
        Importee.Given(typ())
      else Importee.GivenAll()
    } else if (token.is[Unquote]) Importee.Name(unquote[Name.Quasi])
    else {
      val name = termName(); Importee.Name(atPos(name, name)(Name.Indeterminate(name.value)))
    }
  }

  def importeeRename(from: Name) = atPos(from, auto) {
    importWildcardOrName() match {
      case to: Importee.Name => Importee.Rename(from, to.name)
      case _: Importee.Wildcard => Importee.Unimport(from)
      case other => unreachable(debug(other, other.structure))
    }
  }

  def importee(): Importee = autoPos {
    importWildcardOrName() match {
      case from: Importee.Name if token.is[RightArrow] || token.is[soft.KwAs] =>
        next()
        importeeRename(from.name)
      // NOTE: this is completely nuts
      case from: Importee.Wildcard
          if (token.is[RightArrow] || token.is[soft.KwAs]) &&
            ahead(token.is[Underscore] || isStarWildcard) =>
        nextTwice()
        from
      case other =>
        other
    }
  }

  def quasiquoteImportee(): Importee = entrypointImportee()

  def entrypointImportee(): Importee = importee()

  def nonLocalDefOrDcl(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): Stat = {
    val anns = annots(skipNewLines = true)
    val mods = anns ++ modifiers()
    defOrDclOrSecondaryCtor(mods, enumCaseAllowed, secondaryConstructorAllowed) match {
      case s if s.isTemplateStat => s
      case other => syntaxError("is not a valid template statement", at = other)
    }
  }

  def defOrDclOrSecondaryCtor(
      mods: List[Mod],
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): Stat = {
    onlyAcceptMod[Mod.Lazy, KwVal](mods, "lazy not allowed here. Only vals can be lazy")
    onlyAcceptMod[Mod.Opaque, KwType](mods, "opaque not allowed here. Only types can be opaque.")
    token match {
      case KwVal() | KwVar() =>
        patDefOrDcl(mods)
      case KwGiven() =>
        givenDecl(mods)
      case KwDef() =>
        if (!secondaryConstructorAllowed && ahead(token.is[KwThis]))
          syntaxError("Illegal secondary constructor", at = token.pos)
        funDefOrDclOrExtensionOrSecondaryCtor(mods)
      case KwType() =>
        typeDefOrDcl(mods)
      case KwExtension() =>
        extensionGroupDecl(mods)
      case KwCase() if dialect.allowEnums && enumCaseAllowed && ahead(token.is[Ident]) =>
        enumCaseDef(mods)
      case KwCase() if dialect.allowEnums && ahead(token.is[Ident]) =>
        syntaxError("Enum cases are only allowed in enums", at = token.pos)
      case KwIf() if mods.size == 1 && mods.head.is[Mod.Inline] =>
        ifClause(mods)
      case ExprIntro() if mods.size == 1 && mods.head.is[Mod.Inline] =>
        inlineMatchClause(mods)
      case _ =>
        tmplDef(mods)
    }
  }

  def endMarker(): Stat = autoPos {
    assert(token.text == "end")
    next()
    if (token.is[Ident]) {
      EndMarker(termName())
    } else {
      val r = EndMarker(atPos(token)(Term.Name(token.text)))
      next()
      r
    }
  }

  def patDefOrDcl(mods: List[Mod]): Stat = atPos(mods, auto) {
    val isMutable = token.is[KwVar]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    if (!mods.has[Mod.Override])
      rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    next()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2()).map {
      case name: Term.Name => atPos(name, name)(Pat.Var(name))
      case pat => pat
    }
    val tp: Option[Type] = typedOpt()

    if (tp.isEmpty || token.is[Equals]) {
      accept[Equals]
      val rhsExpr = exprMaybeIndented()
      val rhs =
        if (rhsExpr.is[Term.Placeholder]) {
          if (tp.nonEmpty && isMutable && lhs.forall(_.is[Pat.Var]))
            None
          else
            syntaxError("unbound placeholder parameter", at = token)
        } else Some(rhsExpr)

      if (isMutable) Defn.Var(mods, lhs, tp, rhs)
      else Defn.Val(mods, lhs, tp, rhs.get)
    } else {
      if (!isMutable && !dialect.allowLazyValAbstractValues)
        rejectMod[Mod.Lazy](mods, "lazy values may not be abstract")
      val ids = lhs.map {
        case q: Quasi => q
        case name: Pat.Var => name
        case other => syntaxError("pattern definition may not be abstract", at = other)
      }

      if (isMutable) Decl.Var(mods, ids, tp.get)
      else Decl.Val(mods, ids, tp.get)
    }
  }

  /**
   * ```scala
   * Given             ::= 'given' GivenDef
   * GivenDef          ::=  [GivenSig] (Type [‘=’ Expr] | StructuralInstance)
   * GivenSig          ::=  [id] [DefTypeParamClause] {UsingParamClause} ‘:’
   * StructuralInstance ::=  ConstrApp {‘with’ ConstrApp} ‘with’ TemplateBody
   * ```
   */
  private def givenDecl(mods: List[Mod]): Stat = atPos(mods, auto) {
    accept[KwGiven]
    val anonymousName = autoPos(scala.meta.Name.Anonymous())

    val forked = in.fork
    val (sigName, sigTparams, sigUparamss) =
      try {
        val name: meta.Name =
          if (token.is[Ident]) termName() else anonymousName
        val tparams = typeParamClauseOpt(
          ownerIsType = false,
          ctxBoundsAllowed = true,
          allowUnderscore = dialect.allowTypeParamUnderscore
        )
        val uparamss =
          if (token.is[LeftParen] && ahead(token.is[soft.KwUsing]))
            paramClauses(ownerIsType = false)
          else Nil
        if (token.is[Colon]) {
          accept[Colon]
          (name, tparams, uparamss)
        } else {
          in = forked
          (anonymousName, List.empty, List.empty)
        }
      } catch {
        /**
         * We are first trying to parse non-anonymous given, which
         *   - requires `:` for type, if none is found it means it's anonymous
         *   - might fail in cases that anonymous type looks like type param
         *     {{{given Conversion[AppliedName, Expr.Apply[Id]] = ???}}} This will fail because type
         *     params cannot have `.`
         */
        case NonFatal(_) =>
          in = forked
          (anonymousName, List.empty, List.empty)
      }

    val decltype = if (token.is[LeftBrace]) refinement(None) else startModType()

    def parents() = {
      val parents = ListBuffer[Init](
        atPos(decltype.startTokenPos, auto)(
          initRest(() => decltype, allowArgss = true, allowBraces = false)
        )
      )
      while (token.is[KwWith] && ahead(token.is[Ident])) { next(); parents += init() }
      parents.toList
    }

    if (token.is[Equals]) {
      accept[Equals]
      Defn.GivenAlias(mods, sigName, sigTparams, sigUparamss, decltype, exprMaybeIndented())
    } else if (token.is[KwWith] || token.is[LeftParen]) {
      val inits = parents()
      val (slf, stats) = {
        if (inits.size > 1 && token.isNot[KwWith])
          syntaxError("expected 'with' <body>", at = token.pos)

        val needsBody = token.is[KwWith]
        acceptOpt[KwWith]

        newLineOptWhenFollowedBy[LeftBrace]
        if (token.is[LeftBrace]) {
          inBraces(templateStatSeq())
        } else if (token.is[Indentation.Indent]) {
          indented(templateStatSeq())
        } else if (needsBody) {
          syntaxError("expected '{' or indentation", at = token.pos)
        } else (autoPos(Self(autoPos(Name.Anonymous()), None)), Nil)
      }
      val rhs = if (slf.decltpe.nonEmpty) {
        syntaxError("given cannot have a self type", at = slf.pos)
      } else {
        atPos(decltype.startTokenPos, auto)(Template(List.empty, inits, slf, stats))
      }
      Defn.Given(mods, sigName, sigTparams, sigUparamss, rhs)
    } else {
      sigName match {
        case name: Term.Name =>
          Decl.Given(mods, name, sigTparams, sigUparamss, decltype)
        case _ =>
          syntaxError("abstract givens cannot be annonymous", at = sigName.pos)
      }
    }
  }

  def funDefOrDclOrExtensionOrSecondaryCtor(mods: List[Mod]): Stat = {
    if (ahead(token.isNot[KwThis])) funDefRest(mods)
    else secondaryCtor(mods)
  }

  // TmplDef           ::= ‘extension’ [DefTypeParamClause] ‘(’ DefParam ‘)’ {UsingParamClause} ExtMethods
  // ExtMethods        ::=  ExtMethod | [nl] ‘{’ ExtMethod {semi ExtMethod ‘}’
  // ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
  def extensionGroupDecl(mods: List[Mod]): Defn.ExtensionGroup = atPos(mods, auto) {
    next() // 'extension'

    val tparams = typeParamClauseOpt(
      ownerIsType = false,
      ctxBoundsAllowed = true,
      allowUnderscore = dialect.allowTypeParamUnderscore
    )

    newLineOptWhenFollowedBy[LeftParen]

    val paramss = ListBuffer[List[Term.Param]]()

    def collectUparams(): Unit = {
      newLineOptWhenFollowedBy[LeftParen]
      while (token.is[LeftParen] && ahead(token.is[soft.KwUsing])) {
        paramss += inParens {
          if (token.isNot[soft.KwUsing]) syntaxError("expected 'using' keyword", token)
          next()
          commaSeparated(
            param(
              ownerIsCase = false,
              ownerIsType = true,
              isImplicit = false,
              isUsing = true
            )
          )
        }
        newLineOptWhenFollowedBy[LeftParen]
      }
    }

    collectUparams()

    paramss += inParens(
      List(param(ownerIsCase = false, ownerIsType = false, isImplicit = false, isUsing = false))
    )

    collectUparams()
    newLineOptWhenFollowedBy[LeftBrace]

    val body: Stat =
      if (token.is[LeftBrace]) {
        autoPos(Term.Block(inBraces(templateStats())))
      } else {
        newLinesOpt()
        if (in.observeIndented()) {
          val block = autoPos(Term.Block(indented(templateStats())))
          if (block.stats.size == 1) block.stats.head
          else block
        } else if (token.is[DefIntro]) {
          nonLocalDefOrDcl()
        } else {
          syntaxError("Extension without extension method", token)
        }
      }
    Defn.ExtensionGroup(tparams, paramss.toList, body)
  }

  def funDefRest(mods: List[Mod]): Stat = atPos(mods, auto) {
    accept[KwDef]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    rejectMod[Mod.Open](mods, Messages.InvalidOpen)
    if (!mods.has[Mod.Override])
      rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    val name = termName()
    def warnProcedureDeprecation = {
      val hint = s"Convert procedure `$name` to method by adding `: Unit =`."
      if (dialect.allowProcedureSyntax)
        deprecationWarning(
          s"Procedure syntax is deprecated. $hint",
          at = name
        )
      else
        syntaxError(s"Procedure syntax is not supported. $hint", at = name)
    }
    val tparams = typeParamClauseOpt(ownerIsType = false, ctxBoundsAllowed = true)
    val paramss = paramClauses(ownerIsType = false).require[List[List[Term.Param]]]

    def onlyLastParameterCanBeRepeated(params: List[Term.Param]): Unit = {
      params.iterator
        .take(params.length - 1)
        .filter(p => !p.is[Term.Param.Quasi] && p.decltpe.exists(_.is[Type.Repeated]))
        .foreach(p => syntaxError("*-parameter must come last", p))
    }

    paramss.foreach(onlyLastParameterCanBeRepeated)

    newLineOptWhenFollowedBy[LeftBrace]
    val restype = fromWithinReturnType(typedOpt())
    if (token.is[StatSep] || token.is[RightBrace] || token.is[Indentation.Outdent]) {
      if (restype.isEmpty) {
        warnProcedureDeprecation
        Decl.Def(
          mods,
          name,
          tparams,
          paramss,
          atPos(in.tokenPos, in.prevTokenPos)(Type.Name("Unit"))
        )
      } else
        Decl.Def(mods, name, tparams, paramss, restype.get)
    } else if (restype.isEmpty && token.is[LeftBrace]) {
      warnProcedureDeprecation
      Defn.Def(
        mods,
        name,
        tparams,
        paramss,
        Some(atPos(in.tokenPos, in.prevTokenPos)(Type.Name("Unit"))),
        expr()
      )
    } else {
      var isMacro = false
      val rhs = {
        if (token.is[Equals]) {
          next()
          isMacro = token.is[KwMacro]
          if (isMacro) next()
        } else {
          accept[Equals]
        }
        exprMaybeIndented()
      }
      if (isMacro) Defn.Macro(mods, name, tparams, paramss, restype, rhs)
      else Defn.Def(mods, name, tparams, paramss, restype, rhs)
    }
  }

  def typeIndentedOpt() = {
    if (token.is[Indentation.Indent]) {
      indented(typ())
    } else {
      typ()
    }
  }

  def typeDefOrDcl(mods: List[Mod]): Member.Type with Stat = atPos(mods, auto) {
    accept[KwType]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    rejectMod[Mod.Implicit](mods, Messages.InvalidImplicit)
    if (!mods.has[Mod.Override])
      rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)

    def aliasType() = {
      // empty bounds also need to have origin
      val emptyBounds = autoPos(Type.Bounds(None, None))
      Defn.Type(mods, name, tparams, typeIndentedOpt(), emptyBounds)
    }

    def abstractType() = {
      val bounds = typeBounds()
      if (token.is[Equals]) {
        accept[Equals]
        val tpe = typeIndentedOpt()
        if (tpe.is[Type.Match]) {
          Defn.Type(mods, name, tparams, tpe, bounds)
        } else {
          syntaxError("cannot combine bound and alias", at = tpe.pos)
        }
      } else {
        Decl.Type(mods, name, tparams, bounds)
      }
    }
    if (mods.exists(_.is[Mod.Opaque])) {
      val bounds = typeBounds()
      accept[Equals]
      Defn.Type(mods, name, tparams, typeIndentedOpt(), bounds)
    } else {
      token match {
        case Equals() => next(); aliasType()
        case Supertype() | Subtype() | Comma() | RightBrace() => abstractType()
        case StatSep() | Indentation.Outdent() => abstractType()
        case _ => syntaxError("`=', `>:', or `<:' expected", at = token)
      }
    }
  }

  /** Hook for IDE, for top-level classes/objects. */
  def topLevelTmplDef: Stat =
    tmplDef(annots(skipNewLines = true) ++ modifiers())

  def tmplDef(mods: List[Mod]): Stat = {
    if (!dialect.allowToplevelStatements) {
      rejectMod[Mod.Lazy](mods, Messages.InvalidLazyClasses)
    }
    token match {
      case KwTrait() =>
        traitDef(mods)
      case KwEnum() =>
        enumDef(mods)
      case KwClass() =>
        classDef(mods)
      case KwCase() if ahead(token.is[KwClass]) =>
        val casePos = in.tokenPos
        next()
        classDef(mods :+ atPos(casePos, casePos)(Mod.Case()))
      case KwObject() =>
        objectDef(mods)
      case KwCase() if ahead(token.is[KwObject]) =>
        val casePos = in.tokenPos
        next()
        objectDef(mods :+ atPos(casePos, casePos)(Mod.Case()))
      case Token.At() =>
        syntaxError("Annotations must precede keyword modifiers", at = token)
      case DefIntro() if dialect.allowToplevelStatements =>
        defOrDclOrSecondaryCtor(mods)
      case _ =>
        syntaxError(s"expected start of definition", at = token)
    }
  }

  def traitDef(mods: List[Mod]): Defn.Trait = atPos(mods, auto) {
    val traitPos = in.tokenPos
    val assumedAbstract = atPos(traitPos, traitPos)(Mod.Abstract())
    // Add `abstract` to traits for error reporting
    val fullMods = mods :+ assumedAbstract
    accept[KwTrait]
    rejectMod[Mod.Implicit](mods, Messages.InvalidImplicitTrait)
    val traitName = typeName()
    val culprit = s"trait $traitName"
    rejectModCombination[Mod.Final, Mod.Abstract](fullMods, Some(culprit))
    rejectModCombination[Mod.Override, Mod.Abstract](fullMods, Some(culprit))
    rejectModCombination[Mod.Open, Mod.Final](mods, Some(culprit))
    rejectModCombination[Mod.Open, Mod.Sealed](mods, Some(culprit))
    Defn.Trait(
      mods,
      traitName,
      typeParamClauseOpt(
        ownerIsType = true,
        ctxBoundsAllowed = dialect.allowTraitParameters,
        allowUnderscore = dialect.allowTypeParamUnderscore
      ),
      primaryCtor(OwnedByTrait),
      templateOpt(OwnedByTrait)
    )
  }

  def classDef(mods: List[Mod]): Defn.Class = atPos(mods, auto) {
    accept[KwClass]
    rejectMod[Mod.Override](mods, Messages.InvalidOverrideClass)

    val className = typeName()
    val culprit = s"class $className"
    rejectModCombination[Mod.Final, Mod.Sealed](mods, Some(culprit))
    rejectModCombination[Mod.Open, Mod.Final](mods, Some(culprit))
    rejectModCombination[Mod.Open, Mod.Sealed](mods, Some(culprit))
    rejectModCombination[Mod.Case, Mod.Implicit](mods, Some(culprit))
    val typeParams = typeParamClauseOpt(
      ownerIsType = true,
      ctxBoundsAllowed = true,
      allowUnderscore = dialect.allowTypeParamUnderscore
    )
    val ctor = primaryCtor(if (mods.has[Mod.Case]) OwnedByCaseClass else OwnedByClass)

    if (!dialect.allowCaseClassWithoutParameterList && mods.has[Mod.Case] && ctor.paramss.isEmpty) {
      syntaxError(
        s"case classes must have a parameter list; try 'case class $className()' or 'case object $className'",
        at = token
      )
    }

    val tmpl = templateOpt(OwnedByClass)
    Defn.Class(mods, className, typeParams, ctor, tmpl)
  }

  // EnumDef           ::=  id ClassConstr InheritClauses EnumBody
  def enumDef(mods: List[Mod]): Defn.Enum = atPos(mods, auto) {
    accept[KwEnum]

    val enumName = typeName()
    val culprit = s"enum $enumName"

    onlyAllowedMods(
      mods,
      List(
        summonClassifierFunc[Mod, Mod.Private],
        summonClassifierFunc[Mod, Mod.Protected],
        summonClassifierFunc[Mod, Mod.Annot]
      ),
      culprit
    )

    val typeParams = typeParamClauseOpt(
      ownerIsType = true,
      ctxBoundsAllowed = true,
      allowUnderscore = dialect.allowTypeParamUnderscore
    )
    val ctor = primaryCtor(OwnedByEnum)
    val tmpl = templateOpt(OwnedByEnum)
    Defn.Enum(mods, enumName, typeParams, ctor, tmpl)
  }

  // EnumCase          ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps]] | ids)
  // ids               ::=  id {‘,’ id}
  // ClassConstr       ::=  [ClsTypeParamClause] [ConstrMods] ClsParamClauses
  def enumCaseDef(mods: List[Mod]): Stat = atPos(mods, auto) {
    accept[KwCase]
    if (token.is[Ident] && ahead(token.is[Comma])) {
      enumRepeatedCaseDef(mods)
    } else {
      enumSingleCaseDef(mods)
    }
  }

  def enumRepeatedCaseDef(mods: List[Mod]): Defn.RepeatedEnumCase = {
    val values = commaSeparated(termName())
    Defn.RepeatedEnumCase(mods, values)
  }

  def enumSingleCaseDef(mods: List[Mod]): Defn.EnumCase = {
    val name = termName()
    val tparams = typeParamClauseOpt(
      ownerIsType = true,
      ctxBoundsAllowed = true,
      allowUnderscore = dialect.allowTypeParamUnderscore
    )
    val ctor = primaryCtor(OwnedByEnum)
    val inits = if (token.is[KwExtends]) {
      accept[KwExtends]
      templateParents(afterExtend = true)
    } else { List() }
    Defn.EnumCase(mods, name, tparams, ctor, inits)
  }

  def objectDef(mods: List[Mod]): Defn.Object = atPos(mods, auto) {
    accept[KwObject]
    val objectName = termName()
    val culprit = s"object $objectName"
    if (!mods.has[Mod.Override]) rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    rejectModCombination[Mod.Open, Mod.Final](mods, Some(culprit))
    rejectModCombination[Mod.Open, Mod.Sealed](mods, Some(culprit))
    Defn.Object(mods, objectName, templateOpt(OwnedByObject))
  }

  /* -------- CONSTRUCTORS ------------------------------------------- */

  def primaryCtor(owner: TemplateOwner): Ctor.Primary = autoPos {
    if (owner.isClass || (owner.isTrait && dialect.allowTraitParameters) || owner.isEnum) {
      val mods = constructorAnnots() ++ ctorModifiers()
      val name = autoPos(Name.Anonymous())
      val paramss = paramClauses(ownerIsType = true, owner == OwnedByCaseClass)
      Ctor.Primary(mods, name, paramss)
    } else if (owner.isTrait) {
      Ctor.Primary(Nil, autoPos(Name.Anonymous()), Nil)
    } else {
      unreachable(debug(owner))
    }
  }

  def secondaryCtor(mods: List[Mod]): Ctor.Secondary = atPos(mods, auto) {
    accept[KwDef]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    val name = atPos(in.tokenPos, in.tokenPos)(Name.Anonymous())
    accept[KwThis]
    if (token.isNot[LeftParen]) {
      syntaxError("auxiliary constructor needs non-implicit parameter list", at = token.pos)
    } else {
      val paramss = paramClauses(ownerIsType = true)
      secondaryCtorRest(mods, name, paramss)
    }
  }

  def quasiquoteCtor(): Ctor = autoPos {
    val anns = annots(skipNewLines = true)
    val mods = anns ++ modifiers()
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    accept[KwDef]
    val name = atPos(in.tokenPos, in.tokenPos)(Name.Anonymous())
    accept[KwThis]
    val paramss = paramClauses(ownerIsType = true)
    newLineOptWhenFollowedBy[LeftBrace]
    if (token.is[EOF]) Ctor.Primary(mods, name, paramss)
    else secondaryCtorRest(mods, name, paramss)
  }

  def entrypointCtor(): Ctor = {
    ???
  }

  def secondaryCtorRest(
      mods: List[Mod],
      name: Name,
      paramss: List[List[Term.Param]]
  ): Ctor.Secondary = {
    newLineOptWhenFollowedBy[LeftBrace]
    val (init, stats) = token match {
      case LeftBrace() => constrBlock()
      case _ => accept[Equals]; constrExpr()
    }
    Ctor.Secondary(mods, name, paramss, init, stats)
  }

  def constrInternal(): (Init, List[Stat]) = {
    val init = initInsideConstructor()
    val stats =
      if (!token.is[StatSep]) Nil
      else {
        next(); blockStatSeq()
      }
    (init, stats)
  }

  def constrBlock(): (Init, List[Stat]) = inBraces(constrInternal())

  def constrIndent(): (Init, List[Stat]) = indented(constrInternal())

  def constrExpr(): (Init, List[Stat]) =
    if (token.is[LeftBrace]) constrBlock()
    else if (token.is[Indentation.Indent]) constrIndent()
    else (initInsideConstructor(), Nil)

  def initInsideConstructor(): Init = {
    def name = autoPos { accept[KwThis]; Name.Anonymous() }
    def tpe = autoPos(Type.Singleton(autoPos(Term.This(name))))
    initRest(() => tpe, allowArgss = true, allowBraces = true)
  }

  def initInsideAnnotation(allowArgss: Boolean): Init = {
    def tpe = exprSimpleType()
    initRest(() => tpe, allowArgss = allowArgss, allowBraces = false)
  }

  def initInsideTemplate(): Init = {
    def tpe = startModType()
    initRest(() => tpe, allowArgss = true, allowBraces = false)
  }

  def quasiquoteInit(): Init = entrypointInit()

  def entrypointInit(): Init = {
    token match {
      case KwThis() => initInsideConstructor()
      case _ => initInsideTemplate()
    }
  }

  def initRest(typeParser: () => Type, allowArgss: Boolean, allowBraces: Boolean): Init = autoPos {
    def isPendingArglist = token.is[LeftParen] || (token.is[LeftBrace] && allowBraces)
    def newlineOpt() = {
      if (dialect.allowSignificantIndentation)
        newLineOptWhenFollowedBySignificantIndentationAnd(x => x.is[LeftBrace] || x.is[LeftParen])
      else if (allowBraces) newLineOptWhenFollowedBy[LeftBrace]
    }
    token match {
      case Unquote() if !ahead(isPendingArglist) =>
        unquote[Init]
      case _ =>
        val tpe = typeParser()
        val name = autoPos(Name.Anonymous())
        val argss = mutable.ListBuffer[List[Term]]()
        var done = false
        newlineOpt()
        while (isPendingArglist && !done) {
          argss += argumentExprs()
          if (!allowArgss) done = true
          newlineOpt()
        }
        Init(tpe, name, argss.toList)
    }
  }

  /* ---------- SELFS --------------------------------------------- */

  def quasiquoteSelf(): Self = entrypointSelf()

  def entrypointSelf(): Self = self()

  def self(): Self = autoPos {
    val name = token match {
      case Ident(_) =>
        termName()
      case Underscore() | KwThis() =>
        autoPos { next(); Name.Anonymous() }
      case Unquote() =>
        if (ahead(token.is[Colon])) unquote[Name.Quasi]
        else return unquote[Self.Quasi]
      case _ =>
        syntaxError("expected identifier, `this' or unquote", at = token)
    }
    val decltpe = token match {
      case Colon() =>
        next()
        Some(startInfixType())
      case _ =>
        None
    }
    Self(name, decltpe)
  }

  /* -------- TEMPLATES ------------------------------------------- */

  sealed trait TemplateOwner {
    def isTerm = this eq OwnedByObject
    def isClass = (this eq OwnedByCaseClass) || (this eq OwnedByClass)
    def isTrait = this eq OwnedByTrait
    def isEnum = this eq OwnedByEnum
  }
  object OwnedByTrait extends TemplateOwner
  object OwnedByCaseClass extends TemplateOwner
  object OwnedByClass extends TemplateOwner
  object OwnedByEnum extends TemplateOwner
  object OwnedByObject extends TemplateOwner

  def init() = {
    token match {
      case Ellipsis(_) => ellipsis(1, astInfo[Init])
      case _ => initInsideTemplate()
    }
  }

  def templateParents(
      afterExtend: Boolean = false
  ): List[Init] = {
    def isCommaSeparated(token: Token): Boolean =
      afterExtend && token.is[Comma] && dialect.allowCommaSeparatedExtend
    val parents = ListBuffer[Init]()
    parents += init()
    while (token.is[KwWith] || isCommaSeparated(token)) { next(); parents += init() }
    parents.toList
  }

  def derivesClasses(): List[Type] = {
    newLineOptWhenFollowing(t => t.is[soft.KwDerives])
    if (token.is[soft.KwDerives]) {
      next()
      val deriving = ListBuffer[Type]()
      def readInit() = token match {
        case Ellipsis(_) => deriving += ellipsis(1, astInfo[Type])
        case _ => deriving += startModType()
      }
      readInit()
      while (token.is[Comma]) { next(); readInit() }
      deriving.toList
    } else {
      Nil
    }
  }

  def template(
      edefs: List[Stat],
      parents: List[Init],
      enumCaseAllowed: Boolean,
      secondaryConstructorAllowed: Boolean
  ): Template = {
    val derived = derivesClasses()
    val (self, body) =
      templateBodyOpt(parenMeansSyntaxError = false, enumCaseAllowed, secondaryConstructorAllowed)
    Template(edefs, parents, self, body, derived)
  }

  def template(
      afterExtend: Boolean = false,
      enumCaseAllowed: Boolean,
      secondaryConstructorAllowed: Boolean
  ): Template = autoPos {
    newLineOptWhenFollowedBy[LeftBrace]
    if (token.is[LeftBrace]) {
      // @S: pre template body cannot stub like post body can!
      val (self, body) = templateBody(enumCaseAllowed)
      if (token.is[KwWith] && self.name.is[Name.Anonymous] && self.decltpe.isEmpty) {
        val edefs = body.map(ensureEarlyDef)
        next()
        val parents = templateParents(afterExtend)
        template(edefs, parents, enumCaseAllowed, secondaryConstructorAllowed)
      } else {
        Template(Nil, Nil, self, body)
      }
    } else {
      val parents = if (token.is[Colon]) Nil else templateParents(afterExtend)
      template(Nil, parents, enumCaseAllowed, secondaryConstructorAllowed)
    }
  }

  def quasiquoteTemplate(): Template = entrypointTemplate()

  def entrypointTemplate(): Template = autoPos(
    template(enumCaseAllowed = false, secondaryConstructorAllowed = true)
  )

  def ensureEarlyDef(tree: Stat): Stat = tree match {
    case q: Quasi => q
    case v: Defn.Val => v
    case v: Defn.Var => v
    case t: Defn.Type => t
    case other => syntaxError("not a valid early definition", at = other)
  }

  def templateOpt(owner: TemplateOwner): Template = autoPos {
    if (token.is[Unquote] && ahead(
        !token.is[Dot] && !token.is[Hash] && !token.is[At] && !token.is[Ellipsis] &&
          !token.is[LeftParen] && !token.is[LeftBracket] && !token.is[LeftBrace] &&
          !token.is[KwWith]
      )) {
      unquote[Template]
    } else if (token.is[KwExtends] || (token.is[Subtype] && owner.isTrait)) {
      next()
      template(
        afterExtend = true,
        enumCaseAllowed = owner.isEnum,
        secondaryConstructorAllowed = owner.isClass || owner.isEnum
      )
    } else {
      newLineOptWhenFollowing(t => t.is[soft.KwDerives])
      if (token.is[soft.KwDerives]) {
        template(
          Nil,
          Nil,
          enumCaseAllowed = owner.isEnum,
          secondaryConstructorAllowed = owner.isClass || owner.isEnum
        )
      } else {
        val (self, body) =
          templateBodyOpt(
            parenMeansSyntaxError = !owner.isClass,
            enumCaseAllowed = owner.isEnum,
            secondaryConstructorAllowed = owner.isClass || owner.isEnum
          )
        Template(Nil, Nil, self, body)
      }
    }
  }

  def templateBody(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): (Self, List[Stat]) =
    inBraces(templateStatSeq(enumCaseAllowed, secondaryConstructorAllowed))

  def templateBodyOpt(
      parenMeansSyntaxError: Boolean,
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): (Self, List[Stat]) = {
    newLineOptWhenFollowedBy[LeftBrace]
    if (token.is[LeftBrace]) {
      templateBody(enumCaseAllowed, secondaryConstructorAllowed)
    } else if (isColonEol(token)) {
      accept[Colon]

      val nextIndented =
        if (enumCaseAllowed)
          in.observeIndentedEnum()
        else
          in.observeIndented()

      if (nextIndented)
        indented(templateStatSeq(enumCaseAllowed, secondaryConstructorAllowed))
      else if (token.is[EndMarkerIntro] && !enumCaseAllowed)
        (autoPos(Self(autoPos(Name.Anonymous()), None)), Nil)
      else
        syntaxError("expected template body", token)
    } else {
      if (token.is[LeftParen]) {
        if (parenMeansSyntaxError) {
          val what = if (dialect.allowTraitParameters) "objects" else "traits or objects"
          syntaxError(s"$what may not have parameters", at = token)
        } else syntaxError("unexpected opening parenthesis", at = token)
      }
      (autoPos(Self(autoPos(Name.Anonymous()), None)), Nil)
    }
  }

  def refinement(innerType: Option[Type]): Type.Refine = {
    val refinedStats = inBraces(refineStatSeq())
    val refineType = atPos(innerType, auto)(Type.Refine(innerType, refinedStats))
    newLineOptWhenFollowedBy[LeftBrace]
    if (token.is[LeftBrace]) refinement(innerType = Some(refineType))
    else refineType
  }

  def existentialStats(): List[Stat] = inBraces(refineStatSeq()) map {
    case stat if stat.isExistentialStat => stat
    case other => syntaxError("not a legal existential clause", at = other)
  }

  /* -------- STATSEQS ------------------------------------------- */

  private val consumeStat: PartialFunction[Token, Stat] = {
    case KwImport() => importStmt()
    case KwExport() => exportStmt()
    case KwPackage() =>
      packageOrPackageObjectDef(if (dialect.allowToplevelTerms) consumeStat else topStat)
    case DefIntro() => nonLocalDefOrDcl(secondaryConstructorAllowed = true)
    case EndMarkerIntro() => endMarker()
    case ExprIntro() => stat(expr(location = NoStat, allowRepeated = true))
    case Ellipsis(_) => ellipsis(1, astInfo[Stat])
  }

  def quasiquoteStat(): Stat = {
    def failEmpty() = {
      syntaxError("unexpected end of input", at = token)
    }
    def failMix(advice: Option[String]) = {
      val message = "these statements can't be mixed together"
      val addendum = advice.fold("")(", " + _)
      syntaxError(message + addendum, at = scannerTokens.head)
    }
    statSeq(consumeStat) match {
      case Nil => failEmpty()
      case (stat @ Stat.Quasi(1, _)) :: Nil => Term.Block(List(stat))
      case stat :: Nil => stat
      case stats if stats.forall(_.isBlockStat) => Term.Block(stats)
      case stats if stats.forall(_.isTopLevelStat) => failMix(Some("try source\"...\" instead"))
      case _ => failMix(None)
    }
  }

  def entrypointStat(): Stat = {
    @tailrec
    def skipStatementSeparators(): Unit = {
      if (token.is[EOF]) return
      if (!token.is[StatSep]) accept[EOF]
      next()
      skipStatementSeparators()
    }
    val maybeStat = consumeStat.lift(token)
    val stat = maybeStat.getOrElse(syntaxError("unexpected start of statement", at = token))
    skipStatementSeparators()
    stat
  }

  def stat(body: => Stat): Stat = {
    body match {
      case q: Quasi => q.become[Stat.Quasi]
      case other => other
    }
  }

  def statSeq[T <: Tree: AstInfo](
      statpf: PartialFunction[Token, T],
      errorMsg: String = "illegal start of definition"
  ): List[T] = {
    val stats = new ListBuffer[T]
    val isIndented = token.is[Indentation.Indent]

    if (isIndented) accept[Indentation.Indent]
    while (!token.is[StatSeqEnd]) {
      def isDefinedInEllipsis = {
        if (token.is[LeftParen] || token.is[LeftBrace]) next(); statpf.isDefinedAt(token)
      }
      if (statpf.isDefinedAt(token) || (token.is[Ellipsis] && ahead(isDefinedInEllipsis)))
        stats += statpf(token)
      else if (!token.is[StatSep])
        syntaxError(errorMsg + s" ${token.name}", at = token)
      acceptStatSepOpt()
    }
    if (isIndented) accept[Indentation.Outdent]
    stats.toList
  }

  def topStat: PartialFunction[Token, Stat] = {
    case Ellipsis(_) =>
      ellipsis(1, astInfo[Stat])
    case Unquote() =>
      unquote[Stat]
    case KwPackage() =>
      packageOrPackageObjectDef(topStat)
    case KwImport() =>
      importStmt()
    case KwExport() =>
      exportStmt()
    case TemplateIntro() =>
      topLevelTmplDef
    case EndMarkerIntro() =>
      endMarker()
    case DefIntro() if dialect.allowToplevelStatements =>
      nonLocalDefOrDcl()
  }

  def templateStatSeq(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): (Self, List[Stat]) = {
    val emptySelf = autoPos(Self(autoPos(Name.Anonymous()), None))
    var selfOpt: Option[Self] = None
    var firstOpt: Option[Stat] = None
    if (token.is[ExprIntro] && !token.is[KwExtension]) {
      val beforeFirst = in.fork
      val first = expr(location = TemplateStat, allowRepeated = false)
      val afterFirst = in.fork
      if (token.is[RightArrow]) {
        try {
          in = beforeFirst
          selfOpt = Some(self())
          next()
          in.undoIndent()
        } catch {
          case _: ParseException =>
            in = afterFirst
            unreachable
        }
      } else {
        firstOpt = Some(stat(first))
        acceptStatSepOpt()
      }
    }
    (
      selfOpt.getOrElse(emptySelf),
      firstOpt ++: templateStats(enumCaseAllowed, secondaryConstructorAllowed)
    )
  }

  def templateStats(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): List[Stat] = {
    def templateStat: PartialFunction[Token, Stat] = {
      case KwImport() =>
        importStmt()
      case KwExport() =>
        exportStmt()
      case DefIntro() =>
        nonLocalDefOrDcl(enumCaseAllowed, secondaryConstructorAllowed)
      case Unquote() =>
        unquote[Stat]
      case Ellipsis(_) =>
        ellipsis(1, astInfo[Stat])
      case EndMarkerIntro() =>
        endMarker()
      case ExprIntro() =>
        expr(location = TemplateStat, allowRepeated = false)
    }
    statSeq(templateStat)
  }

  def refineStatSeq(): List[Stat] = {
    val stats = new ListBuffer[Stat]
    while (!token.is[StatSeqEnd]) {
      stats ++= refineStat()
      if (token.isNot[RightBrace]) acceptStatSep()
    }
    stats.toList
  }

  def refineStat(): Option[Stat] =
    if (token.is[Ellipsis]) {
      Some(ellipsis(1, astInfo[Stat]))
    } else if (token.is[DclIntro]) {
      defOrDclOrSecondaryCtor(Nil) match {
        case stat if stat.isRefineStat => Some(stat)
        case other => syntaxError("is not a valid refinement declaration", at = other)
      }
    } else if (!token.is[StatSep]) {
      syntaxError(
        "illegal start of declaration" +
          (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
           else ""),
        at = token
      )
    } else None

  def localDef(implicitMod: Option[Mod.Implicit]): Stat = {
    val mods = (implicitMod ++: annots(skipNewLines = true)) ++ localModifiers()
    if (mods forall {
        case _: Mod.Implicit | _: Mod.Lazy | _: Mod.Inline | _: Mod.Infix | _: Mod.Annot => true;
        case _ => false
      }) {
      defOrDclOrSecondaryCtor(mods)
    } else {
      tmplDef(mods)
    } match {
      case stat: Decl.Type if dialect.allowTypeInBlock => stat
      case stat: Decl.Type => syntaxError("is not a valid block statement", at = stat)
      case stat if stat.isBlockStat => stat
      case other => syntaxError("is not a valid block statement", at = other)
    }
  }

  def blockStatSeq(): List[Stat] = {
    val stats = new ListBuffer[Stat]
    while (!token.is[StatSeqEnd] && !token.is[CaseDefEnd] && !in.observeOutdented()) {
      if (token.is[KwExport]) {
        stats += exportStmt()
        acceptStatSepOpt()
      } else if (token.is[KwImport]) {
        stats += importStmt()
        acceptStatSepOpt()
      } else if (token.is[DefIntro] && !token.is[NonlocalModifier]) {
        if (token.is[KwImplicit]) {
          val implicitPos = in.tokenPos
          next()
          if (token.is[Ident] && token.isNot[SoftModifier]) stats += implicitClosure(BlockStat)
          else stats += localDef(Some(atPos(implicitPos, implicitPos)(Mod.Implicit())))
        } else {
          stats += localDef(None)
        }
        if (!token.is[CaseDefEnd]) acceptStatSepOpt()
      } else if (token.is[ExprIntro]) {
        stats += stat(expr(location = BlockStat, allowRepeated = false))
        if (!token.is[CaseDefEnd]) acceptStatSep()
      } else if (token.is[StatSep]) {
        next()
      } else if (token.is[Ellipsis]) {
        stats += ellipsis(1, astInfo[Stat])
      } else if (token.is[EndMarkerIntro]) {
        stats += endMarker()
      } else {
        syntaxError("illegal start of statement", at = token)
      }
    }
    stats.toList
  }

  def packageOrPackageObjectDef(statpf: PartialFunction[Token, Stat]): Stat = autoPos {
    require(token.is[KwPackage] && debug(token))
    if (ahead(token.is[KwObject])) packageObjectDef()
    else packageDef(statpf)
  }

  def packageDef(statpf: PartialFunction[Token, Stat]): Pkg = autoPos {
    accept[KwPackage]
    def packageBody = {
      if (isColonEol(token)) {
        accept[Colon]
        in.observeIndented()
        indented(statSeq(statpf))
      } else {
        inBracesOrNil(statSeq(statpf))
      }
    }
    Pkg(qualId(), packageBody)
  }

  def packageObjectDef(): Pkg.Object = autoPos {
    accept[KwPackage]
    accept[KwObject]
    Pkg.Object(Nil, termName(), templateOpt(OwnedByObject))
  }

  def source(): Source = autoPos {
    batchSource(if (dialect.allowToplevelTerms) consumeStat else topStat)
  }

  def quasiquoteSource(): Source = entrypointSource()

  def entrypointSource(): Source = source()

  def batchSource(statpf: PartialFunction[Token, Stat] = topStat): Source = autoPos {
    def inBracelessPackage(): Boolean = token.is[KwPackage] && !ahead(token.is[KwObject]) && ahead {
      qualId()
      if (token.is[LF]) { next() }
      token.isNot[LeftBrace] && !(dialect.allowSignificantIndentation && isColonEol(token))
    }
    def bracelessPackageStats(): List[Stat] = {
      if (token.is[EOF]) {
        Nil
      } else if (token.is[StatSep]) {
        next()
        bracelessPackageStats()
      } else if (token.is[KwPackage] && !ahead(token.is[KwObject])) {
        val startPos = in.tokenPos
        accept[KwPackage]
        val qid = qualId()
        def inPackage(stats: => List[Stat]) = {
          val pkg = atPos(startPos, auto)(Pkg(qid, stats))
          acceptStatSepOpt()
          pkg +: bracelessPackageStats()
        }
        if (token.is[LeftBrace]) {
          inPackage(inBraces(statSeq(statpf)))
        } else if (isColonEol(token)) {
          next()
          in.observeIndented()
          inPackage(indented(statSeq(statpf)))
        } else {
          List(atPos(startPos, auto)(Pkg(qid, bracelessPackageStats())))
        }
      } else if (token.is[LeftBrace]) {
        inBraces(statSeq(statpf))
      } else {
        statSeq(statpf)
      }
    }
    if (inBracelessPackage()) {
      val startPos = in.tokenPos
      accept[KwPackage]
      Source(List(atPos(startPos, auto)(Pkg(qualId(), bracelessPackageStats()))))
    } else {
      Source(statSeq(statpf))
    }
  }
}

object ScalametaParser {

  def toParse[T](fn: ScalametaParser => T): Parse[T] = new Parse[T] {
    def apply(input: Input, dialect: Dialect): Parsed[T] = {
      try {
        val parser = new ScalametaParser(input)(dialect)
        Parsed.Success(fn(parser))
      } catch {
        case details @ TokenizeException(pos, message) =>
          Parsed.Error(pos, message, details)
        case details @ ParseException(pos, message) =>
          Parsed.Error(pos, message, details)
      }
    }
  }
}

class Location private (val value: Int) extends AnyVal
object Location {
  val NoStat = new Location(0)
  val BlockStat = new Location(1)
  val TemplateStat = new Location(2)
}

object InfixMode extends Enumeration {
  val FirstOp, LeftOp, RightOp = Value
}
