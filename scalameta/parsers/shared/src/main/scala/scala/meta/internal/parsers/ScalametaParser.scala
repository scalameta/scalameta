package scala.meta
package internal
package parsers

import org.scalameta._
import org.scalameta.invariants._
import scala.meta.classifiers._
import scala.meta.inputs._
import scala.meta.internal.parsers.Absolutize._
import scala.meta.internal.parsers.Location._
import scala.meta.internal.trees._
import scala.meta.parsers._
import scala.meta.prettyprinters._
import scala.meta.tokens.Token._
import scala.meta.tokens._
import scala.meta.trees.Origin

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Success, Try}

class ScalametaParser(input: Input)(implicit dialect: Dialect, options: ParserOptions) {
  parser =>

  import ScalametaParser._

  private val scannerTokens: ScannerTokens = ScannerTokens(input)
  import scannerTokens._

  /* ------------- NESTED CONTEXT OBJECTS ----------------------------------------- */
  // must all be parser-specific, to avoid sharing state with other parsers
  private object QuotedSpliceContext extends NestedContext
  private object PatternContext extends NestedContext
  private object ReturnTypeContext extends NestedContext
  private object TypeBracketsContext extends NestedContext
  private object PatternTypeContext extends NestedContext
  private object ExtensionSigContext extends NestedContext
  private object GivenSigContext extends NestedContext
  private object TemplateOwnerContext extends NestedContextWithOwner[TemplateOwner](OwnedByObject)

  /* ------------- PARSER ENTRY POINTS -------------------------------------------- */

  def parseRule[T <: Tree](rule: this.type => T): T = parseRule(rule(this))

  def parseRule[T <: Tree](rule: => T): T = {
    // NOTE: can't require in.tokenPos to be at -1, because TokIterator auto-rewinds when created
    // require(in.tokenPos == -1 && debug(in.tokenPos))
    accept[BOF]
    parseRuleAfterBOF(rule)
  }

  private def parseRuleAfterBOF[T <: Tree](rule: => T): T = {
    val start = prevIndex
    val t = rule
    // NOTE: can't have prevTokenPos here
    // because we need to subsume all the trailing trivia
    val end = currIndex
    accept[EOF]
    atPos(start, end)(t)
  }

  // Entry points for Parse[T]
  def parseStat(): Stat = parseRule(if (dialect.allowUnquotes) quasiquoteStat() else entrypointStat())

  def parseTerm(): Term = parseRule(if (dialect.allowUnquotes) quasiquoteExpr() else entrypointExpr())

  def parseUnquoteTerm(): Term = parseRule(unquoteExpr())

  def parseTermParam(): Term.Param =
    parseRule(if (dialect.allowUnquotes) quasiquoteTermParam() else entrypointTermParam())

  def parseType(): Type = parseRule(if (dialect.allowUnquotes) quasiquoteType() else entrypointType())

  def parseTypeParam(): Type.Param =
    parseRule(if (dialect.allowUnquotes) quasiquoteTypeParam() else entrypointTypeParam())

  def parsePat(): Pat =
    parseRule(if (dialect.allowUnquotes) quasiquotePattern() else entrypointPattern())

  def parseUnquotePat(): Pat = parseRule(unquotePattern())

  def parseCase(): Case = parseRule(if (dialect.allowUnquotes) quasiquoteCase() else entrypointCase())

  def parseCtor(): Ctor = parseRule(if (dialect.allowUnquotes) quasiquoteCtor() else entrypointCtor())

  def parseInit(): Init = parseRule(if (dialect.allowUnquotes) quasiquoteInit() else entrypointInit())

  def parseSelf(): Self = parseRule(if (dialect.allowUnquotes) quasiquoteSelf() else entrypointSelf())

  def parseTemplate(): Template =
    parseRule(if (dialect.allowUnquotes) quasiquoteTemplate() else entrypointTemplate())

  def parseMod(): Mod =
    parseRule(if (dialect.allowUnquotes) quasiquoteModifier() else entrypointModifier())

  def parseEnumerator(): Enumerator =
    parseRule(if (dialect.allowUnquotes) quasiquoteEnumerator() else entrypointEnumerator())

  def parseImporter(): Importer =
    parseRule(if (dialect.allowUnquotes) quasiquoteImporter() else entrypointImporter())

  def parseImportee(): Importee =
    parseRule(if (dialect.allowUnquotes) quasiquoteImportee() else entrypointImportee())

  def parseSource(): Source = parseRule(parseSourceImpl())

  private def parseSourceImpl(): Source =
    if (dialect.allowUnquotes) quasiquoteSource() else entrypointSource()

  def parseAmmonite(): MultiSource = parseRule(entryPointAmmonite())

  def entryPointAmmonite(): MultiSource = {
    assert(input.isInstanceOf[Input.Ammonite], s"Expected Input.Ammonite, not ${input.getClass}")
    val builder = List.newBuilder[Source]

    doWhile(builder += parseRuleAfterBOF(parseSourceImpl()))(currToken match {
      case t: Token.EOF if t.end < input.chars.length =>
        in.next()
        accept[Token.At]
        accept[Token.BOF]
        true
      case _ => false
    })
    MultiSource(builder.result())
  }

  /* ------------- TOKEN STREAM HELPERS -------------------------------------------- */

  @inline
  private def nextIfColonIndent(): Boolean = at[Colon] && nextIfIndentAhead()
  @inline
  private def nextIfIndentAhead(): Boolean = tryAhead[Indentation.Indent]

  /* ------------- PARSER-SPECIFIC TOKENS -------------------------------------------- */

  var in: TokenIterator = LazyTokenIterator(scannerTokens)

  @inline
  def currIndex = in.currIndex
  @inline
  def prevIndex = in.prevIndex
  @inline
  def currToken = in.currToken
  @inline
  def prevToken = in.prevToken
  @inline
  def peekIndex = in.peekIndex
  @inline
  def peekToken = in.peekToken

  def next() = {
    in.next()
    currToken match {
      case t: Token.Invalid => reporter.syntaxError(t.error, at = t)
      case _ =>
    }
  }
  def nextTwice() = {
    next()
    next()
  }

  @inline
  private def nextIf(cond: Boolean): Boolean = {
    if (cond) next()
    cond
  }

  /* ------------- PARSER COMMON -------------------------------------------- */

  /**
   * Scoping operator used to temporarily look into the future. Backs up token iterator before
   * evaluating a block and restores it after.
   */
  @inline
  final def ahead[T](body: => T): T = {
    val forked = in.fork
    try next(body)
    finally in = forked
  }

  @inline
  private def tryAhead[T: ClassTag]: Boolean = nextIf(peek[T])
  @inline
  private def tryAhead[A: ClassTag, B: ClassTag]: Boolean = nextIf(peek[A, B])

  @inline
  private def tryAheadNot[T: ClassTag]: Boolean = nextIf(!peek[T])
  @inline
  private def tryAheadNot[A: ClassTag, B: ClassTag]: Boolean = nextIf(!peek[A, B])

  private def unreachable(debuggees: Map[String, Any]): Nothing = UnreachableError.raise(debuggees)
  private def unreachable(tok: Token): Nothing = unreachable(Map("token" -> tok))
  private def unreachable: Nothing = unreachable(Map.empty[String, Any])

  private def tryAhead(cond: => Boolean): Boolean = {
    val forked = in.fork
    next()
    val ok = cond
    if (!ok) in = forked
    ok
  }

  private def tryParse[A](bodyFunc: => Option[A]): Option[A] = {
    val forked = in.fork
    val body = bodyFunc
    if (body.isEmpty) in = forked
    body
  }

  private def tryAhead[A](bodyFunc: => Option[A]): Option[A] = tryParse(next(bodyFunc))

  /** evaluate block after shifting next */
  @inline
  private def next[T](body: => T): T = {
    next()
    body
  }

  @inline
  private def nextAfter[T](body: T): T = next(body)

  @inline
  final def inParens[T](body: => T): T = {
    accept[LeftParen]
    inParensAfterOpen(body)
  }
  @inline
  final def inParensOr[T](body: => T)(ifEmpty: => T): T = {
    accept[LeftParen]
    inParensAfterOpenOr(body)(ifEmpty)
  }
  @inline
  private def inParensOnOpen[T](body: => T): T = {
    next()
    inParensAfterOpen(body)
  }
  @inline
  private def inParensOnOpenOr[T](body: => T)(ifEmpty: => T): T = {
    next()
    inParensAfterOpenOr(body)(ifEmpty)
  }
  @inline
  private def inParensAfterOpen[T](body: T): T = {
    acceptAfterOptNL[RightParen]
    body
  }
  @inline
  private def inParensAfterOpenOr[T](body: => T)(ifEmpty: => T): T =
    if (acceptOpt[RightParen]) ifEmpty else inParensAfterOpen(body)

  @inline
  final def inBraces[T](body: => T): T = inBracesOr(body)(syntaxErrorExpected[LeftBrace])
  @inline
  final def inBracesOr[T](body: => T)(ifEmpty: => T): T = {
    newLineOpt()
    if (acceptOpt[LeftBrace]) inBracesAfterOpen(body) else ifEmpty
  }
  @inline
  private def inBracesOnOpen[T](body: => T): T = {
    next()
    inBracesAfterOpen(body)
  }
  @inline
  private def inBracesAfterOpen[T](body: T): T = {
    acceptAfterOptNL[RightBrace]
    body
  }

  @inline
  final def indented[T](body: => T): T = {
    accept[Indentation.Indent]
    indentedAfterOpen(body)
  }
  @inline
  private def indentedOnOpen[T](body: => T): T = {
    next()
    indentedAfterOpen(body)
  }
  @inline
  private def indentedAfterOpen[T](body: T): T = {
    acceptAfterOptNL[Indentation.Outdent]
    body
  }
  private def indentedOr[T](body: => T)(orElse: => T): T =
    if (acceptOpt[Indentation.Indent]) indentedAfterOpen(body) else orElse
  @inline
  private def maybeIndented[T](body: => T): T = indentedOr(body)(body)

  @inline
  final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOr(body)(Nil)
  @inline
  final def dropAnyBraces[T](body: => T): T = inBracesOr(body)(body)

  @inline
  final def inBrackets[T](body: => T): T = {
    accept[LeftBracket]
    inBracketsAfterOpen(body)
  }
  @inline
  private def inBracketsOnOpen[T](body: => T): T = {
    next()
    inBracketsAfterOpen(body)
  }
  @inline
  private def inBracketsAfterOpen[T](body: T): T = {
    accept[RightBracket]
    body
  }

  private def typeClauseInBrackets[A <: Tree: AstInfo: ClassTag, B <: Tree: AstInfo](
      part: => A,
      f: List[A] => B
  ): B = TypeBracketsContext.within(autoPos(inBrackets(commaSeparated(part).reduceWith(f))))

  /* ------------- POSITION HANDLING ------------------------------------------- */

  case object AutoPos extends Pos {
    def begIndex = currIndex
    def endIndex = prevIndex
  }
  implicit def intToIndexPos(index: Int): Pos = new IndexPos(index)
  implicit def treeToTreePos(tree: Tree): Pos = new TreePos(tree)
  implicit def optionTreeToPos(tree: Option[Tree]): Pos = tree.fold[Pos](AutoPos)(treeToTreePos)
  implicit def modsToPos(mods: List[Mod]): Pos = mods.headOption

  def atPos[T <: Tree](start: StartPos, end: EndPos)(body: => T): T =
    atPos(start.begIndex, end)(body)
  def atPosIf[T <: Tree](start: StartPos, end: EndPos)(body: => Option[T]): Option[T] =
    atPosIf(start.begIndex, end)(body)
  def atPosOpt[T <: Tree](start: StartPos, end: EndPos)(body: => T): T =
    atPosOpt(start.begIndex, end)(body)

  @inline
  def atPos[T <: Tree](start: Int, end: EndPos)(body: T): T =
    atPosWithBody(start, body, end.endIndex)
  def atPosIf[T <: Tree](start: Int, end: EndPos)(body: Option[T]): Option[T] = body
    .map(atPos(start, end))
  def atPosOpt[T <: Tree](start: Int, end: EndPos)(body: T): T = body.origin match {
    case o: Origin.Parsed if o.source eq originSource => body
    case _ => atPos(start, end)(body)
  }
  def atPos[T <: Tree](pos: Int)(body: => T): T = atPosWithBody(pos, body, pos)
  def atCurPos[T <: Tree](body: => T): T = atPos(currIndex)(body)
  def atCurPosNext[T <: Tree](body: => T): T = nextAfter(atCurPos(body))

  def atPosEmpty[T <: Tree](pos: Int)(body: => T): T = atPosWithBody(pos, body, pos - 1)
  def atPosEmpty[T <: Tree](pos: StartPos)(body: => T): T = atPosEmpty(pos.begIndex)(body)
  def atCurPosEmpty[T <: Tree](body: => T): T = atPosEmpty(currIndex)(body)
  def atCurPosEmptyNext[T <: Tree](body: => T): T = nextAfter(atCurPosEmpty(body))

  private val originSource = new Origin.ParsedSource(input)

  private def asOrigin(beg: Int, end: Int): Origin = Origin.Parsed(originSource, beg, end)
  private def asOrigin(idx: Int): Origin = asOrigin(idx, idx + 1)

  private def asString(token: Token, origin: Origin): Lit.String = Lit.String
    ._ctor(origin = origin, value = token.text)
  private def asString(token: Token, idx: Int): Lit.String = asString(token, asOrigin(idx))
  private def asString(token: CommentUnquote, idx: Int): Lit.String =
    unquoteAt[Lit.String](idx, token)

  private def asComment(parts: List[Lit.String], origin: Origin): Tree.Comment = Tree.Comment
    ._ctor(origin = origin, parts = parts)

  private def asComment(token: Token, idx: Int): Tree.Comment = {
    val origin = asOrigin(idx)
    asComment(asString(token, origin) :: Nil, origin)
  }

  private def asComment(parts: List[Lit.String], beg: Int, end: Int): Tree.Comment =
    asComment(parts, asOrigin(beg, end))

  private def asComments(values: ListBuffer[Tree.Comment]): Option[Tree.Comments] =
    if (values.isEmpty) None
    else Some {
      val origin = asOrigin(values.head.begIndex, values.last.endIndex + 1)
      Tree.Comments._ctor(origin = origin, values = values.toList)
    }

  def atPosWithBody[T <: Tree](startPos: Int, body: T, endPos: Int): T = {
    def getPosRange(): (Int, Int) = { // uses "return"
      if (endPos < startPos) return (startPos, startPos)
      val endExcl = endPos + 1
      val nonSpaceEnd = tokens.rskipIf(_.is[Whitespace], endPos, startPos - 1)
      if (nonSpaceEnd < startPos) return (startPos, if (endPos == startPos) endPos else endExcl)
      val start = tokens.skipIf(_.is[Trivia], startPos, endExcl)
      if (start > endPos) return (startPos, nonSpaceEnd + 1)
      if (!tokens(nonSpaceEnd).is[Comment]) return (start, nonSpaceEnd + 1)
      val end = tokens.rskipIf(_.is[HTrivia], nonSpaceEnd - 1, start)
      (start, (if (tokens(end).is[AtEOLorF]) nonSpaceEnd else end) + 1)
    }
    val (start, endExcl) = getPosRange()

    val (begComment, endComment) =
      if (!options.captureComments) (None, None)
      else if (body.hasComments) (body.begComment, body.endComment)
      else {
        // we don't use comments attributed to a child
        var minChild: Tree = null
        var minChildBeg = Int.MaxValue
        var maxChild: Tree = null
        var maxChildEnd = -1
        body.children.foreach { x =>
          val pos = x.pos
          val beg = x.begComment.fold(pos)(_.pos).start
          val end = x.endComment.fold(pos)(_.pos).end
          if (beg < end) {
            if (beg < minChildBeg) {
              minChild = x
              minChildBeg = beg
            }
            if (end > maxChildEnd) {
              maxChild = x
              maxChildEnd = end
            }
          }
        }

        val begComment =
          if (start < minChildBeg) {
            val begBuf = new ListBuffer[Tree.Comment]
            var idx = start - 1
            var pending = 0
            while (tokens.getOpt(idx) match {
                case Some(t: Comment) =>
                  begBuf.prepend(asComment(t, idx))
                  pending += 1
                  true
                case Some(endPart: CommentEnd) =>
                  val endIdx = idx + 1
                  val parts = new ListBuffer[Lit.String]
                  parts.prepend(asString(endPart, idx))
                  while ({
                    idx -= 1
                    tokens.getOpt(idx) match {
                      case Some(t: CommentStart) =>
                        parts.prepend(asString(t, idx))
                        begBuf.prepend(asComment(parts.toList, idx, endIdx))
                        pending += 1
                        false
                      case Some(t: CommentPart) =>
                        parts.prepend(asString(t, idx))
                        true
                      case Some(t: CommentUnquote) =>
                        parts.prepend(asString(t, idx))
                        true
                      case _ => false
                    }
                  }) {}
                  idx += 1
                  true
                case Some(_: AtEOL) =>
                  pending = 0
                  true
                case Some(_: HSpace) => true
                case Some(t) =>
                  if (t.isAny[Ident, CloseDelim] || body.is[Term.Block]) begBuf.remove(0, pending)
                  false
                case _ => false
              }) idx -= 1
            asComments(begBuf)
          } else minChild.begComment

        val endComment =
          if (endExcl > maxChildEnd) {
            val endBuf = new ListBuffer[Tree.Comment]
            var idx = endExcl
            while (tokens.getOpt(idx) match {
                case Some(t: Comment) =>
                  endBuf.append(asComment(t, idx))
                  true
                case Some(begPart: CommentStart) =>
                  val begIdx = idx
                  val parts = new ListBuffer[Lit.String]
                  parts.append(asString(begPart, idx))
                  while ({
                    idx += 1
                    tokens.getOpt(idx) match {
                      case Some(t: CommentEnd) =>
                        parts.append(asString(t, idx))
                        endBuf.prepend(asComment(parts.toList, begIdx, idx + 1))
                        false
                      case Some(t: CommentPart) =>
                        parts.append(asString(t, idx))
                        true
                      case Some(t: CommentUnquote) =>
                        parts.append(asString(t, idx))
                        true
                      case _ => false
                    }
                  }) {}
                  idx -= 1
                  true
                case Some(_: HSpace) => true
                case _ => false
              }) idx += 1
            asComments(endBuf)
          } else maxChild.endComment

        (begComment, endComment)
      }

    body.privateCopy(
      origin = asOrigin(start, endExcl),
      begComment = begComment,
      endComment = endComment
    ).asInstanceOf[T]
  }

  def atPosTry[T <: Tree](start: StartPos, end: EndPos)(body: => Try[T]): Try[T] = {
    val startTokenPos = start.begIndex
    body.map(atPos(startTokenPos, end))
  }
  def atPosTryOpt[T <: Tree](start: StartPos, end: EndPos)(body: => Try[T]): Try[T] = {
    val startTokenPos = start.begIndex
    body.map(atPosOpt(startTokenPos, end))
  }

  def autoPos[T <: Tree](body: => T): T = atPos(start = AutoPos, end = AutoPos)(body)
  def autoPosIf[T <: Tree](body: => Option[T]): Option[T] =
    atPosIf(start = AutoPos, end = AutoPos)(body)
  def autoPosOpt[T <: Tree](body: => T): T = atPosOpt(start = AutoPos, end = AutoPos)(body)
  @inline
  def autoEndPos[T <: Tree](start: Int)(body: => T): T = atPos(start = start, end = AutoPos)(body)
  @inline
  def autoEndPosOpt[T <: Tree](start: Int)(body: => T): T =
    atPosOpt(start = start, end = AutoPos)(body)
  @inline
  def autoEndPos[T <: Tree](start: StartPos)(body: => T): T = autoEndPos(start.begIndex)(body)
  @inline
  def autoEndPosOpt[T <: Tree](start: StartPos)(body: => T): T = autoEndPosOpt(start.begIndex)(body)
  @inline
  def autoPrevPos[T <: Tree](body: => T) = autoEndPos(prevIndex)(body)

  def autoPosTry[T <: Tree](body: => Try[T]): Try[T] = atPosTry(start = AutoPos, end = AutoPos)(body)

  /* ------------- ERROR HANDLING ------------------------------------------- */

  final lazy val reporter = Reporter()
  import this.reporter._

  implicit class XtensionToken(tok: Token) {
    def is[T](implicit ctag: ClassTag[T]) = ctag.runtimeClass.isAssignableFrom(tok.getClass())
    def as[T <: AnyRef: ClassTag]: T = (if (is[T]) tok else null).asInstanceOf[T]

    def isAny[A: ClassTag, B: ClassTag] = is[A] || is[B]
    def isAny[A: ClassTag, B: ClassTag, C: ClassTag] = is[A] || is[B] || is[C]
  }

  @inline
  private def at[T: ClassTag]: Boolean = currToken.is[T]
  @inline
  private def at[A: ClassTag, B: ClassTag]: Boolean = currToken.isAny[A, B]
  @inline
  private def at[A: ClassTag, B: ClassTag, C: ClassTag]: Boolean = currToken.isAny[A, B, C]
  @inline
  private def peek[T: ClassTag]: Boolean = peekToken.is[T]
  @inline
  private def peek[A: ClassTag, B: ClassTag]: Boolean = peekToken.isAny[A, B]
  @inline
  private def prev[T: ClassTag]: Boolean = prevToken.is[T]

  private def syntaxErrorExpected[T <: Token: ClassTag]: Nothing = syntaxErrorExpected[T](currToken)
  private def syntaxErrorExpected[T <: Token: ClassTag](tok: Token): Nothing =
    syntaxError(syntaxExpectedMessage[T](tok), at = tok)
  private def expectAt[T <: Token: ClassTag](
      tok: Token,
      exists: Boolean
  )(msg: => String, prefix: => String): Unit = if (tok.is[T] != exists)
    syntaxError(Option(prefix).filter(_.nonEmpty).fold(msg)(p => s"$p: $msg"), at = tok)

  @inline
  private def expectAt[T <: Token: ClassTag](tok: Token, prefix: => String = ""): Unit =
    expectAt[T](tok, exists = true)(msg = syntaxExpectedMessage[T](tok), prefix = prefix)
  @inline
  private def expect[T <: Token: ClassTag]: Unit = expectAt[T](currToken)
  @inline
  private def expect[T <: Token: ClassTag](prefix: => String): Unit =
    expectAt[T](currToken, prefix = prefix)

  @inline
  private def expectNotAt[T <: Token: ClassTag](tok: Token, prefix: => String = ""): Unit =
    expectAt[T](tok, exists = false)(msg = syntaxNotExpectedMessage[T], prefix = prefix)
  @inline
  private def expectNot[T <: Token: ClassTag]: Unit = expectNotAt[T](currToken)
  @inline
  private def expectNot[T <: Token: ClassTag](prefix: => String): Unit =
    expectNotAt[T](currToken, prefix = prefix)

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept[T <: Token: ClassTag]: Unit = {
    expect[T]
    if (!at[EOF]) next()
  }

  /** If current token is T consume it. */
  @inline
  private def acceptOpt[T: ClassTag]: Boolean = nextIf(at[T])

  private def acceptAs[T <: AnyRef: ClassTag]: T = {
    val res = currToken.as[T]
    if (res ne null) next()
    res
  }

  /** If current token is T consume it. */
  @inline
  private def acceptIf(unapply: Token => Boolean): Boolean = nextIf(unapply(currToken))

  private def acceptIfAfterOpt(unapplyA: Token => Boolean, unapplyB: Token => Boolean): Boolean =
    if (unapplyA(currToken)) {
      next()
      true
    } else if (unapplyB(currToken) && unapplyA(peekToken)) {
      nextTwice()
      true
    } else false

  private def acceptIfAfterOpt[A <: Token: ClassTag](unapply: Token => Boolean): Boolean =
    acceptIfAfterOpt(_.is[A], unapply)

  private def acceptIfAfterOpt[A <: Token: ClassTag, B <: Token: ClassTag]: Boolean =
    acceptIfAfterOpt[A](_.is[B])

  @inline
  private def acceptIfAfterOptNL[T <: Token: ClassTag]: Boolean = acceptIfAfterOpt[T, AtEOL]

  @inline
  private def acceptAfterOpt[A <: Token: ClassTag, B <: Token: ClassTag]: Unit = {
    if (at[B]) next()
    accept[A]
  }

  @inline
  private def acceptAfterOptNL[T <: Token: ClassTag]: Unit = acceptAfterOpt[T, AtEOL]

  def isAtEndMarker(): Boolean = isEndMarkerIntro(currToken, peekIndex)

  @inline
  def acceptIfStatSep(): Boolean = acceptIf(StatSep)

  @inline
  def isImplicitStatSep(): Boolean = prev[Indentation.Outdent]

  def acceptIfExtendedStatSep(): Boolean = acceptIfStatSep() || isAtEndMarker() ||
    isImplicitStatSep() && !at[Indentation.Outdent]

  def acceptExtendedStatSep(): Unit = if (!acceptIfExtendedStatSep()) syntaxErrorExpected[Semicolon]
  def acceptExtendedStatSepOpt(): Boolean = {
    val ok = !StatSeqEnd(currToken)
    if (ok) acceptExtendedStatSep()
    ok
  }

  def skipAllStatSep(): Boolean = {
    val ok = acceptIfStatSep()
    while (acceptIfStatSep()) {}
    ok
  }

  /* -------------- TOKEN CLASSES ------------------------------------------- */

  @inline
  def isStar: Boolean = isStar(currToken)
  def isStar(tok: Token): Boolean = Keywords.Star(tok)

  private object MacroSplicedIdent {
    final def unapply(tok: Ident): Option[Term] =
      if (dialect.allowSpliceAndQuote) {
        val value = tok.text
        if (value.isEmpty || value.charAt(0) != '$') None
        else if (value.length > 1)
          if (QuotedSpliceContext.isInside())
            Some(autoPos(macroIdent(value.substring(1), Term.SplicedMacroExpr.apply)))
          else None
        else peekToken match {
          case _: LeftBrace => Some(autoPos {
              next()
              if (PatternContext.isInside()) Term.SplicedMacroPat(autoPos(inBracesOnOpen(pattern())))
              else Term.SplicedMacroExpr(autoPos(inBracesOnOpen(blockRaw())))
            })
          case t: Ident if t.value.nonEmpty && QuotedSpliceContext.isInside() =>
            Some(autoPos(next(macroIdent(t.value, Term.SplicedMacroExpr.apply))))
          case _ => None
        }
      } else None
  }

  private object MacroQuoted {
    final def unapply(tok: MacroQuote): Some[Term] = QuotedSpliceContext.within(Some(autoPos {
      next()
      currToken match {
        case _: LeftBrace => Term.QuotedMacroExpr(autoPos(inBracesOnOpen(blockRaw())))
        case _: LeftBracket => Term.QuotedMacroType(inBracketsOnOpen(typeBlock()))
        case t: Ident => macroIdent(t.value, Term.QuotedMacroExpr.apply)
        case t => syntaxError("Macro quote must be followed by id, brace or bracket", at = t)
      }
    }))
  }

  private object InfixTypeIdent {
    def unapply(tok: Token.Ident): Boolean = tok.text match {
      case soft.KwPureFunctionLikeArrow() => false
      case "*" => // we assume that this is a type specification for a vararg parameter
        peekToken match {
          case _: RightParen | _: Comma | _: Equals | _: RightBrace | _: EOF => false
          case _ => true
        }
      case _ => true
    }
  }

  /* ---------- TREE CONSTRUCTION ------------------------------------------- */

  private def listBy[T](f: ListBuffer[T] => Unit): List[T] = {
    val buf = new ListBuffer[T]
    f(buf)
    buf.toList
  }

  def ellipsis[T <: Tree: AstInfo: ClassTag](ell: Ellipsis, rank: Int, extraSkip: => Unit = {}): T = {
    if (ell.rank != rank) syntaxError(Messages.QuasiquoteRankMismatch(ell.rank, rank), at = ell)
    ellipsis(ell, extraSkip)
  }

  def ellipsis[T <: Tree: AstInfo: ClassTag](ell: Ellipsis): T with Quasi = ellipsis(ell, {})

  def ellipsis[T <: Tree: AstInfo: ClassTag](ell: Ellipsis, extraSkip: => Unit): T with Quasi =
    autoPos {
      if (!dialect.allowUnquotes) syntaxError(s"$dialect doesn't support ellipses", at = ell)
      next()
      extraSkip
      // unquote returns a rank=0 quasi tree
      val tree = currToken match {
        case _: LeftParen => inParensOnOpen(unquote[T])
        case _: LeftBrace => inBracesOnOpen(unquote[T])
        case t: Unquote => unquote[T](t)
        case t => syntaxError(s"$$, ( or { expected but ${t.name} found", at = t)
      }
      // NOTE: In the case of an unquote nested directly under ellipsis, we get a bit of a mixup.
      // Unquote's pt may not be directly equal unwrapped ellipsis's pt, but be its refinement instead.
      // For example, in `new { ..$stats }`, ellipsis's pt is List[Stat], but quasi's pt is Term.
      // This is an artifact of the current implementation, so we just need to keep it mind and work around it.
      assert(
        classTag[T].runtimeClass.isAssignableFrom(tree.pt),
        s"ellipsis: $ell,\ntree: $tree,\nstructure: ${tree.structure}"
      )
      quasi[T](ell.rank, tree)
    }

  private def unquoteAt[T <: Tree: AstInfo](idx: Int, unquote: Token): T with Quasi = {
    assert(unquote.input.chars(unquote.start + 1) != '$', "Expected unquote to start with $")
    val unquoteDialect = dialect.unquoteParentDialect
    if (null eq unquoteDialect) syntaxError(s"$dialect doesn't support unquotes", at = unquote)
    // NOTE: I considered having Input.Slice produce absolute positions from the get-go,
    // but then such positions wouldn't be usable with Input.Slice.chars.
    val unquotedTree = atPos(idx) {
      val unquoteInput = Input.Slice(input, unquote.start + 1, unquote.end)
      try {
        val unquoteParser = {
          implicit val dialect: Dialect = unquoteDialect
          new ScalametaParser(unquoteInput)
        }
        if (dialect.allowTermUnquotes) unquoteParser.parseUnquoteTerm()
        else if (dialect.allowPatUnquotes) unquoteParser.parseUnquotePat()
        else unreachable
      } catch { case ex: Exception => throw ex.absolutize }
    }
    copyPos(unquotedTree)(quasi[T](0, unquotedTree))
  }

  private def unquote[T <: Tree: AstInfo](unquote: Token): T with Quasi = {
    next()
    unquoteAt[T](prevIndex, unquote)
  }

  def unquote[T <: Tree: AstInfo]: T with Quasi = currToken match {
    case t: Unquote => unquote[T](t)
    case t => unreachable(t)
  }

  def unquoteOpt[T <: Tree: AstInfo]: Option[T with Quasi] = currToken match {
    case t: Unquote => Some(unquote[T](t))
    case _ => None
  }

  def unquoteOpt[T <: Tree: AstInfo](pred: => Boolean): Option[T with Quasi] = currToken match {
    case t: Unquote if pred => Some(unquote[T](t))
    case _ => None
  }

  final def tokenSeparated[Sep: ClassTag, T <: Tree: AstInfo: ClassTag](
      sepFirst: Boolean,
      part: Int => T
  ): List[T] = listBy[T] { ts =>
    @tailrec
    def iter(sep: Boolean): Unit = currToken match {
      case t: Ellipsis =>
        ts += ellipsis[T](t, 1)
        iter(false)
      case _ if sep =>
        ts += part(ts.length)
        iter(false)
      case _ if acceptOpt[Sep] => iter(true)
      case _ =>
    }
    iter(!sepFirst)
  }

  @inline
  final def commaSeparated[T <: Tree: AstInfo: ClassTag](part: => T): List[T] =
    commaSeparatedWithIndex(_ => part)

  @inline
  final def commaSeparatedWithIndex[T <: Tree: AstInfo: ClassTag](part: Int => T): List[T] =
    tokenSeparated[Comma, T](sepFirst = false, part)

  private def makeTuple[A <: Tree](lpPos: Int, body: List[A], zero: => A, tuple: List[A] => A)(
      single: A => Either[List[A], A]
  ): A = body match {
    case Nil => autoEndPos(lpPos)(zero)
    case (q: Quasi) :: Nil if q.rank == 1 => copyPos(q)(tuple(body))
    case t :: Nil => single(t) match {
        case Right(x) => x
        case Left(x) => autoEndPos(lpPos)(tuple(x))
      }
    case _ => autoEndPos(lpPos)(tuple(body))
  }

  private def getTupleSingleTerm(single: Term): Either[List[Term], Term] = {
    val force = single match {
      // https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html#source-incompatibilities
      case _: Term.Assign => dialect.allowNamedTuples
      case _ => false
    }
    if (force) Left(single :: Nil) else Right(single)
  }

  private def makeTupleTerm(
      single: Term => Either[List[Term], Term]
  )(lpPos: Int, body: List[Term]): Term =
    makeTuple(lpPos, body, Lit.Unit(), Term.Tuple.apply)(single)

  private def makeTupleTerm(lpPos: Int, body: List[Term]): Term =
    makeTupleTerm(getTupleSingleTerm)(lpPos, body)

  private def keepTupleType(single: Type): Boolean = single match {
    case t: Type.TypedParam => dialect.allowNamedTuples
    case _ => false
  }

  private def makeTupleType(lpPos: Int, body: List[Type], zero: => Type): Type =
    makeTuple(lpPos, body, zero, Type.Tuple.apply) { x =>
      val single = maybeAnonymousLambda(x)
      if (keepTupleType(single)) Left(single :: Nil) else Right(single)
    }

  private def makeTupleType(lpPos: Int, body: List[Type]): Type = {
    def invalidLiteralUnitType =
      syntaxError("illegal literal type (), use Unit instead", at = currToken.pos)
    makeTupleType(lpPos, body, invalidLiteralUnitType)
  }

  private def inParensOrTupleOrUnitExpr(allowRepeated: Boolean): Term = {
    val lpPos = currIndex
    val maybeTupleArgs = inParensOnOpenOr(
      commaSeparated(expr(location = PostfixStat, allowRepeated = allowRepeated))
    )(Nil)
    if (maybeTupleArgs.lengthCompare(1) > 0) maybeTupleArgs.foreach {
      case arg: Term.Repeated =>
        syntaxError("repeated argument not allowed here", at = arg.tokens.last)
      case _ =>
    }
    makeTupleTerm(x => getTupleSingleTerm(maybeAnonymousFunction(x)))(lpPos, maybeTupleArgs)
  }

  /* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

  /**
   * Methods which implicitly propagate the context in which they were called: either in a pattern
   * context or not. Formerly, this was threaded through numerous methods as boolean isPattern.
   */
  trait PatternContextSensitive {
    private def tupleInfixType(allowFunctionType: Boolean = true): Type = autoPosOpt {
      // NOTE: This is a really hardcore disambiguation caused by introduction of Type.Method.
      // We need to accept `(T, U) => W`, `(x: T): x.U` and also support unquoting.
      var hasImplicits = false

      def modOrType(modsBuf: mutable.Builder[Mod, List[Mod]]): Type = currToken match {
        case _: KwImplicit if !hasImplicits && allowFunctionType =>
          next()
          hasImplicits = true
          null
        case soft.KwErased() if allowFunctionType =>
          modsBuf += atCurPosNext(Mod.Erased())
          null
        case _ =>
          val mods = modsBuf.result()
          val tpe = maybeParamType(allowFunctionType)
          if (mods.isEmpty) tpe else autoEndPos(mods.head)(Type.FunctionArg(mods, tpe))
      }

      @tailrec
      def paramOrType(modsBuf: mutable.Builder[Mod, List[Mod]]): Type =
        namedTypeOpt(modsBuf.result(), allowFunctionType = allowFunctionType) match {
          case Some(x) => x
          case None =>
            val x = modOrType(modsBuf)
            if (x ne null) x else paramOrType(modsBuf)
        }

      val openParenPos = currIndex
      // NOTE: can't have this, because otherwise we run into #312
      // newLineOptWhenFollowedBy[LeftParen]

      def makeTuple(ts: List[Type]) = makeTupleType(openParenPos, ts)

      val ts = {
        val ts = inParensOr(commaSeparated(paramOrType(List.newBuilder[Mod])))(Nil)

        var hasTypes = false
        var hasParams = false
        ts.foreach {
          case _: Quasi =>
          case _: Type.TypedParam => hasParams = true
          case _ => hasTypes = true
        }
        if (hasTypes && hasParams)
          syntaxError("can't mix function type and dependent function type syntaxes", at = currToken)
        if (hasParams && !dialect.allowDependentFunctionTypes)
          syntaxError("dependent function types are not supported", at = currToken)
        if (!hasTypes && !hasImplicits && at[LeftParen]) {
          val message = "can't have multiple parameter lists in function types"
          syntaxError(message, at = currToken)
        }

        withTypeCapturesOpt(openParenPos, needCaret = true)(makeTuple(ts)).fold(ts)(_ :: Nil)
      }

      def maybeFunc = getAfterOptNewLine(typeFuncOnArrowOpt(openParenPos, ts))
      (if (allowFunctionType) maybeFunc else None).getOrElse {
        val simple = simpleTypeRest(makeTuple(ts), openParenPos)
        val compound = compoundTypeRest(annotTypeRest(simple, openParenPos), openParenPos)
        infixTypeRest(compound) match {
          case `compound` => compound
          case t => autoEndPos(openParenPos)(t)
        }
      }
    }

    private def typeLambdaOrPoly(): Type = {
      val quants = typeParamClauseOpt()
      newLineOpt()
      currToken match {
        case _: TypeLambdaArrow =>
          next()
          Type.Lambda(quants, typeIndentedOpt())
        case t: RightArrow =>
          next()
          typeIndentedOpt() match {
            case tpe: Type.FunctionType => Type.PolyFunction(quants, tpe)
            case _ => syntaxError("polymorphic function types must have a value parameter", at = t)
          }
        case t => syntaxError("expected =>> or =>", at = t)
      }
    }

    def typeIndentedOpt(): Type = maybeIndented(typ())

    def typ(inParam: Boolean = false): Type = autoPosOpt {
      val startPos = currIndex
      val t: Type =
        if (at[LeftBracket] && dialect.allowTypeLambdas) typeLambdaOrPoly() else infixTypeOrTuple()

      getAfterOptNewLine(currToken match {
        case _: KwForsome => Some(existentialTypeOnForSome(t))
        case _: KwMatch if dialect.allowTypeMatch =>
          next()
          Some(Type.Match(t, typeCaseClauses()))
        case _ => typeFuncOnArrowOpt(startPos, t :: Nil, inParam = inParam)
      }).getOrElse(t)
    }

    private def paramValueType(allowRepeated: Boolean = false): Type = {
      val startPos = currIndex
      def withRepeated(t: Type) =
        if (allowRepeated && isStar) {
          next()
          autoEndPos(startPos)(Type.Repeated(t))
        } else t
      withRepeated(typ(inParam = true))
    }

    def paramType(): Type = {
      def byNameParamValueType =
        paramValueType(allowRepeated = dialect.allowByNameRepeatedParameters)
      currToken match {
        case _: RightArrow => autoPos {
            next()
            Type.ByName(byNameParamValueType)
          }
        case soft.KwPureFunctionArrow() =>
          val startPos = currIndex
          next()
          withTypeCaptures(startPos)(autoEndPos(startPos)(Type.PureByName(byNameParamValueType)))
        case _ => paramValueType(allowRepeated = true)
      }
    }

    private def maybeParamType(allowFunctionType: Boolean): Type =
      if (allowFunctionType) paramType() else typ()

    private def namedTypeOpt(
        fmods: => List[Mod],
        allowFunctionType: Boolean
    ): Option[Type.TypedParam] = currToken match {
      case t: Ident if peek[Colon] =>
        val startPos = currIndex
        nextTwice() // skip ident and colon
        val mods = fmods
        val modPos = if (mods.isEmpty) startPos else mods.head.begIndex
        val name = atPos(startPos)(Type.Name(t.value))
        val tpe = maybeParamType(allowFunctionType)
        Some(autoEndPos(modPos)(Type.TypedParam(name, tpe, mods)))
      case _ => None
    }

    def typeBlock(): Type =
      // TypeBlock, https://dotty.epfl.ch/docs/internals/syntax.html#expressions-3
      if (dialect.allowQuotedTypeVariables && at[KwType]) autoPos {
        val typeDefs = listBy[Stat.TypeDef](buf =>
          doWhile(buf += typeDefOrDcl(Nil))(acceptIfStatSep() && at[KwType])
        )
        Type.Block(typeDefs, typ())
      }
      else typ()

    private def typeCaptures(needCaret: Boolean, allowCaptures: Boolean): Option[Type.Captures] =
      if (allowCaptures && dialect.allowCaptureChecking) {
        def ifCaret[A](thenPart: => A, elsePart: => A): A = currToken match {
          case t: Token.Ident if t.text == "^" =>
            next()
            thenPart
          case _ => elsePart
        }
        def capturesOnBrace(): Option[Type.Captures] =
          if (!acceptOpt[Token.LeftBrace]) None
          else Some(autoEndPos(prevIndex)(Type.CapturesSet(
            if (acceptOpt[Token.RightBrace]) Nil
            else inBracesAfterOpen(commaSeparated(path() match {
              case x: Term.Name => ifCaret(autoEndPos(x)(Term.CapSetName(x.value)), x)
              case x => x
            }))
          )))
        if (!needCaret) capturesOnBrace()
        else ifCaret(capturesOnBrace().orElse(Some(atCurPosEmpty(Type.CapturesAny()))), None)
      } else None

    private def withTypeCapturesOpt(startPos: Int, needCaret: Boolean, allowCaptures: Boolean = true)(
        capturedType: => Type
    ): Option[Type] = typeCaptures(needCaret = needCaret, allowCaptures = allowCaptures)
      .map(captures => autoEndPos(startPos)(Type.Capturing(capturedType, captures)))

    private def withTypeCaptures(
        startPos: Int,
        needCaret: Boolean = false,
        allowCaptures: Boolean = true
    )(capturedType: => Type): Type =
      withTypeCapturesOpt(startPos, needCaret = needCaret, allowCaptures = allowCaptures)(
        capturedType
      ).getOrElse(capturedType)

    private def typeFuncOnArrow(
        paramPos: Int,
        params: List[Type],
        allowCaptures: Boolean = false,
        inParam: Boolean = false
    )(ctor: (Type.FuncParamClause, Type) => Type): Type = {
      val funcParams =
        autoEndPos(paramPos)(params.map(typeVar).reduceWith(Type.FuncParamClause.apply))
      next()
      withTypeCaptures(paramPos, allowCaptures = allowCaptures)(autoEndPos(paramPos)(
        ctor(funcParams, maybeIndented(if (inParam) paramValueType() else typ()))
      ))
    }

    private def typeFuncOnArrowOpt(
        paramPos: Int,
        params: List[Type],
        inParam: Boolean = false
    ): Option[Type] = {
      def func(caps: Boolean = true)(ctor: (Type.FuncParamClause, Type) => Type): Option[Type] =
        Some(typeFuncOnArrow(paramPos, params, inParam = inParam, allowCaptures = caps)(ctor))
      currToken match {
        case _: RightArrow => func(caps = false)(Type.Function(_, _))
        case _: ContextArrow => func(caps = false)(Type.ContextFunction(_, _))
        case t: Ident => t.text match {
            case soft.KwPureFunctionArrow() => func()(Type.PureFunction(_, _))
            case soft.KwPureContextFunctionArrow() => func()(Type.PureContextFunction(_, _))
            case _ => None
          }
        case _ => None
      }
    }

    private def typeCaseClauses(): Type.CasesBlock = autoPos {
      def cases() = listBy[TypeCase](allCases =>
        while (at[KwCase]) {
          allCases += autoPos {
            next()
            val pat = infixTypeOrTuple(inMatchType = true)
            accept[RightArrow]
            TypeCase(pat, typeIndentedOpt())
          }
          newLinesOpt()
        }
      )
      (if (at[Indentation.Indent]) indentedOnOpen(cases()) else inBraces(cases()))
        .reduceWith(Type.CasesBlock.apply)
    }

    def quasiquoteType(): Type = entrypointType()

    def entrypointType(): Type = paramType()

    private def typeArg(): Type = currToken match {
      case t: Ident if peek[Equals] =>
        val startPos = currIndex
        nextTwice() // skip ident and equals
        val name = atPos(startPos)(Type.Name(t.value))
        autoEndPos(startPos)(Type.Assign(name, typ()))
      case t => typeVar(typ(), t.text)
    }

    def typeArgsInBrackets(): Type.ArgClause = typeClauseInBrackets(typeArg(), Type.ArgClause.apply)

    def infixTypeOrTuple(inMatchType: Boolean = false): Type =
      if (at[LeftParen]) tupleInfixType(allowFunctionType = !inMatchType)
      else infixType(inMatchType = inMatchType)

    @inline
    def infixType(inMatchType: Boolean = false, inGivenSig: Boolean = false): Type =
      maybeAnonymousLambda(infixTypeRest(
        compoundType(inMatchType = inMatchType, inGivenSig = inGivenSig),
        inMatchType = inMatchType,
        inGivenSig = inGivenSig
      ))

    @inline
    private def infixTypeRest(
        t: Type,
        inMatchType: Boolean = false,
        inGivenSig: Boolean = false
    ): Type =
      if (dialect.useInfixTypePrecedence)
        infixTypeRestWithPrecedence(t, inMatchType = inMatchType, inGivenSig = inGivenSig)
      else infixTypeRestImpl(t, identity, inMatchType = inMatchType, inGivenSig = inGivenSig)

    @tailrec
    private final def infixTypeRestImpl(
        t: Type,
        f: Type => Type,
        inMatchType: Boolean = false,
        inGivenSig: Boolean = false
    ): Type = {
      val ok = currToken match {
        case _: Unquote | InfixTypeIdent() => true
        case _ => false
      }
      if (ok) {
        val op = typeName()
        newLineOptWhenFollowedBy(TypeIntro)
        val typ = compoundType(inMatchType = inMatchType, inGivenSig = inGivenSig)
        def mkOp(t1: Type) = atPos(t, t1)(Type.ApplyInfix(t, op, t1))
        if (op.isLeftAssoc)
          infixTypeRestImpl(mkOp(typ), f, inMatchType = inMatchType, inGivenSig = inGivenSig)
        else
          infixTypeRestImpl(typ, f.compose(mkOp), inMatchType = inMatchType, inGivenSig = inGivenSig)
      } else f(t)
    }

    private final def infixTypeRestWithPrecedence(
        t: Type,
        inMatchType: Boolean = false,
        inGivenSig: Boolean = false
    ): Type = {
      import TypeInfixContext._
      val base = stack
      @inline
      def reduce(rhs: Typ, op: Option[Op]): Typ = reduceStack(base, rhs, rhs, op)
      def getNextRhs(rhs: Typ)(op: Op): Typ = {
        newLineOptWhenFollowedBy(TypeIntro)
        push(UnfinishedInfix(reduce(rhs, Some(op)), op))
        compoundType(inMatchType = inMatchType, inGivenSig = inGivenSig)
      }
      @tailrec
      def loop(rhs: Typ): Typ = (currToken match {
        case lf: InfixLF => getLeadingInfix(lf)(Type.Name.apply)(getNextRhs(rhs))
        case _: Unquote | InfixTypeIdent() => Some(getNextRhs(rhs)(typeName()))
        case _ => None
      }) match {
        case Some(x) => loop(x)
        case None => reduce(rhs, None)
      }
      loop(t)
    }

    def compoundType(inMatchType: Boolean = false, inGivenSig: Boolean = false): Type =
      refinement(innerType = None).getOrElse {
        val startPos = currIndex
        val annot = annotType(startPos, inMatchType = inMatchType)
        if (inGivenSig) annot else compoundTypeRest(annot, startPos)
      }

    def compoundTypeRest(typ: Type, startPos: Int): Type = {
      @tailrec
      def gatherWithTypes(previousType: Type): Type = refinement(Some(previousType)) match {
        /* Indentation means a refinement and we cannot join
         * refinements this way so stop looping.
         */
        case None | Some(`previousType`) =>
          if (acceptOpt[KwWith]) {
            val rhs = annotType()
            val t = autoEndPos(startPos)(Type.With(previousType, rhs))
            gatherWithTypes(t)
          } else previousType
        case Some(t) => t
      }

      gatherWithTypes(typ)
    }

    def annotType(inMatchType: Boolean = false): Type =
      annotType(currIndex, inMatchType = inMatchType)

    private def annotType(startPos: Int, inMatchType: Boolean): Type =
      annotTypeRest(simpleType(inMatchType = inMatchType), startPos)

    def annotTypeRest(t: Type, startPos: Int): Type = {
      val annots = ScalametaParser.this.annots(skipNewLines = false)
      if (annots.isEmpty) t else autoEndPos(startPos)(Type.Annotate(t, annots))
    }

    def simpleType(inMatchType: Boolean = false): Type = {
      val startPos = currIndex
      def wildcardType(): Type = {
        next()
        Type.Wildcard(typeBounds())
      }
      def anonymousParamWithVariant(modOpt: Option[Mod.Variant]): Type = Type
        .AnonymousParam(modOpt.map(v => atPos(startPos)(v)))
      def pathSimpleType(): Type = {
        val ref = path()
        if (acceptOpt[Dot]) {
          accept[KwType]
          Type.Singleton(ref.become[Term.Ref])
        } else ref match {
          case q: Quasi => q.become[Type]
          case Term.Select(qual: Term.Quasi, name: Term.Name.Quasi) =>
            val newQual = qual.become[Term.Ref]
            val newName = name.become[Type.Name]
            Type.Select(newQual, newName)
          case Term.Select(qual: Term.Ref, name) =>
            val newName = name.becomeOr(x => copyPos(x)(Type.Name(x.value)))
            Type.Select(qual, newName)
          case name: Term.Name => Type.Name(name.value)
          case _ => syntaxError("identifier expected", at = ref)
        }
      }
      val res = currToken match {
        case _: Ident if peek[Dot] => pathSimpleType()
        case _: LeftParen => makeTupleType(startPos, typesInParens())
        case MacroSplicedIdent(term) => Type.Macro(term)
        case _: Underscore if inMatchType =>
          next()
          Type.PatWildcard()
        case _: Underscore
            if dialect.allowUnderscoreAsTypePlaceholder ||
              dialect.allowTypeLambdas && !PatternTypeContext.isInside() &&
              TypeBracketsContext.isDeeper(1) && !peek[Supertype, Subtype] =>
          next()
          Type.AnonymousParam(None)
        case _: Underscore => wildcardType()
        case _: Literal =>
          if (dialect.allowLiteralTypes) literal()
          else syntaxError(s"$dialect doesn't support literal types", at = path())
        case Unary(unary) if dialect.allowLiteralTypes && tryAhead[Literal] =>
          autoEndPos(prevIndex)(rawLiteral(unary))
        case t: Token.Ident if !inMatchType =>
          t.text match {
            case soft.QuestionMarkAsTypeWildcard() => wildcardType()
            case soft.StarAsTypePlaceholder(value) =>
              next()
              anonymousParamWithVariant(value)
            case value @ TParamVariantStr(mod)
                if (dialect.allowPlusMinusUnderscoreAsIdent ||
                  dialect.allowUnderscoreAsTypePlaceholder) && tryAhead[Underscore] =>
              next() // Ident and Underscore
              if (dialect.allowUnderscoreAsTypePlaceholder) anonymousParamWithVariant(Some(mod))
              else Type.Name(s"${value}_")
            case _ => pathSimpleType()
          }
        case _ => pathSimpleType()
      }
      val rest = simpleTypeRest(autoEndPosOpt(startPos)(res), startPos)
      withTypeCaptures(startPos, needCaret = true)(rest)
    }

    @tailrec
    private final def simpleTypeRest(t: Type, startPos: Int): Type = {
      def onLeftBracket() = autoEndPos(startPos)(Type.Apply(t, typeArgsInBrackets()))
      currToken match {
        case _: Hash =>
          next()
          simpleTypeRest(autoEndPos(startPos)(Type.Project(t, typeName())), startPos)
        case _: LeftBracket => simpleTypeRest(onLeftBracket(), startPos)
        case _: AtEOL if isIndentingOrEOL(nonOptBracesOK = true) && tryAhead[LeftBracket] =>
          simpleTypeRest(onLeftBracket(), startPos)
        case _ => t
      }
    }

    def typesInParens(): List[Type] =
      inParensOnOpen(commaSeparated(namedTypeOpt(Nil, allowFunctionType = false).getOrElse(typ())))

    private def infixPatternTypeImpl(): Type = {
      val t = if (at[LeftParen]) tupleInfixType() else compoundType()
      currToken match {
        case _: KwForsome => existentialTypeOnForSome(t)
        case _: Unquote | Keywords.NotPatAlt() => infixTypeRest(t)
        case _ => t
      }
    }

    def patternTyp(allowInfix: Boolean): Type = PatternTypeContext
      .within(if (allowInfix) infixPatternTypeImpl() else compoundType())

    def patternTypeArgs() = PatternTypeContext.within(
      typeClauseInBrackets(typeVar(infixPatternTypeImpl(), prevToken.text), Type.ArgClause.apply)
    )

    private def typeVar(t: Type, text: => String): Type = t match {
      case _: Quasi => t
      case t: Type.Name
          if PatternTypeContext.isInside() && TypeBracketsContext.isInside() && text.isPatVar =>
        copyPos(t)(Type.Var(t))
      case _ => t
    }

    private def typeVar(t: Type): Type = typeVar(t, t.text)
  }

  private trait AllowedName[T]
  private object AllowedName {
    implicit object AllowedTermName extends AllowedName[Term.Name]
    implicit object AllowedTypeName extends AllowedName[Type.Name]
  }

  private def identName[T <: Tree](ident: Ident, ctor: String => T): T =
    atCurPosNext(ctor(ident.value))
  private def name[T <: Tree: AllowedName: AstInfo](ctor: String => T): T = currToken match {
    case t: Ident => identName(t, ctor)
    case t: Unquote => unquote[T](t)
    case _ => syntaxErrorExpected[Ident]
  }

  def termName(): Term.Name = name(Term.Name(_))
  def typeName(): Type.Name = name(Type.Name(_))
  private def nameAsType(t: Name): Type.Name = copyPos(t)(Type.Name(t.value))
  private def termName(t: Ident): Term.Name = identName(t, Term.Name.apply)
  private def typeName(t: Ident): Type.Name = identName(t, Type.Name.apply)
  @inline
  private def anonNameRaw(): Name.Anonymous = Name.Anonymous()
  @inline
  private def anonName(): Name.Anonymous = atCurPosEmpty(anonNameRaw())
  @inline
  private def anonNameAt(pos: StartPos): Name.Anonymous = atPosEmpty(pos)(anonNameRaw())
  @inline
  private def nameThis(): Name.This = atCurPosNext(Name.This())
  @inline
  private def namePlaceholder(): Name.Placeholder = atCurPosNext(Name.Placeholder())
  private def anonThis(): Term.This = atCurPosNext(Term.This(anonName()))

  def path(thisOK: Boolean = true): Term.Ref = {
    val startsAtBof = prev[BOF]
    def afterDot[A <: Term.Ref](ref: A)(f: PartialFunction[Token, A => Term.Ref]): Term.Ref =
      if (!at[Dot]) ref
      else f.lift(peekToken) match {
        case Some(f) =>
          next()
          f(ref)
        case _ => ref
      }
    @inline
    def maybeSelectors(ref: Term.Ref): Term.Ref = afterDot(ref) { case _: Ident | _: Unquote =>
      selectors
    }
    def getThis(name: Name): Term.Ref = {
      val thisp = autoEndPos(name)(Term.This(name))
      if (thisOK) maybeSelectors(thisp)
      else {
        accept[Dot]
        selectors(thisp)
      }
    }
    def getSuper(name: Name): Term.Ref = {
      val superp = autoEndPos(name)(Term.Super(name, mixinQualifier()))
      if (startsAtBof && dialect.allowUnquotes && at[EOF]) superp
      else {
        accept[Dot]
        maybeSelectors(autoEndPos(name)(Term.Select(superp, termName())))
      }
    }
    def getAnonQual(): Name = nextAfter(anonName())
    def getQual(name: Term.Name): Name = {
      next()
      name.becomeOr[Name](x => copyPos(x)(Name.Indeterminate(x.value)))
    }
    currToken match {
      case _: KwThis => getThis(getAnonQual())
      case _: KwSuper => getSuper(getAnonQual())
      case _ => afterDot(termName()) {
          case _: KwThis => x => getThis(getQual(x))
          case _: KwSuper => x => getSuper(getQual(x))
          case _: Ident | _: Unquote => x => selectors(x.become[Term])
        }
    }
  }

  def selector(t: Term, startPos: Int): Term.Select = autoEndPos(startPos)(Term.Select(t, termName()))
  @tailrec
  private final def selectors(t: Term, startPos: Int): Term.Ref = {
    val t1 = selector(t, startPos)
    if (at[Dot] && tryAhead[Ident]) selectors(t1, startPos) else t1
  }
  private final def selectors(t: Term): Term.Ref = selectors(t, t.begIndex)

  def mixinQualifier(): Name =
    if (acceptOpt[LeftBracket])
      inBracketsAfterOpen(typeName().becomeOr[Name](x => copyPos(x)(Name.Indeterminate(x.value))))
    else anonName()

  def stableId(): Term.Ref = path(thisOK = false)

  def qualId(): Term.Ref = {
    val name = termName().become[Term.Ref]
    if (acceptOpt[Dot]) selectors(name) else name
  }

  private def rawNumericLiteral(unary: Unary.Numeric): Either[Lit, Lit] = {
    def withUnary(lit: Lit) = Either.cond(unary ne Unary.Noop, lit, lit)
    def getBigDecimal(tok: NumericConstant[BigDecimal], f: String => Lit) = unary(tok.value) match {
      case Some(x) => withUnary(f(x.toString()))
      case _ => Left(f(tok.value.toString()))
    }
    currToken match {
      case tok: Constant.Int => withUnary(Lit.Int(unary(tok.value).intValue))
      case tok: Constant.Long => withUnary(Lit.Long(unary(tok.value).longValue))
      case tok: Constant.IntXL => withUnary(Lit.IntXL(unary(tok.value)))
      case tok: Constant.Float => getBigDecimal(tok, Lit.Float.apply)
      case tok: Constant.Double => getBigDecimal(tok, Lit.Double.apply)
      case tok => unreachable(tok)
    }
  }

  private def rawLiteral(unary: Unary): Lit = nextAfter {
    val res = currToken match {
      case t: Constant.FloatXL => Left(Lit.FloatXL(t.value))
      case _: NumericConstant[_] => rawNumericLiteral(unary match {
          case unary: Unary.Numeric => unary
          case _ => Unary.Noop
        })
      case Constant.Char(value) => Left(Lit.Char(value))
      case Constant.String(value) => Left(Lit.String(value))
      case t: Constant.Symbol =>
        if (dialect.allowSymbolLiterals) Left(Lit.Symbol(t.value))
        else syntaxError("Symbol literals are no longer allowed", at = t)
      case x: BooleanConstant => unary match {
          case unary: Unary.Logical => Right(Lit.Boolean(unary(x.value)))
          case _ => Left(Lit.Boolean(x.value))
        }
      case _: KwNull => Left(Lit.Null())
      case t => unreachable(t)
    }
    res match {
      case Right(arg) => arg
      case Left(arg) =>
        if (unary eq Unary.Noop) arg
        else Lit.WithUnary(atPos(prevIndex)(Term.Name(unary.op)), atCurPos(arg))
    }
  }

  private def literal(): Lit = atCurPos(rawLiteral(Unary.Noop))

  private def interpolateWith[Ctx, Ret <: Tree](
      arg: => Ctx,
      result: (Term.Name, List[Lit], List[Ctx]) => Ret
  ): Ret = autoPos {
    val partsBuf = new ListBuffer[Lit]
    val argsBuf = new ListBuffer[Ctx]
    @tailrec
    def loop(): Unit = currToken match {
      case Interpolation.Part(value) =>
        partsBuf += atCurPos(Lit.String(value))
        next()
        loop()
      case _: Interpolation.SpliceStart =>
        next()
        argsBuf += arg
        accept[Interpolation.SpliceEnd]
        loop()
      case _ =>
    }
    val interpolator = atCurPos(currToken match {
      case Interpolation.Id(value) =>
        next()
        Term.Name(value)
      case _ => syntaxErrorExpected[Interpolation.Id]
    })
    accept[Interpolation.Start]
    loop()
    accept[Interpolation.End]
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  private def xmlWith[Ctx, Ret <: Tree](arg: => Ctx, result: (List[Lit], List[Ctx]) => Ret): Ret =
    autoPos {
      val partsBuf = new ListBuffer[Lit]
      val argsBuf = new ListBuffer[Ctx]
      @tailrec
      def loop(): Unit = currToken match {
        case Xml.Part(value) =>
          partsBuf += atCurPos(Lit.String(value))
          next()
          loop()
        case _: Xml.SpliceStart =>
          next()
          argsBuf += arg
          accept[Xml.SpliceEnd]
          loop()
        case _ =>
      }
      accept[Xml.Start]
      loop()
      accept[Xml.End]
      result(partsBuf.toList, argsBuf.toList)
    }

  def interpolateTerm(): Term.Interpolate = interpolateWith(unquoteExpr(), Term.Interpolate.apply)

  def xmlTerm(): Term.Xml = xmlWith(unquoteExpr(), Term.Xml.apply)

  def interpolatePat(): Pat.Interpolate = interpolateWith(unquotePattern(), Pat.Interpolate.apply)

  def xmlPat(): Pat.Xml = xmlWith(unquoteSeqPattern(), Pat.Xml.apply)

  /* ------------- NEW LINES ------------------------------------------------- */

  @inline
  def newLineOpt(): Unit = if (at[EOL]) next()

  @inline
  def newLinesOpt(): Unit = if (at[AtEOL]) next()

  def nextIfPair[A: ClassTag, B: ClassTag]: Boolean = at[A] && tryAhead[B]
  def nextIfPair[A: ClassTag](pred: Token => Boolean): Boolean = at[A] && tryAhead(pred(currToken))

  def newLineOptWhenFollowedBy(pred: Token => Boolean): Boolean = nextIfPair[EOL](pred)
  def newLineOptWhenFollowedBy[T: ClassTag]: Boolean = nextIfPair[EOL, T]

  def isIndentingOrEOL(nonOptBracesOK: => Boolean): Boolean =
    if (dialect.allowSignificantIndentation) in.indenting else nonOptBracesOK && at[EOL]

  def isAfterOpt[A: ClassTag, B: ClassTag]: Boolean = if (at[B]) tryAhead[A] else at[A]
  def isAfterOptNewLine[T: ClassTag]: Boolean = isAfterOpt[T, EOL]

  def atOrPeekAfterEOL(body: Token => Boolean): Boolean =
    if (at[EOL]) nextIf(body(peekToken)) else body(currToken)

  def getAfterOptNewLine[A](body: => Option[A]): Option[A] = if (at[EOL]) tryAhead(body) else body

  /* ------------- TYPES ---------------------------------------------------- */

  def typedOpt(): Option[Type] =
    if (acceptOpt[Colon]) Some(
      if (at[At] && peek[Ident]) {
        val startPos = currIndex
        outPattern.annotTypeRest(autoEndPos(startPos)(Type.AnonymousName()), startPos)
      } else typ()
    )
    else None

  private def getDeclTpeOpt(fullTypeOK: Boolean): Option[Type] =
    if (acceptOpt[Colon]) Some(typeOrInfixType(fullTypeOK)) else None

  private def typeOrInfixType(fullTypeOK: Boolean): Type = if (fullTypeOK) typ() else startInfixType()

  @inline
  private def typeOrInfixType(location: Location): Type = typeOrInfixType(location.fullTypeOK)

  /* ----------- EXPRESSIONS ------------------------------------------------ */

  def condExpr(): Term = inParens(expr())

  def expr(): Term = expr(location = NoStat, allowRepeated = false)

  def quasiquoteExpr(): Term = expr(location = NoStat, allowRepeated = true)

  def entrypointExpr(): Term = expr(location = NoStat, allowRepeated = false)

  def unquoteExpr(): Term = currToken match {
    case t: Ident => termName(t)
    case _: LeftBrace => expr(location = UnquoteStat, allowRepeated = true)
    case _: KwThis => anonThis()
    case _ => syntaxError(
        "error in interpolated string: identifier, `this' or block expected",
        at = currToken
      )
  }

  /**
   * Deals with Scala 3 concept of {{{inline x match { ...}}}. Since matches can also be chained in
   * Scala 3 we need to create the Match first and only then add the the inline modifier.
   */
  def inlineMatchClause(inlineMods: List[Mod]) =
    autoEndPos(inlineMods)(postfixExpr(allowRepeated = false)) match {
      case t: Term.Match => t.fullCopy(mods = inlineMods)
      case other => syntaxError("`inline` must be followed by an `if` or a `match`", at = other.pos)
    }

  private def matchClause(t: Term, startPos: Int, isSelect: Boolean = false) = {
    val cases =
      autoPos(if (at[Indentation.Indent]) indentedOnOpen(casesBlock()) else inBraces(casesBlock()))
    autoEndPos(startPos)(if (isSelect) Term.SelectMatch(t, cases) else Term.Match(t, cases))
  }

  def ifClause(mods: List[Mod] = Nil) = autoEndPos(mods) {
    accept[KwIf]
    def getMaybeIndented(fthen: Term => Term)(felse: => Term): Term =
      if (at[Indentation.Indent]) {
        val begPos = currIndex
        val termInitRaw = blockExprMaybePartialRaw().getOrElse(next(blockMaybeRaw()))
        val outdented = acceptOpt[Indentation.Outdent]
        val init = autoEndPosOpt(begPos)(termInitRaw)
        val term = exprAfterSimpleInit(init, begPos)
        try fthen(term)
        finally if (!outdented) accept[Indentation.Outdent]
      } else felse
    def getWithCondAndThenp(cond: Term, thenp: Term) = {
      val elsep = if (acceptIfAfterOpt[KwElse](StatSep)) expr() else atCurPosEmpty(Lit.Unit())
      Term.If(cond, thenp, elsep, mods)
    }
    def getWithCond(cond: Term) =
      getMaybeIndented(getWithCondAndThenp(cond, _))(getWithCondAndThenp(cond, expr()))

    getMaybeIndented { cond =>
      acceptAfterOptNL[KwThen]
      getWithCond(cond)
    } {
      val (cond, thenpOpt) = condExprWithOptionalBody[KwThen]
      thenpOpt.fold(getWithCond(cond))(getWithCondAndThenp(cond, _))
    }
  }

  private def condExprWithOptionalBody[T <: Token: ClassTag]: (Term, Option[Term]) =
    if (!dialect.allowQuietSyntax) {
      val cond = condExpr()
      newLinesOpt()
      (cond, None)
    } else if (!at[LeftParen]) {
      val cond = expr()
      acceptAfterOptNL[T]
      (cond, None)
    } else {
      val startPos = currIndex
      val simpleExpr = condExpr()
      if (acceptIfAfterOptNL[T]) (simpleExpr, None)
      else {
        // let's consider case when something can continue cond or start body
        val argsOrInitBody = currToken match {
          case _: LeftParen => Some(inParensOrTupleOrUnitExpr(allowRepeated = false))
          case _: LeftBrace => Some(blockExprOnBrace())
          case _ => None
        }
        val complexExpr = tryParse {
          val simpleExprWithArgs = argsOrInitBody.fold(simpleExpr) { t =>
            val args = copyPos(t)(TermInfixContext.toArgClause(t))
            autoEndPos(startPos)(Term.Apply(simpleExpr, args))
          }
          Try(exprAfterSimpleInit(simpleExprWithArgs, startPos = startPos, canApply = true))
            .toOption.flatMap(x => if (acceptIfAfterOptNL[T]) Some(x -> None) else None)
        }
        complexExpr.getOrElse {
          if (argsOrInitBody.isEmpty) newLinesOpt()
          simpleExpr -> argsOrInitBody.map(t => exprAfterSimpleInit(t, t.begIndex, canApply = true))
        }
      }
    }

  private def condExprWithBody[T <: Token: ClassTag]: (Term, Term) = {
    val (cond, bodyOpt) = condExprWithOptionalBody[T]
    val body = bodyOpt.getOrElse(expr())
    (cond, body)
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
    val res = autoPosOpt {
      currToken match {
        case soft.KwInline() if peek[KwIf] => ifClause(List(inlineMod()))
        case _ if isInlineMatchMod(currIndex) => inlineMatchClause(List(inlineMod()))
        case _: KwIf => ifClause()
        case _: KwTry =>
          next()
          val body: Term = currToken match {
            case _ if dialect.allowTryWithAnyExpr => expr()
            case _: LeftParen => inParensOnOpen(expr())
            case _: LeftBrace => blockOnBrace()
            case _: Indentation.Indent => blockOnIndent()
            case _ => expr()
          }

          def finallyopt = if (acceptIfAfterOptNL[KwFinally]) Some(expr()) else None

          def tryWithCases(cases: Option[Term.CasesBlock]) = Term.Try(body, cases, finallyopt)
          def tryWithHandler(handler: Term) = Term.TryWithHandler(body, handler, finallyopt)
          def tryInDelims(
              f: (=> Either[Term, Term.CasesBlock]) => Either[Term, Term.CasesBlock]
          ): Term = {
            val catchPos = currIndex
            f(casesBlockIfAny().toRight(blockRaw())).fold(
              x => tryWithHandler(autoEndPos(catchPos)(x)),
              x => tryWithCases(Some(autoEndPos(catchPos)(x)))
            )
          }

          if (acceptIfAfterOptNL[KwCatch]) currToken match {
            case _: KwCase => tryWithCases(Some(autoPos {
                next()
                toCasesBlock(caseClause(true) :: Nil)
              }))
            case _: Indentation.Indent => tryInDelims(indentedOnOpen)
            case _: LeftBrace => tryInDelims(inBracesOnOpen)
            case _ => tryWithHandler(expr())
          }
          else tryWithCases(None)

        case _: KwWhile =>
          next()
          val (cond, body) = condExprWithBody[KwDo]
          Term.While(cond, body)
        case _: KwDo if dialect.allowDoWhile =>
          next()
          val body = expr()
          skipAllStatSep()
          accept[KwWhile]
          val cond = condExpr()
          Term.Do(body, cond)
        case _: KwDo =>
          syntaxError("do {...} while (...) syntax is no longer supported", at = currToken)
        case _: KwFor =>
          next()
          def enumList =
            if (acceptOpt[LeftBrace]) inBracesAfterOpen(enumerators())
            else if (at[LeftParen]) {
              def parseInParens() = inParensOnOpen(enumerators())
              if (dialect.allowQuietSyntax)
                // Dotty retry in case of `for (a,b) <- list1.zip(list2) yield (a, b)`
                tryParse(Try(parseInParens()).toOption).getOrElse(enumerators())
              else parseInParens()
            } else maybeIndented(enumerators())
          val enums = autoPos(enumList.reduceWith(Term.EnumeratorsBlock.apply))

          newLinesOpt()
          if (acceptOpt[KwDo]) Term.For(enums, expr())
          else if (acceptOpt[KwYield]) Term.ForYield(enums, expr())
          else Term.For(enums, expr())
        case _: KwReturn =>
          next()
          if (isExprIntro(currToken, currIndex)) Term.Return(expr())
          else Term.Return(atCurPosEmpty(Lit.Unit()))
        case _: KwThrow =>
          next()
          Term.Throw(expr())
        case _: KwImplicit =>
          next()
          implicitClosure(location)
        case _: LeftBracket if dialect.allowPolymorphicFunctions =>
          val quants = typeParamClauseOpt()
          accept[RightArrow]
          Term.PolyFunction(quants, expr(location, allowRepeated))
        case _ =>
          val startPos = currIndex
          val t: Term = postfixExpr(startPos, allowRepeated)
          exprOtherRest(t, startPos, location, allowRepeated)
      }
    }
    if (location.anonFuncOK) maybeAnonymousFunction(res) else res
  }

  private def exprOtherRest(
      prefix: Term,
      startPos: Int,
      location: Location,
      allowRepeated: Boolean
  ): Term = {
    @inline
    def addPos[T <: Tree](body: T) = autoEndPos(startPos)(body)
    def repeatedTerm(t: Term, nextTokens: () => Unit): Term =
      if (allowRepeated) addPos {
        nextTokens()
        Term.Repeated(t)
      }
      else syntaxError("repeated argument not allowed here", at = currToken)
    @tailrec
    def iter(t: Term): Term = currToken match {
      case _: Equals => t match {
          case _: Term.Ref | _: Term.Apply | _: Quasi =>
            next()
            addPos(Term.Assign(t, expr(location = NoStat, allowRepeated = true)))
          case _ => t
        }
      case _: Colon => getFewerBracesApplyOnColon(t, startPos) match {
          case Some(x) => x
          case _ =>
            next()
            if (at[At] || at[Ellipsis] && peek[At])
              iter(addPos(Term.Annotate(t, annots(skipNewLines = false))))
            else if (at[Underscore] && isStar(peekToken)) repeatedTerm(t, nextTwice)
            else
              // this does not necessarily correspond to syntax, but is necessary to accept lambdas
              // check out the `if (token.is[RightArrow]) { ... }` block below
              iter(addPos(Term.Ascribe(t, typeOrInfixType(location))))
        }
      case soft.StarSplice() if allowRepeated && peek[RightParen, Comma] => repeatedTerm(t, next)
      case _: KwMatch =>
        next()
        matchClause(t, startPos)
      case _ => t
    }

    val res: Term = iter(prefix)

    // Now check for a possible arrow and then parse it as lambda.
    //
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

    // if couldn't convert to params:
    // do nothing, which will either allow self-type annotation parsing to kick in
    // or will trigger an unexpected token error down the line

    def allowName = location != TemplateStat
    def allowParam(hasType: Boolean) = location.funcParamOK && // scala3 requires typed in parens
      !(hasType && dialect.allowFewerBraces) || tokens(startPos).is[LeftParen] && prev[RightParen]

    val funcParamClauseOpt = res match {
      case _ if !at[FunctionArrow] => None
      case _: Lit.Unit => Some(Nil)
      case q: Quasi => q.rank match {
          case 0 if allowName => Some(q.become[Term.Param] :: Nil)
          case 1 if allowParam(hasType = false) => Some(q.become[Term.Param] :: Nil)
          case _ => None
        }
      case t: Term.Tuple =>
        val params = new ListBuffer[Term.Param]
        @tailrec
        def iter(ts: List[Term]): Option[List[Term.Param]] = ts match {
          case Nil => Some(params.toList)
          case head :: tail => convertToParam(head) match {
              case None => None
              case Some(p) =>
                params += p
                iter(tail)
            }
        }
        iter(t.args)
      case t => convertToParam(t).filter(p =>
          if (p.decltpe.nonEmpty) allowParam(hasType = true)
          else if (p.mods.nonEmpty) allowParam(hasType = false)
          else allowName
        ).map(_ :: Nil)
    }

    funcParamClauseOpt.fold(res) { x =>
      val contextFunction = at[ContextArrow]
      val pc = addPos(x.reduceWith(toParamClause(None)))
      val trm = next(termFunctionBody(location))
      addPos(if (contextFunction) Term.ContextFunction(pc, trm) else Term.Function(pc, trm))
    }
  }

  private def termFunctionBody(location: Location): Term =
    if (location != BlockStat) expr()
    else (currToken match {
      case _: LeftBrace => blockExprOnBrace(isOptional = true)
      case _: Indentation.Indent => blockExprOnIndent()
      case _ => blockOnOther()
    }) match {
      case t: Term.PartialFunction => getDeclTpeOpt(fullTypeOK = false)
          .fold[Term](t)(tpe => autoEndPos(t)(Term.Ascribe(t, tpe)))
      case t => t
    }

  private def convertToParam(tree: Tree): Option[Term.Param] = {
    def getModFromName(name: Name): Option[Mod] = name.value match {
      case soft.KwUsing() => Some(copyPos(name)(Mod.Using()))
      case soft.KwErased() => Some(copyPos(name)(Mod.Erased()))
      case _ => None
    }
    @tailrec
    def getMod(t: Tree, mods: List[Mod] = Nil): Option[List[Mod]] = t match {
      case t: Term.Name => getModFromName(t).map(_ :: mods)
      case t: Term.SelectPostfix => getModFromName(t.name) match {
          case Some(mod) => getMod(t.qual, mod :: mods)
          case _ => None
        }
      case t: Term.ApplyInfix => t.argClause.values match {
          case (n: Name) :: Nil if t.targClause.values.isEmpty =>
            val mOpt = for {
              m1 <- getModFromName(t.op)
              m2 <- getModFromName(n)
            } yield m1 :: m2 :: mods
            mOpt match {
              case Some(m) => getMod(t.lhs, m)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
    def getNameAndMod(t: Tree): Option[(Name, List[Mod])] = t match {
      case t: Name => Some((t, Nil))
      case t: Quasi => Some((t.become[Term.Name], Nil))
      case t: Term.SelectPostfix => getMod(t.qual).map((t.name, _))
      case t: Term.ApplyInfix => t.argClause.values match {
          case arg :: Nil if t.targClause.values.isEmpty =>
            for {
              mod <- getModFromName(t.op)
              name <- arg match {
                case n: Term.Placeholder => Some(copyPos(n)(Name.Placeholder()))
                case n: Name => Some(n)
                case _ => None
              }
              mods <- getMod(t.lhs, mod :: Nil)
            } yield (name, mods)
          case _ => None
        }
      case t: Term.Placeholder => Some((copyPos(t)(Name.Placeholder()), Nil))
      case t: Term.Eta => getMod(t.expr).map((atPos(t.endIndex)(Name.Placeholder()), _))
      case _ => None
    }
    def getType(t: Term): Option[Type] = t.becomeOrOpt[Type] {
      case t: Term.Name => Some(copyPos(t)(Type.Name(t.value)))
      case t: Term.Function => getTypeFunction(t)
      case t: Term.ApplyType => getType(t.fun).map(fun => copyPos(t)(Type.Apply(fun, t.targClause)))
      case _ => None
    }
    def getTypeFunction(t: Term.Function): Option[Type.Function] = t.becomeOrOpt[Type.Function](t =>
      getType(t.body).map { tpe =>
        val pc = t.paramClause.becomeOr[Type.FuncParamClause](pc =>
          copyPos(pc)(Type.FuncParamClause(pc.values.map(convertTermParamToType)))
        )
        copyPos(t)(Type.Function(pc, tpe))
      }
    )

    tree.becomeOrOpt[Term.Param] {
      case _: Lit.Unit => None
      case t: Term.Ascribe => getNameAndMod(t.expr).map { case (name, mod) =>
          copyPos(t)(Term.Param(mod, name, Some(t.tpe), None))
        }
      case t => getNameAndMod(t).map { case (name, mod) =>
          copyPos(t)(Term.Param(mod, name, None, None))
        }
    }
  }

  private def exprAfterSimple(
      term: Term,
      startPos: Int,
      location: Location = NoStat,
      allowRepeated: Boolean = false
  ): Term = {
    val postfix = postfixExpr(term, startPos, allowRepeated = allowRepeated)
    exprOtherRest(postfix, startPos, location, allowRepeated = allowRepeated)
  }

  private def exprAfterSimpleInit(
      term: Term,
      startPos: Int,
      location: Location = NoStat,
      canApply: Boolean = false,
      allowRepeated: Boolean = false
  ): Term = {
    val simple = simpleExprRest(term, canApply = canApply, startPos = startPos)
    exprAfterSimple(simple, startPos, location, allowRepeated = allowRepeated)
  }

  private def implicitClosure(location: Location): Term.Function = {
    val implicitPos = prevIndex
    val paramName = termName()
    val paramTpt = getDeclTpeOpt(fullTypeOK = false)
    val mod = atPos(implicitPos)(Mod.Implicit())
    val param = autoEndPos(implicitPos)(Term.Param(mod :: Nil, paramName, paramTpt, None))
    val params = copyPos(param)(Term.ParamClause(param :: Nil, Some(mod)))
    accept[RightArrow]
    autoEndPos(implicitPos)(Term.Function(params, termFunctionBody(location)))
  }

  // Encapsulates state and behavior of parsing infix syntax.
  // See `postfixExpr` for an involved usage example.
  // Another, much less involved usage, lives in `pattern3`.
  sealed abstract class InfixContext {
    // (Lhs, op and targs form UnfinishedInfix).
    // FinishedInfix is the type of an infix expression.
    // The conversions are necessary to push the output of finishInfixExpr on stack.
    type Typ
    type Op <: Name
    type UnfinishedInfix <: Unfinished

    // Represents an unfinished infix expression, e.g. [a * b +] in `a * b + c`.
    protected trait Unfinished {
      def lhs: Typ
      def op: Op
      final def precedence = op.precedence
      override def toString = s"[$lhs $op]"
    }

    // The stack of unfinished infix expressions, e.g. Stack([a + ]) in `a + b [*] c`.
    // `push` takes `b`, reads `*`, checks for type arguments and adds [b *] on the top of the stack.
    // Other methods working on the stack are self-explanatory.
    var stack: List[UnfinishedInfix] = Nil
    @inline
    def isDone(base: List[UnfinishedInfix]): Boolean = this.stack == base
    def push(unfinishedInfix: UnfinishedInfix): Unit = stack ::= unfinishedInfix

    def reduceStack(base: List[UnfinishedInfix], curr: Typ, currEnd: EndPos, op: Option[Op]): Typ =
      if (isDone(base)) curr
      else {
        val opPrecedence = op.fold(0)(_.precedence)
        val leftAssoc = op.forall(_.isLeftAssoc)

        // Pop off an unfinished infix expression off the stack and finish it with the rhs.
        // Then convert the result, so that it can become someone else's rhs.
        // Repeat while precedence and associativity allow.
        @tailrec
        def loop(rhs: Typ): Typ = {
          val lhs = stack.head
          val diffPrecedence = opPrecedence - lhs.precedence
          val canReduce = diffPrecedence < 0 || diffPrecedence == 0 && leftAssoc
          if (!canReduce) rhs
          else {
            stack = stack.tail
            val fin = finishInfixExpr(lhs, rhs, currEnd)
            if (isDone(base)) fin else loop(fin)
          }
        }

        loop(curr)
      }

    // Takes the unfinished infix expression, e.g. `[x +]`,
    // then takes the right-hand side (which can have multiple args), e.g. ` (y, z)`,
    // and creates `x + (y, z)`.
    // We need to carry endPos explicitly because its extent may be bigger than rhs because of parent of whatnot.
    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ
  }

  // Infix syntax in terms is borderline crazy.
  //
  // For example, did you know that `a * b + (c, d) * (f, g: _*)` means:
  // a.$times(b).$plus(scala.Tuple2(c, d).$times(f, g: _*))?!
  //
  // Actually there's even crazier stuff in scala-compiler.jar.
  // Apparently you can parse and typecheck `a + (bs: _*) * c`,
  // however I'm going to error out on this.
  object TermInfixContext extends InfixContext {
    type Typ = Term
    type Op = Term.Name

    // We need to carry lhsStart/lhsEnd separately from lhs.pos
    // because their extent may be bigger than lhs because of parentheses or whatnot.
    case class UnfinishedInfix(lhs: Typ, op: Op, targs: Type.ArgClause) extends Unfinished {
      override def toString = s"[$lhs $op$targs]"
    }

    def toArgClause(rhs: Typ): Term.ArgClause = copyPos(rhs)(
      (rhs match {
        case _: Lit.Unit if dialect.allowEmptyInfixArgs => Nil
        // https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html#source-incompatibilities
        case Term.Tuple(args) if (args match {
              case (_: Term.Assign) :: Nil => !dialect.allowNamedTuples
              case _ => true
            }) => args
        case _ => rhs :: Nil
      }).reduceWith(Term.ArgClause(_))
    )

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhsExt, op, targs) = unf
      val lhs = lhsExt match {
        // https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html#source-incompatibilities
        case Term.Tuple(arg :: Nil) if !dialect.allowNamedTuples || !arg.is[Term.Assign] => arg
        case x => x
      }

      if (lhs.is[Term.Repeated])
        syntaxError("repeated argument not allowed here", at = lhs.tokens.last)

      atPos(lhsExt, rhsEnd)(Term.ApplyInfix(lhs, op, targs, toArgClause(rhs)))
    }
  }

  // In comparison with terms, patterns are trivial.
  implicit object PatInfixContext extends InfixContext {
    type Typ = Pat
    type Op = Term.Name

    case class UnfinishedInfix(lhs: Typ, op: Op) extends Unfinished

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhsExt, op) = unf
      val lhs = lhsExt match {
        // https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html#source-incompatibilities
        case Pat.Tuple(arg :: Nil) if !dialect.allowNamedTuples || !arg.is[Pat.Assign] => arg
        case x => x
      }
      val args = copyPos(rhs)(
        (rhs match {
          case _: Lit.Unit if dialect.allowEmptyInfixArgs => Nil
          // https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html#source-incompatibilities
          case Pat.Tuple(args) if (args match {
                case (_: Pat.Assign) :: Nil => !dialect.allowNamedTuples
                case _ => true
              }) => args
          case _ => rhs :: Nil
        }).reduceWith(Pat.ArgClause.apply)
      )
      atPos(lhsExt, rhsEnd)(Pat.ExtractInfix(lhs, op, args))
    }
  }

  private object TypeInfixContext extends InfixContext {
    type Typ = Type
    type Op = Type.Name

    case class UnfinishedInfix(lhs: Typ, op: Op) extends Unfinished

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhs, op) = unf
      atPos(lhs, rhsEnd)(Type.ApplyInfix(lhs, op, rhs))
    }
  }

  private def getLeadingInfix[A <: Name, B](lf: InfixLF)(f: String => A)(g: A => B): Option[B] =
    peekToken match {
      case op: Ident =>
        def res = Some(g(atCurPos {
          next()
          newLineOpt()
          f(op.value)
        }))
        tryAhead(if (peek[Indentation]) None else lf.invalid.fold(res)(syntaxError(_, at = op)))
      case _ => None
    }

  def postfixExpr(allowRepeated: Boolean): Term = postfixExpr(currIndex, allowRepeated)

  def postfixExpr(startPos: Int, allowRepeated: Boolean): Term = {
    // Start the infix chain.
    // We'll use `a + b` as our running example.
    val rhs0 = prefixExpr(allowRepeated)

    postfixExpr(rhs0, startPos, allowRepeated)
  }

  private def postfixExpr(rhs0: Term, startPos: Int, allowRepeated: Boolean): Term = {
    import TermInfixContext._
    val base = stack

    def getLhsStartPos(lhs: Typ): Int = if (lhs eq rhs0) startPos else lhs.begIndex

    // Skip to later in the `postfixExpr` method to start mental debugging.
    // rhsStartK/rhsEndK may be bigger than then extent of rhsK,
    // so we really have to track them separately.
    @tailrec
    def loop(rhsK: Typ): Typ = {
      val rhsEndK = prevIndex

      def getPrevLhs(op: Term.Name): Term = reduceStack(base, rhsK, rhsEndK, Some(op))

      def getNextRhs(targs: => Type.ArgClause)(op: Term.Name) =
        getNextRhsWith(op, targs, argumentExprsOrPrefixExpr(PostfixStat))

      def getNextRhsWith(op: Term.Name, targs: Type.ArgClause, rhs: Term) = {
        val lhs = getPrevLhs(op)
        val wrap = (lhs eq rhs0) && lhs.begIndex != startPos
        val lhsExt = if (wrap) atPosWithBody(startPos, Term.Tuple(lhs :: Nil), rhsEndK) else lhs
        push(UnfinishedInfix(lhsExt, op, targs))
        Right(rhs)
      }

      def getPostfix(op: Term.Name, targs: Type.ArgClause) = {
        // Infix chain has ended with a postfix expression.
        // This never happens in the running example.
        if (targs.nonEmpty)
          syntaxError("type application is not allowed for postfix operators", at = targs)
        val finQual = getPrevLhs(op)
        val term: Term = atPos(getLhsStartPos(finQual), op)(Term.SelectPostfix(finQual, op))
        Left(term)
      }

      def emptyTypeArgs = atCurPosEmpty(Type.ArgClause(Nil))

      def getPostfixOrNextRhs(op: Term.Name): Either[Term, Term] = {
        // Infix chain continues.
        // In the running example, we're at `a [+] b`.
        val hasTypeArgs = at[LeftBracket] ||
          isIndentingOrEOL(nonOptBracesOK = true) && tryAhead[LeftBracket]
        val targs = if (hasTypeArgs) exprTypeArgs() else emptyTypeArgs

        // Check whether we're still infix or already postfix by testing the current token.
        // In the running example, we're at `a + [b]` (infix).
        // If we were parsing `val c = a b`, then we'd be at `val c = a b[]` (postfix).
        if (if (at[EOL]) nextIf(isExprIntro(peekToken, peekIndex)) else isIdentOrExprIntro(currToken))
          // Infix chain continues, so we need to reduce the stack.
          // In the running example, base = List(), rhsK = [a].
          getNextRhs(targs)(op) // [a]
        // afterwards, stack = List([a +])
        else {
          val argPos = currIndex
          (currToken match {
            case _: Colon => getFewerBracesArgOnColon()
            case _ => None
          }) match {
            case None => getPostfix(op, targs)
            case Some(x) => getNextRhsWith(op, targs, autoEndPos(argPos)(Term.Tuple(x :: Nil)))
          }
        }
      }

      val resOpt = currToken match {
        case lf: InfixLF => getLeadingInfix(lf)(Term.Name.apply)(getNextRhs(emptyTypeArgs))
        case _ if prev[Indentation.Outdent] => None
        case t: Unquote =>
          val op = unquote[Term.Name](t)
          Some(getPostfixOrNextRhs(op))
        case t: Ident if !(allowRepeated && soft.StarSplice(t) && peek[RightParen, Comma]) =>
          val op = atCurPosNext(Term.Name(t.value))
          Some(getPostfixOrNextRhs(op))
        case _: KwMatch if dialect.allowMatchAsOperator =>
          val op = atCurPosNext(Term.Name("match"))
          val lhs = getPrevLhs(op)
          Some(Right(matchClause(lhs, getLhsStartPos(lhs))))
        case _ => None
      }
      resOpt match {
        case Some(Left(x)) => x
        case Some(Right(x)) =>
          // Try to continue the infix chain.
          loop(x)
        case None =>
          // Infix chain has ended.
          // In the running example, we're at `a + b[]`
          // with base = List([a +]), rhsK = List([b]).
          rhsK
      }
    }

    // Iteratively read the infix chain via `loop`.
    // rhs0 is now [a]
    // If the next token is not an ident or an unquote, the infix chain ends immediately,
    // and `postfixExpr` becomes a fallthrough.
    val rhsN = loop(rhs0)

    // Infix chain has ended.
    // base contains pending UnfinishedInfix parts and rhsN is the final rhs.
    // For our running example, this'll be List([a +]) and [b].
    // Afterwards, lhsResult will be List([a + b]).
    if (rhs0 == rhsN && isDone(base)) rhs0
    else {
      val endPos = prevIndex
      atPosWithBody(startPos, reduceStack(base, rhsN, endPos, None), endPos)
    }
  }

  def prefixExpr(allowRepeated: Boolean): Term = currToken match {
    case Unary(unary) =>
      val startPos = currIndex
      next()
      def op = atPos(startPos)(Term.Name(unary.op))
      def addPos(tree: Term) = autoEndPos(startPos)(tree)
      def rest(tree: Term) = simpleExprRest(tree, canApply = true, startPos = startPos)
      if (at[Literal]) rest(addPos(rawLiteral(unary)))
      else simpleExpr0(allowRepeated = true) match {
        case Success(x) => addPos(Term.ApplyUnary(op, x))
        // maybe it is not unary operator but simply an ident `trait - {...}`
        // we would fail here anyway, let's try to treat it as ident
        case _ => rest(op)
      }
    case _ => simpleExpr(allowRepeated)
  }

  def simpleExpr(allowRepeated: Boolean): Term = simpleExpr0(allowRepeated).get

  private def simpleExpr0(allowRepeated: Boolean): Try[Term] = {
    var canApply = true
    val startPos = currIndex
    (currToken match {
      case MacroQuoted(term) => Success(term)
      case MacroSplicedIdent(term) => Success(term)
      case _: Literal => Success(literal())
      case _: Interpolation.Id => Success(interpolateTerm())
      case _: Xml.Start => Success(xmlTerm())
      case _: Ident | _: KwThis | _: KwSuper | _: Unquote => Success(path().become[Term])
      case _: Underscore => Success(atCurPosNext(Term.Placeholder()))
      case _: LeftParen => Success(inParensOrTupleOrUnitExpr(allowRepeated = allowRepeated))
      case _: LeftBrace =>
        canApply = false
        Success(blockExprOnBrace())
      case _: Indentation.Indent =>
        canApply = false
        Success(blockExprOnIndent())
      case _: KwNew =>
        canApply = false
        Success(autoPos {
          next()
          val tpl = TemplateOwnerContext.within(OwnedByTrait)(template())
          tpl.inits match {
            case init :: Nil if !prev[RightBrace] && tpl.earlyClause.isEmpty && tpl.body.isEmpty =>
              Term.New(init)
            case _ => Term.NewAnonymous(tpl)
          }
        })
      case _ => Failure(ParseException(currToken.pos, "illegal start of simple expression"))
    }) match {
      case Success(x) => Success(simpleExprRest(x, canApply = canApply, startPos = startPos))
      case x: Failure[_] => x
    }
  }

  private def macroIdent(ident: String, f: Term.Name => Term): Term = f(atCurPosNext(Term.Name(ident)))

  @tailrec
  private def simpleExprRest(t: Term, canApply: Boolean, startPos: Int): Term = {
    @inline
    def addPos(body: Term): Term = autoEndPos(startPos)(body)
    currToken match {
      case _: AtEOL if (peekToken match {
            case _: Dot => true
            case t: OpenDelim if canApply => isIndentingOrEOL(t.is[LeftBrace])
            case _ => false
          }) =>
        next()
        simpleExprRest(t, canApply, startPos)
      case _: Dot =>
        next()
        val isMatch = dialect.allowMatchAsOperator && acceptOpt[KwMatch]
        val clause =
          if (isMatch) matchClause(t, startPos, isSelect = true) else selector(t, startPos)
        simpleExprRest(clause, canApply = !isMatch, startPos = startPos)
      case _: LeftBracket =>
        @tailrec
        def isOk(tree: Tree): Boolean = tree match {
          case _: Quasi | _: Term.Name | _: Term.Select | _: Term.Apply | _: Term.ApplyInfix |
              _: Term.ApplyUnary | _: Term.New | _: Term.Placeholder | _: Term.ApplyUsing |
              _: Term.Interpolate | _: Term.SplicedMacroExpr | _: Term.PolyFunction => true
          case Term.Block(t :: Nil) => isOk(t)
          case _ => false
        }
        if (isOk(t)) {
          var app: Term = t
          doWhile { app = addPos(Term.ApplyType(app, exprTypeArgs())) }(at[LeftBracket])
          simpleExprRest(app, canApply = true, startPos = startPos)
        } else addPos(t)
      case tok @ (_: LeftParen | _: LeftBrace) if canApply =>
        def argClause = if (tok.is[LeftBrace]) getArgClauseOnBrace() else getArgClauseOnParen()
        val arguments = addPos(Term.Apply(t, argClause))
        simpleExprRest(arguments, canApply = true, startPos = startPos)
      case _: Colon if canApply =>
        getFewerBracesApplyOnColon(t, startPos, okSingleLineLambda = true).getOrElse(t)
      case _: Underscore if canApply =>
        next()
        addPos(Term.Eta(t))
      case _ => t
    }
  }

  private def getFewerBracesArgOnColon(okSingleLineLambda: Boolean = false): Option[Term] =
    if (!dialect.allowFewerBraces) None
    else {
      val colonPos = currIndex
      def addPos(term: Term) = autoEndPos(colonPos)(term)
      def tryGetArgAsLambdaBlock(postCheck: => Boolean) = tryGetArgAsLambda(okSingleLineLambda)
        .flatMap(arg => if (postCheck) Some(addPos(toBlockRaw(arg :: Nil))) else None)
      peekToken match {
        case _: Indentation.Indent =>
          next()
          tryAhead(tryGetArgAsLambdaBlock(acceptIfAfterOptNL[Indentation.Outdent]))
            .orElse(Some(addPos(blockExprOnIndent(keepBlock = true))))
        case _: Indentation => syntaxError("expected fewer-braces method body", currToken)
        case _: AtEOL =>
          val colon = currToken
          nextTwice()
          val argOpt = tryGetArgAsLambdaBlock(true)
          if (argOpt.isEmpty) syntaxError("expected fewer-braces method body", colon)
          argOpt
        case _ => tryAhead(tryGetArgAsLambdaBlock(true))
      }
    }

  private def tryGetArgAsLambda(okSingleLine: Boolean): Option[Term.FunctionLike] = Try {
    val paramPos = currIndex
    def getFunctionTerm(params: Term.ParamClause): Option[Term.FunctionTerm] = {
      def impl(f: (Term.ParamClause, Term) => Term.FunctionTerm) = {
        val bodyOpt =
          if (nextIfIndentAhead()) Some(blockExprOnIndent())
          else if (!okSingleLine) None
          else if (in.currRegions.headOption.exists(_.isInstanceOf[RegionParen])) None
          else next(Some(expr()))
        bodyOpt.map(body => autoEndPos(paramPos)(f(params, body)))
      }
      currToken match {
        case _: RightArrow => impl(Term.Function.apply)
        case _: ContextArrow => impl(Term.ContextFunction.apply)
        case _ => None
      }
    }
    def getPolyFunction(params: Type.ParamClause): Option[Term.PolyFunction] =
      if (at[RightArrow] && nextIfIndentAhead())
        Some(autoEndPos(paramPos)(Term.PolyFunction(params, blockExprOnIndent())))
      else None

    /**
     * We need to handle param and then open indented region, otherwise only the block will be
     * handles and any `.` will be accepted into the block:
     * ```
     * .map: a =>
     * a+1
     * .filter: x =>
     * x > 2
     * ```
     * Without manual handling here, filter would be included for `(a+1).filter`
     */
    def getParamAsFunction(pt: Option[Mod.ParamsType]) = {
      val param = getParamWithPos(pt.toList, fullTypeOK = false)
      getFunctionTerm(copyPos(param)(reduceAs(param :: Nil, toParamClause(pt))))
    }
    def getParamWithPos(mods: List[Mod], fullTypeOK: Boolean) = autoPos(getParam(mods, fullTypeOK))
    @tailrec
    def getParam(mods: List[Mod], fullTypeOK: Boolean): Term.Param = {
      def afterName(name: Name) = {
        val tpe = if (fullTypeOK || mods.nonEmpty) getDeclTpeOpt(fullTypeOK = fullTypeOK) else None
        Term.Param(mods, name, tpe, None)
      }
      currToken match {
        case t: Ellipsis => ellipsis[Term.Param](t, 1)
        case t: Ident =>
          val mod =
            if (peek[Ident]) t.text match {
              case soft.KwErased() => atCurPosNext(Mod.Erased())
              case soft.KwUsing() => atCurPosNext(Mod.Using())
              case _ => null
            }
            else null
          if (mod eq null) afterName(termName(t)) else getParam(mod :: mods, fullTypeOK)
        case _: Underscore => afterName(namePlaceholder())
        case _ => syntaxErrorExpected[Ident]
      }
    }

    currToken match {
      case _: LeftParen => getFunctionTerm(autoPos(
          inParensOnOpenOr {
            val pt = if (at[KwImplicit]) Some(atCurPosNext(Mod.Implicit())) else None
            val mods = pt.toList
            commaSeparated(getParamWithPos(mods, fullTypeOK = true)).reduceWith(toParamClause(pt))
          }(Term.ParamClause(Nil))
        ))
      case _: LeftBracket => getPolyFunction(typeParamClauseOnBracket())
      case t: Ellipsis if t.rank == 2 => getFunctionTerm(ellipsis[Term.ParamClause](t))
      case _: Ident | _: Underscore | _: Ellipsis => getParamAsFunction(None)
      case _: KwImplicit => getParamAsFunction(Some(atCurPosNext(Mod.Implicit())))
      case _ => None
    }
  }.getOrElse(None)

  private def getFewerBracesApplyOnColon(
      fun: Term,
      startPos: Int,
      okSingleLineLambda: Boolean = false
  ): Option[Term] = {
    val colonPos = currIndex
    getFewerBracesArgOnColon(okSingleLineLambda).map { arg =>
      val endPos = AutoPos.endIndex
      val argClause = atPos(colonPos, endPos)(Term.ArgClause(arg :: Nil))
      val arguments = atPos(startPos, endPos)(Term.Apply(fun, argClause))
      simpleExprRest(arguments, canApply = true, startPos = startPos)
    }
  }

  private def argumentExprsOrPrefixExpr(location: Location): Term = {
    val isBrace = at[LeftBrace]
    if (!isBrace && !at[LeftParen]) prefixExpr(allowRepeated = false)
    else {
      def findRep(args: List[Term]): Option[Term.Repeated] = args.collectFirst {
        case Term.Assign(_, rep: Term.Repeated) => rep
        case rep: Term.Repeated => rep
      }
      val lpPos = currIndex
      val args =
        if (isBrace) checkNoTripleDot(blockExprOnBrace(allowRepeated = true)) :: Nil
        else inParensOnOpenOr(argumentExprsInParens(location))(Nil)
      def getRest() = {
        findRep(args).foreach(x => syntaxError("repeated argument not allowed here", at = x))
        simpleExprRest(makeTupleTerm(lpPos, args), canApply = true, startPos = lpPos)
      }
      currToken match {
        case _: Dot | _: OpenDelim | _: Underscore => getRest()
        // see ArgumentExprs in:
        // https://scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html#context-free-syntax
        case _: EOL if !isBrace && !dialect.allowSignificantIndentation && tryAhead[LeftBrace] =>
          getRest()
        case _ => makeTupleTerm { arg =>
            val res = maybeAnonymousFunction(arg)
            if (isBrace) getTupleSingleTerm(res) else Left(res :: Nil)
          }(lpPos, args)
      }
    }
  }

  private def argumentExpr(location: Location): Term = currToken match {
    case t @ Ellipsis(2) => syntaxError(Messages.QuasiquoteRankMismatch(2, 1), at = t)
    case _ => expr(location = location, allowRepeated = true)
  }

  private def getArgClauseOnBrace(): Term.ArgClause = autoPos {
    val arg = blockExprOnBrace(allowRepeated = true)
    Term.ArgClause(arg :: Nil)
  }

  private def getArgClauseOnParen(location: Location = NoStat): Term.ArgClause = autoPos(
    inParensOnOpenOr(currToken match {
      case t @ Ellipsis(2) => (ellipsis[Term](t) :: Nil).reduceWith(Term.ArgClause(_))
      case x =>
        val using = x.text == soft.KwUsing.name && mightStartStat(peekToken, closeDelimOK = false)
        val mod = if (using) Some(atCurPosNext(Mod.Using())) else None
        argumentExprsInParens(location).reduceWith(Term.ArgClause(_, mod))
    })(Term.ArgClause(Nil))
  )

  private def argumentExprsInParens(location: Location = NoStat): List[Term] =
    commaSeparated(argumentExpr(location))

  private def checkNoTripleDot[T <: Tree](tree: T): T = tree match {
    case q: Quasi if q.rank == 2 => syntaxError(Messages.QuasiquoteRankMismatch(q.rank, 1), at = q)
    case t => t
  }

  private def isCaseIntro(): Boolean = at[KwCase] && isCaseIntroOnKwCase()

  // call it only if token is KwCase
  private def isCaseIntroOnKwCase(): Boolean = !peekToken.isClassOrObject

  private def blockExprPartial[T <: Token: ClassTag](orElse: => Term): Term = {
    val startPos = currIndex
    blockExprMaybePartialRaw().fold(orElse) { term =>
      acceptAfterOptNL[T]
      autoEndPos(startPos)(term)
    }
  }

  private def blockExprMaybePartialRaw(): Option[Term] = {
    val isPartial = peekToken match {
      case _: KwCase => ahead(isCaseIntroOnKwCase())
      case _: Ellipsis => ahead(peek[KwCase])
      case _ => false
    }
    if (isPartial) next(Some(Term.PartialFunction(caseClauses()))) else None
  }

  private def blockRaw(allowRepeated: Boolean = false): Term.Block =
    toBlockRaw(blockStatSeq(allowRepeated = allowRepeated))

  private def blockMaybeRaw(allowRepeated: Boolean = false, keepBlock: Boolean = false): Term =
    blockStatSeq(allowRepeated = allowRepeated) match {
      case (t: Term) :: Nil if !(keepBlock || isPrecededByDetachedComment(currIndex, t.endIndex)) =>
        t
      case stats => toBlockRaw(stats)
    }

  private def blockOnIndent(keepBlock: Boolean = false): Term =
    autoPosOpt(indentedOnOpen(blockMaybeRaw(keepBlock = keepBlock)))
  private def blockExprOnIndent(keepBlock: Boolean = false): Term =
    blockExprPartial[Indentation.Outdent](blockOnIndent(keepBlock))

  private def blockOnBrace(fstats: => List[Stat]): Term = autoPos(toBlockRaw(inBracesOnOpen(fstats)))
  private def blockOnBrace(allowRepeated: Boolean = false): Term =
    blockOnBrace(blockStatSeq(allowRepeated = allowRepeated))
  private def blockExprOnBrace(allowRepeated: Boolean = false, isOptional: Boolean = false): Term =
    blockExprPartial[RightBrace](
      if (isOptional) blockOnOther(allowRepeated) else blockOnBrace(allowRepeated)
    )

  private def blockOnOther(allowRepeated: Boolean = false): Term = {
    val start = currIndex
    blockStatSeq(allowRepeated = allowRepeated) match {
      case (term: Term) :: Nil => term
      case (q: Quasi) :: Nil => q.become[Term]
      case stats => autoEndPos(start)(toBlockRaw(stats))
    }
  }

  def caseClause(forceSingleExpr: Boolean = false): Case = {
    expectNot[KwCase]
    autoEndPos(prevIndex) {
      def caseBody() = {
        accept[RightArrow]
        if (at[Indentation.Indent]) blockExprOnIndent()
        else if (forceSingleExpr) expr(location = BlockStat, allowRepeated = false)
        else blockOnOther()
      }
      @inline
      def guard(): Option[Term] = if (at[KwIf]) Some(guardOnIf()) else None
      Case(pattern(), guard(), caseBody())
    }
  }

  def quasiquoteCase(): Case = entrypointCase()

  def entrypointCase(): Case = {
    accept[KwCase]
    caseClause()
  }

  def toCasesBlock(cases: List[Case]): Term.CasesBlock = cases.reduceWith(Term.CasesBlock.apply)
  def casesBlock(): Term.CasesBlock = casesBlockIfAny().getOrElse(syntaxErrorExpected[KwCase])
  def casesBlockIfAny(): Option[Term.CasesBlock] = caseClausesIfAny().map(toCasesBlock)

  def caseClauses(): List[Case] = caseClausesIfAny().getOrElse(syntaxErrorExpected[KwCase])
  def caseClausesIfAny(): Option[List[Case]] = {
    val cases = new ListBuffer[Case]
    @tailrec
    def iter(): Unit = currToken match {
      case t: Ellipsis =>
        cases += ellipsis[Case](t, 1, accept[KwCase])
        skipAllStatSep()
        iter()
      case _: KwCase if isCaseIntroOnKwCase() =>
        next()
        val quasiCase = unquoteOpt[Case]
        cases += quasiCase.getOrElse(caseClause())
        if (quasiCase.nonEmpty) skipAllStatSep() else if (StatSep(currToken)) tryAhead(isCaseIntro())
        iter()
      case _ =>
    }
    iter()
    if (cases.isEmpty) None else Some(cases.toList)
  }

  private def guardOnIf(): Term = {
    next()
    postfixExpr(allowRepeated = false)
  }

  private def enumeratorGuardOnIf() = autoPos(Enumerator.Guard(guardOnIf()))

  def enumerators(): List[Enumerator] = listBy[Enumerator] { enums =>
    def notEnumsEnd(token: Token): Boolean = token match {
      case _: Indentation.Outdent | _: CloseDelim | _: KwDo | _: KwYield => false
      case _ => true
    }
    doWhile {
      enums += enumerator(isFirst = enums.isEmpty)
      while (at[Token.KwIf]) enums += enumeratorGuardOnIf()
    }(
      if (StatSep(currToken)) nextIf(notEnumsEnd(peekToken))
      else isImplicitStatSep() && notEnumsEnd(currToken)
    )
  }

  private def enumerator(isFirst: Boolean = false): Enumerator = currToken match {
    case _: KwIf if !isFirst => enumeratorGuardOnIf()
    case t: Ellipsis => ellipsis[Enumerator](t, 1)
    case t: Unquote if !peek[Equals, LeftArrow] => unquote[Enumerator](t) // support for q"for ($enum1; ..$enums; $enum2)"
    case _ => generator()
  }

  def quasiquoteEnumerator(): Enumerator = entrypointEnumerator()

  def entrypointEnumerator(): Enumerator = enumerator()

  private def generator(): Enumerator with Tree.WithBody = {
    val startPos = currIndex
    val hasVal = acceptOpt[KwVal]
    val isCase = acceptOpt[KwCase]

    val pat = noSeq.pattern1(isForComprehension = true)
    val hasEq = at[Equals]

    if (hasVal)
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated", at = currToken)
      else syntaxError("val in for comprehension must be followed by assignment", at = currToken)

    if (hasEq) next() else accept[LeftArrow]
    val rhs = expr()

    autoEndPos(startPos)(
      if (hasEq) Enumerator.Val(pat, rhs)
      else if (isCase) Enumerator.CaseGenerator(pat, rhs)
      else Enumerator.Generator(pat, rhs)
    )
  }

  /* -------- PATTERNS ------------------------------------------- */

  /**
   * Methods which implicitly propagate whether the initial call took place in a context where
   * sequences are allowed. Formerly, this was threaded through methods as boolean seqOK.
   */
  trait SeqContextSensitive extends PatternContextSensitive {
    // is a sequence pattern _* allowed?
    def isSequenceOK: Boolean
    def isNamedTupleOk: Boolean = false

    def patterns(): List[Pat] = commaSeparated(pattern())

    def pattern(): Pat = patternAlternatives(Nil)

    @tailrec
    private def patternAlternatives(pats: List[Pat]): Pat = {
      val pat = pattern1()
      checkNoTripleDot(pat)
      if (Keywords.PatAlt(currToken)) {
        next()
        patternAlternatives(pat :: pats)
      } else if (pats.isEmpty) pat
      else {
        val endPos = pat.endIndex
        pats.foldLeft(pat) { case (rtAll, ltOne) =>
          atPos(ltOne.begIndex, endPos)(Pat.Alternative(ltOne, rtAll))
        }
      }
    }

    def quasiquotePattern(): Pat = {
      // NOTE: As per quasiquotes.md
      // * p"x" => Pat.Var (ok)
      // * p"X" => Pat.Var (needs postprocessing, parsed as Term.Name)
      // * p"`x`" => Term.Name (ok)
      // * p"`X`" => Term.Name (ok)
      val nonbqIdent = at[Ident] && !currToken.isBackquoted
      argumentPattern() match {
        case pat: Term.Name if nonbqIdent => copyPos(pat)(Pat.Var(pat))
        case pat => pat
      }
    }

    def entrypointPattern(): Pat = pattern()

    def unquotePattern(): Pat = dropAnyBraces(pattern())

    private def getSeqWildcard(isEnabled: Boolean, elseF: => Pat, mapF: Pat => Pat = identity) =
      if (isEnabled && at[Underscore]) autoPosIf(getSeqWildcardAtUnderscore()).fold(elseF)(mapF)
      else elseF

    private def getSeqWildcardAtUnderscore() =
      if (isSequenceOK && isStar(peekToken)) tryParse {
        nextTwice() // skip underscore and star
        newLinesOpt()
        val isArgListEnd = at[RightParen, RightBrace, EOF]
        if (isArgListEnd) Some(Pat.SeqWildcard()) else None
      }
      else None

    def pattern1(isForComprehension: Boolean = false): Pat = {
      val p = pattern2(isForComprehension)
      @inline
      def typed() = Pat.Typed(p, super.patternTyp(allowInfix = false))
      val pat = p match {
        case _ if !at[Colon] => p
        case _: Quasi =>
          next()
          typed()
        case _: Pat.Var =>
          next()
          if (!dialect.allowColonForExtractorVarargs && at[Underscore] && isStar(peekToken))
            syntaxError(s"$dialect does not support var: _*", at = p)
          getSeqWildcard(dialect.allowColonForExtractorVarargs, typed(), Pat.Bind(p, _))
        case _: Pat.Wildcard =>
          next()
          getSeqWildcard(dialect.allowColonForExtractorVarargs, typed())
        case _: Pat if dialect.allowAllTypedPatterns =>
          next()
          typed()
        case _: Pat => p
      }
      if (pat eq p) p else autoEndPos(p)(pat)
    }

    def pattern2(isForComprehension: Boolean = false): Pat = {
      val p = pattern3(isForComprehension)
      val pat = p match {
        case _ if !at[At] => p
        case _: Quasi =>
          next()
          Pat.Bind(p, pattern3())
        case _: Term.Name =>
          syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)", at = p)
        case p: Pat.Var =>
          next()
          Pat.Bind(p, getSeqWildcard(dialect.allowAtForExtractorVarargs, pattern3()))
        case _: Pat.Wildcard =>
          next()
          getSeqWildcard(dialect.allowAtForExtractorVarargs, pattern3())
        case p => p
      }
      if (pat eq p) p else autoEndPos(p)(pat)
    }

    def pattern3(isForComprehension: Boolean = false): Pat = {
      import PatInfixContext._
      val lhs = simplePattern(badPattern3, isForComprehension = isForComprehension)
      val base = stack
      @tailrec
      def loop(rhs: Typ): Typ = {
        @inline
        def lhs(opOpt: Option[Term.Name]) = reduceStack(base, rhs, rhs, opOpt)
        currToken match {
          case _: Unquote | Keywords.NotPatAlt() =>
            val op = termName()
            expectNot[LeftBracket]("infix patterns cannot have type arguments")
            push(UnfinishedInfix(lhs(Some(op)), op))
            loop(simplePattern(badPattern3, isRhs = true))
          case _ => lhs(None)
        }
      }
      loop(lhs)
    }

    def badPattern3(tok: Token): Nothing = {
      import PatInfixContext._
      def isComma = tok.is[Comma]
      def isDelimiter = tok.isAny[RightParen, RightBrace]
      def isCommaOrDelimiter = isComma || isDelimiter
      val (isUnderscore, isStar) = stack match {
        case UnfinishedInfix(lhs, Term.Name("*")) :: _ => (lhs.is[Pat.Wildcard], true)
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
      syntaxError(msg, at = tok)
    }

    def simplePattern(
        onError: Token => Nothing,
        isRhs: Boolean = false,
        isForComprehension: Boolean = false
    ): Pat = PatternContext.within {
      val startPos = currIndex
      autoEndPos(startPos) {
        currToken match {
          case Unary(unary) if tryAhead[Literal] => rawLiteral(unary)
          case sidToken @ (_: Ident | _: KwThis | _: Unquote) =>
            val sid = stableId()
            val targs = if (at[LeftBracket]) Some(super.patternTypeArgs()) else None
            if (at[LeftParen]) {
              val ref = sid.become[Term]
              Pat.Extract(
                targs.fold(ref)(x => autoEndPos(sid)(Term.ApplyType(ref, x))),
                autoPos(argumentPatterns().reduceWith(Pat.ArgClause.apply))
              )
            } else {
              targs.foreach(x =>
                syntaxError(s"pattern must be a value or have parens: $sid$x", at = currToken)
              )
              sid match {
                case name: Term.Name if isNamedTupleOk && acceptOpt[Equals] =>
                  Pat.Assign(name, noSeqWithNamed.pattern())
                case name: Term.Name.Quasi => name.become[Pat]
                case name: Term.Name =>
                  if (soft.StarSplice(currToken) && tryAhead[RightParen, Comma]) Pat.Repeated(name)
                  else if (if (!isForComprehension && sidToken.isBackquoted) at[Colon, At]
                    else {
                      val first = name.value.head
                      first == '_' || Character.getType(first) == Character.LOWERCASE_LETTER ||
                      dialect.allowUpperCasePatternVarBinding && at[At]
                    }) Pat.Var(name)
                  else name
                case select: Term.Select => select
                case _ => unreachable(Map(
                    "token" -> currToken,
                    "tokenStructure" -> currToken.structure,
                    "sid" -> sid,
                    "sidStructure" -> sid.structure
                  ))
              }
            }
          case _: Underscore => getSeqWildcardAtUnderscore().getOrElse(next(Pat.Wildcard()))
          case MacroQuoted(term) => Pat.Macro(term)
          case _: Literal => literal()
          case _: Interpolation.Id => interpolatePat()
          case _: Xml.Start => xmlPat()
          case _: LeftParen =>
            val lpPos = currIndex
            val patterns = inParensOnOpenOr(noSeqWithNamed.patterns())(Nil)
            makeTuple(lpPos, patterns, Lit.Unit(), Pat.Tuple.apply) {
              case t: Pat.Assign if dialect.allowNamedTuples => Left(t :: Nil)
              case t if !isRhs => Right(t)
              case t @ Pat.Tuple(_ :: Nil) => Right(t)
              case t => Left(t :: Nil)
            }
          case _: KwGiven =>
            next()
            Pat.Given(super.patternTyp(allowInfix = false))
          case t => onError(t)
        }
      }
    }
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

  /* In addition to above named tuple patterns are allowed*/
  object seqOKWithNamed extends SeqContextSensitive {
    val isSequenceOK = true
    override val isNamedTupleOk: Boolean = true
  }

  object noSeqWithNamed extends SeqContextSensitive {
    val isSequenceOK = false
    override def isNamedTupleOk: Boolean = true
  }

  /**
   * These are default entry points into the pattern context sensitive methods: they are all
   * initiated from non-pattern context.
   */
  def typ() = outPattern.typ()
  def paramType() = outPattern.paramType()
  private def typeBlock() = outPattern.typeBlock()
  def typeIndentedOpt() = outPattern.typeIndentedOpt()
  def quasiquoteType() = outPattern.quasiquoteType()
  def entrypointType() = outPattern.entrypointType()
  def startInfixType(inGivenSig: Boolean = false) = outPattern.infixType(inGivenSig = inGivenSig)
  def startModType() = outPattern.annotType()
  def exprTypeArgs() = outPattern.typeArgsInBrackets()
  def exprSimpleType() = outPattern.simpleType()

  /** Default entry points into some pattern contexts. */
  def pattern(): Pat = noSeq.pattern()
  def quasiquotePattern(): Pat = seqOK.quasiquotePattern()
  def entrypointPattern(): Pat = seqOK.entrypointPattern()
  def unquotePattern(): Pat = noSeq.unquotePattern()
  def unquoteSeqPattern(): Pat = seqOK.unquotePattern()
  def seqPatterns(): List[Pat] = seqOKWithNamed.patterns()
  def argumentPattern(): Pat = seqOK.pattern()
  def argumentPatterns(): List[Pat] = inParens(if (at[RightParen]) Nil else seqPatterns())
  def xmlLiteralPattern(): Pat = syntaxError("XML literals are not supported", at = currToken)
  def patternTyp() = noSeq.patternTyp(allowInfix = true)
  def patternTypeArgs() = noSeq.patternTypeArgs()

  /* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  private def privateModifier(): Mod = accessModifier(Mod.Private(_))

  private def protectedModifier(): Mod = accessModifier(Mod.Protected(_))

  private def accessModifier(mod: Ref => Mod): Mod = autoPos {
    next()
    if (!acceptOpt[LeftBracket]) mod(anonName())
    else {
      val result = mod(
        if (at[KwThis]) anonThis()
        else termName().becomeOr[Ref](x => copyPos(x)(Name.Indeterminate(x.value)))
      )
      accept[RightBracket]
      result
    }
  }

  private def badModifier(tok: Token, isLocal: Boolean): Nothing = {
    val local = if (isLocal) "local " else ""
    syntaxError(s"${local}modifier expected but ${tok.text} found", at = tok)
  }

  private def modifier(tok: ModifierKeyword, isLocal: Boolean): Mod = tok match {
    case _: KwAbstract => atCurPosNext(Mod.Abstract())
    case _: KwFinal => atCurPosNext(Mod.Final())
    case _: KwSealed => atCurPosNext(Mod.Sealed())
    case _: KwImplicit => atCurPosNext(Mod.Implicit())
    case _: KwLazy => atCurPosNext(Mod.Lazy())
    case _: KwOverride if !isLocal => atCurPosNext(Mod.Override())
    case _: KwPrivate if !isLocal => privateModifier()
    case _: KwProtected if !isLocal => protectedModifier()
    case _ => badModifier(tok, isLocal)
  }

  private def modifier(tok: Ident, isLocal: Boolean): Mod = tok.text match {
    case soft.KwInline() => atCurPosNext(Mod.Inline())
    case soft.KwInfix() => atCurPosNext(Mod.Infix())
    case soft.KwInto() => atCurPosNext(Mod.Into())
    case soft.KwOpen() if !isLocal => atCurPosNext(Mod.Open())
    case soft.KwOpaque() => atCurPosNext(Mod.Opaque())
    case soft.KwTransparent() => atCurPosNext(Mod.Transparent())
    case soft.KwErased() => atCurPosNext(Mod.Erased())
    case soft.KwTracked() => atCurPosNext(Mod.Tracked())
    case _ => badModifier(tok, isLocal)
  }

  def quasiquoteModifier(): Mod = entrypointModifier()

  def entrypointModifier(): Mod = {
    def fail(t: Token, what: String = "modifier"): Nothing =
      syntaxError(s"$what expected but ${t.name} found", at = t)
    val mod = currToken match {
      case t: Unquote => unquote[Mod](t)
      case _: At => annots(skipNewLines = true) match {
          case Nil => unreachable
          case annot :: Nil => annot
          case _ => fail(currToken, "end of file")
        }
      case _: KwPrivate => privateModifier()
      case _: KwProtected => protectedModifier()
      case _: KwImplicit => atCurPosNext(Mod.Implicit())
      case _: KwFinal => atCurPosNext(Mod.Final())
      case _: KwSealed => atCurPosNext(Mod.Sealed())
      case _: KwOverride => atCurPosNext(Mod.Override())
      case _: KwCase => atCurPosNext(Mod.Case())
      case _: KwAbstract => atCurPosNext(Mod.Abstract())
      case _: KwLazy => atCurPosNext(Mod.Lazy())
      case _: KwVal if !dialect.allowUnquotes => atCurPosNext(Mod.ValParam())
      case _: KwVar if !dialect.allowUnquotes => atCurPosNext(Mod.VarParam())
      case t: Ident => t.text match {
          case "+" => atCurPosNext(Mod.Covariant())
          case "-" => atCurPosNext(Mod.Contravariant())
          case "valparam" if dialect.allowUnquotes => atCurPosNext(Mod.ValParam())
          case "varparam" if dialect.allowUnquotes => atCurPosNext(Mod.VarParam())
          case soft.KwOpen() => atCurPosNext(Mod.Open())
          case soft.KwTransparent() => atCurPosNext(Mod.Transparent())
          case soft.KwInline() => atCurPosNext(Mod.Inline())
          case soft.KwInfix() => atCurPosNext(Mod.Infix())
          case soft.KwErased() => atCurPosNext(Mod.Erased())
          case _ => fail(t)
        }
      case t => fail(t)
    }
    newLinesOpt()
    mod
  }

  private def ctorModifiers(buf: ListBuffer[Mod]): Unit = currToken match {
    case t: Unquote => if (peek[LeftParen]) buf += unquote[Mod](t)
    case t: Ellipsis => buf += ellipsis[Mod](t, 1)
    case _: KwPrivate => buf += privateModifier()
    case _: KwProtected => buf += protectedModifier()
    case _ =>
  }

  private def tparamModifiers(buf: ListBuffer[Mod]): Unit = currToken match {
    case t: Unquote => if (peek[Ident, Unquote]) buf += unquote[Mod](t)
    case t: Ellipsis => buf += ellipsis[Mod](t, 1)
    case TParamVariant(mod) => buf += atCurPosNext(mod)
    case _ =>
  }

  def modifiersBuf(
      buf: ListBuffer[Mod],
      isLocal: Boolean = false,
      isParams: Boolean = false
  ): Unit = {
    def append(mod: Mod): Unit = {
      buf += mod
      newLinesOpt()
    }
    // the only things that can come after $mod or $mods are either keywords or names; the former is easy,
    // but in the case of the latter, we need to take care to not hastily parse those names as modifiers
    def continueLoop = peekToken match {
      case _: Colon | _: Equals | _: EOF | _: LeftBracket | _: Subtype | _: Supertype |
          _: Viewbound => true
      case _ => false
    }
    @tailrec
    def loop: Unit = currToken match {
      case _: AtEOL if !isLocal =>
        next()
        loop
      case t: Unquote => if (!continueLoop) {
          append(unquote[Mod](t))
          loop
        }
      case t: Ellipsis =>
        append(ellipsis[Mod](t, 1))
        loop
      case _: KwCase if peek[KwObject, KwClass] => buf += atCurPosNext(Mod.Case())
      case t: ModifierKeyword =>
        append(modifier(t, isLocal))
        loop
      case t: Ident if {
            if (isParams) !peek[Colon] && ParamsModifier.matches(t.value)
            else isSoftModifier(currIndex)
          } =>
        append(modifier(t, isLocal))
        loop
      case _ =>
    }
    loop
  }

  def annots(skipNewLines: Boolean, allowArgss: Boolean = true): List[Mod.Annot] =
    listBy[Mod.Annot](annotsBuf(_, skipNewLines, allowArgss))

  def annotsBuf[T >: Mod.Annot](
      annots: ListBuffer[T],
      skipNewLines: Boolean,
      insidePrimaryCtorAnnot: Boolean = false,
      allowArgss: Boolean = true
  ): Unit = while (currToken match {
      case _: At =>
        next()
        annots += unquoteOpt[Mod.Annot].getOrElse(autoEndPos(prevIndex)(
          Mod.Annot(initRest(exprSimpleType(), allowArgss, insidePrimaryCtorAnnot))
        ))
        true
      case t: Ellipsis if peek[At] =>
        annots += ellipsis[Mod.Annot](t, 1, next())
        true
      case _ => false
    }) if (skipNewLines) newLineOpt()

  /* -------- PARAMETERS ------------------------------------------- */

  @tailrec
  private def onlyLastParameterCanBeRepeated(params: List[Term.Param]): Unit = params match {
    case p :: tail if tail.nonEmpty =>
      if (!p.is[Term.Param.Quasi] && p.decltpe.is[Type.Repeated])
        syntaxError("*-parameter must come last", p)
      onlyLastParameterCanBeRepeated(tail)
    case _ =>
  }

  private def memberParamClauseGroupOnParen(): Member.ParamClauseGroup = {
    val tparams = emptyTypeParams
    autoPos(termParamClausesOnParen(ellipsisMaxRank = 3) match {
      case (x: Quasi) :: Nil if x.rank == 2 => reellipsis[Member.ParamClauseGroup](x, 1)
      case x => Member.ParamClauseGroup(tparams, x)
    })
  }

  private def memberParamClauseGroupOnBracket(): Member.ParamClauseGroup = autoPos {
    val tparamClause = typeParamClauseOnBracket()
    val paramClauses = termParamClauses()
    Member.ParamClauseGroup(tparamClause, paramClauses)
  }

  def memberParamClauseGroup(isFirst: Boolean): Option[Member.ParamClauseGroup] = {
    def onFirstParen = Some(memberParamClauseGroupOnParen())
    def onBracket = Some(memberParamClauseGroupOnBracket())
    currToken match {
      case _: LeftParen if isFirst => onFirstParen
      case _: LeftBracket => onBracket
      case _: EOL => peekToken match {
          case _: LeftParen if isFirst =>
            next()
            onFirstParen
          case _: LeftBracket =>
            next()
            onBracket
          case _ => None
        }
      case _ => None
    }
  }

  def memberParamClauseGroups(): List[Member.ParamClauseGroup] = listBy[Member.ParamClauseGroup](
    buf =>
      while ({
        val pcgOpt = memberParamClauseGroup(isFirst = buf.isEmpty)
        pcgOpt.exists { pcg =>
          buf += pcg
          // can't have consecutive type clauses (so params must be present)
          // also, only the very last param may contain implicit
          pcg.is[Quasi] ||
          pcg.paramClauses.lastOption.exists(pc => pc.is[Quasi] || !pc.mod.is[Mod.Implicit])
        }
      }) {}
  )

  def termParamClauses(): List[Term.ParamClause] =
    if (!isAfterOptNewLine[LeftParen]) Nil else termParamClausesOnParen()

  private def termParamClausesOnParen(
      first: Option[Term.ParamClause] = None,
      ellipsisMaxRank: Int = 2
  ): List[Term.ParamClause] = listBy[Term.ParamClause] { paramss =>
    first.foreach(paramss += _)
    while ({
      val clause = termParamClauseOnParen(ellipsisMaxRank = ellipsisMaxRank)
      paramss += clause
      val hasModImplicit = clause match {
        case _: Quasi => false
        case x => x.mod.exists(_.is[Mod.Implicit])
      }
      !hasModImplicit && isAfterOptNewLine[LeftParen]
    }) {}
  }

  private def termParamClauseOnParen(ellipsisMaxRank: Int = 2): Term.ParamClause = autoPos {
    def reduceParams(params: List[Term.Param], mod: Option[Mod.ParamsType] = None) = params
      .reduceWith { x =>
        onlyLastParameterCanBeRepeated(x)
        toParamClause(mod)(x)
      }
    def parseParams(mod: Option[Mod.ParamsType] = None) = {
      val params = commaSeparatedWithIndex(termParam(mod = mod))
      reduceParams(params, mod)
    }
    inParensOnOpenOr(currToken match {
      case t @ Ellipsis(rank) if rank >= 2 && rank <= ellipsisMaxRank =>
        reduceParams(List(ellipsis[Term.Param](t)))
      case _: KwImplicit => parseParams(Some(atCurPosNext(Mod.Implicit())))
      case t: Ident if !peek[Colon] =>
        t.text match {
          case soft.KwUsing() => parseParams(mod = Some(atCurPosNext(Mod.Using())))
          case _ => parseParams()
        }
      case _ => parseParams()
    })(Term.ParamClause(Nil))
  }

  def termParam(mod: Option[Mod.ParamsType] = None)(paramIdx: Int): Term.Param = autoPos {
    val mods = new ListBuffer[Mod]
    annotsBuf(mods, skipNewLines = false)
    val numAnnots = mods.length
    modifiersBuf(mods, isParams = true)
    val hasExplicitMods = mods.view.drop(numAnnots).exists {
      case _: Mod.Quasi | _: Mod.Erased | _: Mod.Inline => false
      case m: Mod.Private => m.within.is[Name.Anonymous]
      case m: Mod.Protected => m.within.is[Name.Anonymous]
      case _ => true
    }

    mod.foreach { mod =>
      val clazz = mod.getClass
      mods.find(_.getClass eq clazz) match {
        case None => mods += mod
        case Some(x) => if (paramIdx == 0) syntaxError("repeated modifier", at = x)
      }
    }

    val varOrVarParamMod = currToken match {
      case _: KwVal => Some(atCurPosNext(Mod.ValParam()))
      case _: KwVar => Some(atCurPosNext(Mod.VarParam()))
      case _ =>
        if (hasExplicitMods) syntaxErrorExpected[KwVal]
        None
    }
    varOrVarParamMod.foreach(mods += _)

    def endParamQuasi = at[RightParen, Comma]
    def getParamType: Type = paramType()
    def getParam(name: Name, tpt: Option[Type]) = {
      val default = if (acceptOpt[Equals]) Some(expr()) else None
      Term.Param(mods.toList, name, tpt, default)
    }

    mods.headOption.collect { case q: Mod.Quasi if endParamQuasi => q.become[Term.Param] }.getOrElse {
      currToken match {
        case t: Ellipsis => ellipsis[Term.Param](t, 1)
        case t: Unquote =>
          val name = unquote[Name](t)
          if (endParamQuasi) name.become[Term.Param]
          else if (acceptOpt[Colon]) getParam(name, Some(getParamType))
          else if (at[Equals]) getParam(name, None)
          else getParam(anonNameAt(name), Some(name.become[Type]))
        case _ if !peek[Colon] => getParam(anonName(), Some(getParamType))
        case t: Ident =>
          val name = atCurPosNext(Term.Name(t.value))
          accept[Colon]
          getParam(name, Some(getParamType))
        case _ => syntaxErrorExpected[Ident]
      }
    }
  }

  def quasiquoteTermParam(): Term.Param = entrypointTermParam()

  def entrypointTermParam(): Term.Param = termParam()(-1)

  private def emptyTypeParamsRaw: Type.ParamClause = Type.ParamClause(Nil)
  private def emptyTypeParams: Type.ParamClause = atCurPosEmpty(emptyTypeParamsRaw)

  private def typeParamClauseOpt(): Type.ParamClause =
    if (!isAfterOptNewLine[LeftBracket]) emptyTypeParams else typeParamClauseOnBracket()

  private def typeParamClauseOnBracket(): Type.ParamClause =
    typeClauseInBrackets(typeParam(), Type.ParamClause.apply)

  def typeParam(): Type.Param = autoPos {
    val mods: List[Mod] = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = false)
      tparamModifiers(buf)
    }
    def endTparamQuasi = at[RightBracket, Comma]
    mods.headOption match {
      case Some(q: Mod.Quasi) if endTparamQuasi => q.become[Type.Param]
      case _ =>
        val name = currToken match {
          case t: Ident => peekToken match {
              case p: Token.Ident if dialect.allowCaptureChecking && p.text == "^" =>
                autoPos {
                  nextTwice()
                  Type.CapSetName(t.value)
                }
              case _ => typeName(t)
            }
          case t: Unquote => unquote[Name](t)
          case _: Underscore => namePlaceholder()
          case _ => syntaxError("identifier expected", at = currToken)
        }
        name match {
          case q: Quasi if endTparamQuasi => q.become[Type.Param]
          case _ =>
            val tparams = typeParamClauseOpt()
            val bounds = typeBoundsWithOptViewBounds()
            Type.Param(mods, name, tparams, bounds)
        }
    }
  }

  def contextBoundOrAlias(allowAlias: Boolean) = currToken match {
    case t: Ellipsis => ellipsis[Type](t, 1)
    case _ => typ() match {
        case tpe @ Type.ApplyInfix(nm: Type.Name, Type.Name("as"), alias: Type.Name)
            if allowAlias => copyPos(tpe)(Type.BoundsAlias(alias, nm))
        case tpe => tpe
      }
  }

  def quasiquoteTypeParam(): Type.Param = entrypointTypeParam()

  def entrypointTypeParam(): Type.Param = typeParam()

  def typeBounds() = typeBoundsWithOptViewBounds()

  private def typeBoundsWithOptViewBounds() = autoPos {
    val loBound = bound[Supertype]
    val hiBound = bound[Subtype]
    val vbounds =
      listBy[Type](buf => while (acceptOpt[Viewbound]) buf += contextBoundOrAlias(allowAlias = false))
    val cbounds =
      if (acceptOpt[Colon]) listBy[Type] { buf =>
        def addContextBounds[T: ClassTag]: Unit = doWhile(
          buf += contextBoundOrAlias(allowAlias = dialect.allowImprovedTypeClassesSyntax)
        )(acceptOpt[T])
        if (acceptOpt[LeftBrace]) inBracesAfterOpen(addContextBounds[Comma])
        else addContextBounds[Colon]
      }
      else Nil
    Type.Bounds(lo = loBound, hi = hiBound, context = cbounds, view = vbounds)
  }

  def bound[T: ClassTag]: Option[Type] = if (acceptOpt[T]) Some(typ()) else None

  /* -------- DEFS ------------------------------------------- */

  def exportStmt(): Stat = autoPos {
    accept[KwExport]
    Export(commaSeparated(importer()))
  }

  def importStmt(): Import = autoPos {
    accept[KwImport]
    Import(commaSeparated(importer()))
  }

  def importer(): Importer = autoPos {
    val sid = stableId() match {
      case q: Quasi => q.become[Term.Ref]
      case sid @ Term.Select(q: Quasi, name) => copyPos(sid)(Term.Select(q.become[Term.Ref], name))
      case path => path
    }
    def dotselectors = Importer(sid, importees())
    def name(tn: Term.Name) = copyPos(tn)(Name.Indeterminate(tn.value))
    sid match {
      case Term.Select(sid: Term.Ref, tn: Term.Name) if sid.isPath =>
        if (acceptOpt[Dot]) dotselectors
        else if (acceptIf(soft.KwAs)) Importer(sid, importeeRename(name(tn)) :: Nil)
        else if (Wildcard.isStar(tn.tokens.head))
          Importer(sid, copyPos(tn)(Importee.Wildcard()) :: Nil)
        else Importer(sid, copyPos(tn)(Importee.Name(name(tn))) :: Nil)
      case tn: Term.Name if acceptIf(soft.KwAs) =>
        Importer(atCurPosEmpty(Term.Anonymous()), importeeRename(name(tn)) :: Nil)
      case _ =>
        accept[Dot]
        dotselectors
    }
  }

  def quasiquoteImporter(): Importer = entrypointImporter()

  def entrypointImporter(): Importer = importer()

  def importees(): List[Importee] =
    if (!acceptOpt[LeftBrace]) List(importWildcardOrName())
    else {
      val importees = inBracesAfterOpen(commaSeparated(importee()))
      if (dialect.allowGivenImports) importees.map {
        case Importee.Name(nm) if nm.value == "given" && importees.exists {
              case _: Importee.Wildcard => true
              case _ => false
            } => copyPos(nm)(Importee.GivenAll())
        case i => i
      }
      else importees
    }

  def importWildcardOrName(): Importee = {
    val startPos = currIndex
    autoEndPos(startPos)(currToken match {
      case Wildcard() =>
        next()
        Importee.Wildcard()
      case _: KwGiven =>
        next()
        if (at[Ident]) Importee.Given(typ()) else Importee.GivenAll()
      case t: Unquote => Importee.Name(unquote[Name.Quasi](t))
      case t: Ident =>
        next()
        Importee.Name(atPos(startPos)(Name.Indeterminate(t.value)))
      case _ => syntaxErrorExpected[Ident]
    })
  }

  def importeeRename(from: Name) = autoEndPos(from)(importWildcardOrName() match {
    case to: Importee.Name => Importee.Rename(from, to.name)
    case _: Importee.Wildcard => Importee.Unimport(from)
    case other => unreachable(Map("importees" -> other, "importeesStructure" -> other.structure))
  })

  def importee(): Importee = autoPos(importWildcardOrName() match {
    case from: Importee.Name if at[RightArrow] || soft.KwAs(currToken) =>
      next()
      importeeRename(from.name)
    // NOTE: this is completely nuts
    case from: Importee.Wildcard
        if (at[RightArrow] || soft.KwAs(currToken)) && nextIf(Wildcard.unapply(peekToken)) =>
      next()
      from
    case other => other
  })

  def quasiquoteImportee(): Importee = entrypointImportee()

  def entrypointImportee(): Importee = importee()

  def nonLocalDefOrDcl(): Stat = {
    val mods = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = true)
      modifiersBuf(buf)
    }
    defOrDclOrSecondaryCtor(mods) match {
      case s if s.isTemplateStat => s
      case other => syntaxError("is not a valid template statement", at = other)
    }
  }

  def defOrDclOrSecondaryCtor(mods: List[Mod]): Stat = {
    def onlyInline() = mods match {
      case (_: Mod.Inline) :: Nil => true
      case _ => false
    }
    currToken match {
      case _: KwVal | _: KwVar => patDefOrDcl(mods)
      case _: KwGiven => givenDecl(mods)
      case t: KwDef => autoEndPos(mods) {
          next() // skip KwDef
          if (!at[KwThis]) funDefRestAfterKwDef(mods)
          else if (TemplateOwnerContext.owner.isSecondaryCtorAllowed) peekToken match {
            case _: LeftParen => secondaryCtorRest(mods, nameThis(), termParamClausesOnParen())
            case t => syntaxError("secondary constructor needs explicit parameter list", at = t)
          }
          else syntaxError("Illegal secondary constructor", at = t)
        }
      case _: KwType => typeDefOrDcl(mods)
      case _: KwCase if dialect.allowEnums && peek[Ident] =>
        autoEndPos(mods) {
          next() // skip KwCase, now at Ident
          if (peek[Comma]) enumRepeatedCaseDef(mods) else enumSingleCaseDef(mods)
        }
      case _: KwIf if onlyInline() => ifClause(mods)
      case _ if isExprIntro(currToken, currIndex) && onlyInline() => inlineMatchClause(mods)
      case _ if isKwExtension(currIndex) => extensionGroupDecl(mods)
      case _ => tmplDef(mods, okTopLevel = false)
    }
  }

  def endMarker(): Stat = autoPos {
    assert(currToken.text == "end")
    next()
    Term.EndMarker(atCurPosNext(Term.Name(currToken match {
      case t: Ident => t.value
      case t => t.text
    })))
  }

  def patDefOrDcl(mods: List[Mod]): Stat = autoEndPos(mods) {
    val isVal = at[KwVal]
    next()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2()).map {
      case name: Term.Name => copyPos(name)(Pat.Var(name))
      case pat => pat
    }
    val tpOpt: Option[Type] = typedOpt()

    if (acceptOpt[Equals]) {
      val rhs = expr()
      if (rhs.is[Term.Placeholder] && (tpOpt.isEmpty || isVal || !lhs.forall(_.is[Pat.Var])))
        syntaxError("unbound placeholder parameter", at = currToken)
      if (isVal) Defn.Val(mods, lhs, tpOpt, rhs) else Defn.Var(mods, lhs, tpOpt, rhs)
    } else {
      lhs.foreach(x =>
        if (!x.is[Quasi] && !x.is[Pat.Var])
          syntaxError("pattern definition may not be abstract", at = x)
      )
      tpOpt.fold(syntaxError("declaration requires a type", at = currToken))(tp =>
        if (isVal) Decl.Val(mods, lhs, tp) else Decl.Var(mods, lhs, tp)
      )
    }
  }

  /**
   * @param oldOrNewSyntax
   *   new only if positive, old only if negative, either if zero
   *
   * ```scala
   * Given             ::= 'given' (GivenDef | OldGivenDef)
   * GivenDef          ::=  [id ':'] GivenSig
   * GivenSig          ::=  GivenImpl
   *                     |  '(' ')' '=>' GivenImpl
   *                     |  GivenConditional '=>' GivenSig
   * GivenImpl         ::=  GivenType ([‘=’ Expr] | TemplateBody)
   *                     |  ConstrApps TemplateBody
   * GivenConditional  ::=  DefTypeParamClause
   *                     |  DefTermParamClause
   *                     |  '(' FunArgTypes ')'
   *                     |  GivenType
   * GivenType         ::=  AnnotType1 {id [nl] AnnotType1}
   *
   * -- syntax up to Scala 3.5, to be deprecated in the future
   * OldGivenDef       ::=  [OldGivenSig] (AnnotType [‘=’ Expr] | StructuralInstance)
   *   -- one of `id`, `DefTypeParamClause`, `UsingParamClause` must be present
   * OldGivenSig       ::=  [id] [DefTypeParamClause] {UsingParamClause} ‘:’
   * StructuralInstance ::=  ConstrApp {‘with’ ConstrApp} [‘with’ WithTemplateBody]
   *
   * -- for reference
   * ConstrApps        ::=  ConstrApp ({‘,’ ConstrApp} | {‘with’ ConstrApp})
   * ConstrApp         ::=  SimpleType {Annotation} {ParArgumentExprs}
   * TemplateBody      ::=  :<<< [SelfType] TemplateStat {semi TemplateStat} >>>
   * WithTemplateBody  ::=  <<< [SelfType] TemplateStat {semi TemplateStat} >>>
   * ```
   */
  private def givenDecl(mods: List[Mod]): Stat = autoEndPos(mods) {
    accept[KwGiven]
    val sig = givenSig()

    val newSyntaxOK = dialect.allowImprovedTypeClassesSyntax
    val usedNewSyntax = newSyntaxOK && !prev[Colon]
    val initPos = sig.declPos.getOrElse(sig.declType.fold(currIndex)(_.begIndex))
    val decltype = sig.declType.getOrElse(refinement(None).getOrElse(startModType()))

    def getDefnGiven() = TemplateOwnerContext.within(OwnedByGiven) {
      val headInit =
        if (!at[LeftParen]) initImpl(initPos, decltype)(Nil)
        else initRestAt(initPos, decltype)(allowArgss = true)
      val inits = templateParentsWithFirst(allowComma = newSyntaxOK, allowWithBody = true)(headInit)
      val body =
        if (acceptOpt[KwWith])
          if (isAfterOptNewLine[LeftBrace]) templateBodyOnLeftBrace()
          else if (at[Indentation.Indent]) autoPos(templateBodyOnIndentRaw())
          else syntaxError("expected '{' or indentation", at = currToken)
        else if (usedNewSyntax) templateBodyOpt()
        else if (inits.lengthCompare(1) > 0) syntaxError("expected 'with' <body>", at = prevToken)
        else emptyTemplateBody()
      val rhs = autoEndPos(initPos)(Template(None, inits, body, Nil))
      Defn.Given(mods, sig.name, sig.pcg, rhs)
    }
    currToken match {
      case _: Equals =>
        next()
        Defn.GivenAlias(mods, sig.name, sig.pcg, decltype, expr())
      case _: KwWith | _: LeftParen => getDefnGiven()
      case _: Comma | _: LeftBrace | _: Colon if newSyntaxOK => getDefnGiven()
      case _ => sig.name match {
          case n: Term.Name => Decl.Given(mods, n, sig.pcg, decltype)
          case n: Name.Anonymous if dialect.allowImprovedTypeClassesSyntax =>
            Decl.GivenAnonymous(mods, n, sig.pcg, decltype)
          case n => syntaxError("abstract givens cannot be anonymous", at = n)
        }
    }
  }

  /**
   * We are first trying to parse non-anonymous given, which
   *   - requires `:` for type, if none is found it means it's anonymous
   *   - might fail in cases that anonymous type looks like type param
   *     {{{given Conversion[AppliedName, Expr.Apply[Id]] = ???}}}
   *
   * This will fail because type params cannot have `.`
   */
  private def givenSig(): GivenSig = tryParse {
    GivenSigContext.within {
      val ident = currToken.as[Ident]
      def newSyntax = dialect.allowImprovedTypeClassesSyntax
      if (ident ne null) { // possibly named typeclass
        val identPos = currIndex
        def name = atPos(identPos)(Term.Name(ident.value))
        def anon = anonNameAt(identPos)
        if (tryAhead[Colon])
          if (!tryAheadNot[Indentation.Indent]) None
          else if (newSyntax) givenSigAfterColon(name)
          else Some(GivenSig(name))
        else if (tryAhead[LeftBracket]) givenSigOnBracket(name)
        else if (tryAhead[LeftParen]) givenSigOnParen(name) // only with old syntax
        else if (tryAhead[EOL])
          if (tryAhead[LeftBracket]) givenSigOnBracket(name)
          else if (tryAhead[LeftParen]) givenSigOnParen(name) // only with old syntax
          else None
        else if (newSyntax)
          if (!tryAhead[RightArrow]) givenSigOther(anon) // only with old syntax
          else givenSigOnArrow(anon, atPos(identPos)(Type.Name(ident.value)))
        else None
      } else // anonymous typeclass or start of decltype
      if (isAfterOptNewLine[LeftBracket]) givenSigOnBracket(anonName())
      else if (isAfterOptNewLine[LeftParen]) Try(givenSigOnParen(anonName())).getOrElse(None)
      else None
    }
  }.getOrElse(GivenSig(anonName()))

  private def givenOldSyntaxColon(): Boolean = at[Colon] && tryAheadNot[Indentation.Indent]

  private def givenTypeParamClause(): Type.ParamClause = typeParamClauseOnBracket()

  private def givenTermParamClause(): Term.ParamClause = termParamClauseOnParen()

  private def givenSigOnBracket(name: Name): Option[GivenSig] = Try(memberParamClauseGroupOnBracket())
    .toOption.flatMap { pcg =>
      if (givenOldSyntaxColon()) Some(GivenSig(name, pcg :: Nil))
      else if (pcg.paramClauses.nonEmpty) None // too hard to convert to init, re-parse
      else if (!name.isAnonymous) {
        val tpe = convertNameTypeParamClauseToType(nameAsType(name), pcg.tparamClause)
        val anon = anonNameAt(name)
        if (dialect.allowImprovedTypeClassesSyntax && acceptOpt[RightArrow])
          givenSigAfterArrow(anon, convertTypeToTermParamClause(tpe))
        else Some(GivenSig(anon, declType = Some(tpe)))
      } else if (dialect.allowImprovedTypeClassesSyntax && acceptOpt[RightArrow])
        givenSigAfterArrow(name, pcg.tparamClause)
      else None
    }

  private def givenSigOnParen(name: Name): Option[GivenSig] = {
    // under the new syntax, this is possible only with anonymous name
    val useNewSyntax = dialect.allowImprovedTypeClassesSyntax && name.isAnonymous
    if (!useNewSyntax && !soft.KwUsing(peekToken)) None
    else {
      val pcg = memberParamClauseGroupOnParen()
      if (givenOldSyntaxColon()) Some(GivenSig(name, pcg :: Nil))
      else pcg.paramClauses match {
        case Seq(pc) if useNewSyntax && acceptOpt[RightArrow] =>
          givenSigAfterArrow(name, pcg.tparamClause, pc)
        case _ => None // too hard to convert to init, re-parse
      }
    }
  }

  private def givenSigAfterColon(name: Name): Option[GivenSig] =
    if (isAfterOptNewLine[LeftParen]) {
      val pc = givenTermParamClause()
      if (acceptOpt[RightArrow]) givenSigAfterArrow(name, pc)
      else Some(GivenSig(name, declType = Some(convertTermParamClauseToType(pc))))
    } else if (isAfterOptNewLine[LeftBracket]) {
      val tpc = givenTypeParamClause()
      accept[RightArrow]
      givenSigAfterArrow(name, tpc)
    } else givenSigOther(name)

  // with old syntax deprecated, fold into `givenSigAfterColon`
  private def givenSigOther(name: => Name): Option[GivenSig] = {
    val startPos = currIndex
    val tpe = startInfixType(inGivenSig = true)
    if (at[RightArrow]) givenSigOnArrow(name, tpe)
    else Some(GivenSig(name, declType = Some(tpe), declPos = Some(startPos)))
  }

  private def givenSigOnArrow(name: Name, tpe: Type): Option[GivenSig] = {
    next()
    givenSigAfterArrow(name, convertTypeToTermParamClause(tpe))
  }

  private def givenSigAfterArrow(name: Name, pc: Term.ParamClause): Option[GivenSig] =
    givenSigAfterArrow(name, atPosEmpty(pc)(emptyTypeParamsRaw), pc)

  private def givenSigAfterArrow(
      name: Name,
      tpc: Type.ParamClause,
      pc: Term.ParamClause = null
  ): Option[GivenSig] = {
    val pcbuf = new ListBuffer[Term.ParamClause]
    if (pc ne null) pcbuf += pc
    implicit val pcgbuf = new ListBuffer[Member.ParamClauseGroup]
    givenSigAfterArrow(name, tpc, pcbuf)
  }

  @tailrec
  private def givenSigAfterArrow(
      name: Name,
      tpc: Type.ParamClause,
      pcbuf: ListBuffer[Term.ParamClause]
  )(implicit pcgbuf: ListBuffer[Member.ParamClauseGroup]): Option[GivenSig] = {
    val arrowPos = prevIndex
    def flushPcBuf(): Unit = {
      val ok = pcbuf.nonEmpty || tpc.is[Quasi] || tpc.nonEmpty
      if (ok) pcgbuf += atPos(tpc, arrowPos)(Member.ParamClauseGroup(tpc, pcbuf.toList))
      pcbuf.clear()
    }
    if (pcbuf.lastOption.exists(!_.nonEmpty)) { // empty clause ends `GivenConditional`
      flushPcBuf()
      Some(GivenSig(name, pcgbuf.toList))
    } else if (isAfterOptNewLine[LeftParen]) {
      val pc = givenTermParamClause()
      if (acceptOpt[RightArrow]) {
        pcbuf += pc
        givenSigAfterArrow(name, tpc, pcbuf)
      } else {
        flushPcBuf()
        Some(GivenSig(name, pcgbuf.toList, Some(convertTermParamClauseToType(pc))))
      }
    } else if (isAfterOptNewLine[LeftBracket]) {
      flushPcBuf()
      val tpc = givenTypeParamClause()
      accept[RightArrow]
      givenSigAfterArrow(name, tpc, pcbuf)
    } else {
      val startPos = currIndex
      val tpe = startInfixType(inGivenSig = true)
      if (acceptOpt[RightArrow]) {
        pcbuf += convertTypeToTermParamClause(tpe)
        givenSigAfterArrow(name, tpc, pcbuf)
      } else {
        flushPcBuf()
        Some(GivenSig(name, pcgbuf.toList, declType = Some(tpe), declPos = Some(startPos)))
      }
    }
  }

  private def convertTypeParamToType(param: Type.Param): Type = param
    .becomeOr[Type](x => convertNameTypeParamClauseToType(nameAsType(x.name), param.tparamClause))

  private def convertNameTypeParamClauseToType(name: Type.Name, tpc: Type.ParamClause) = tpc match {
    case q: Quasi => atPos(name, q)(Type.Apply(name, q.become[Type.ArgClause]))
    case x if x.nonEmpty =>
      val args = x.values.map(tp => convertTypeParamToType(tp))
      atPos(name, x)(Type.Apply(name, copyPos(x)(Type.ArgClause(args))))
    case _ => name
  }

  private def convertTermParamToType(param: Term.Param): Type = param.becomeOr[Type] { x =>
    def name = nameAsType(x.name)
    x.decltpe.fold[Type](name)(tpe =>
      if (x.mods.isEmpty && x.name.isAnonymous) tpe
      else copyPos(x)(Type.TypedParam(name, tpe, x.mods))
    )
  }

  private def convertTermParamClauseToType(pc: Term.ParamClause) = pc.becomeOr[Type](x =>
    x.values.map(convertTermParamToType) match {
      case Nil => copyPos(x)(Lit.Unit())
      case v :: Nil if !keepTupleType(v) => v
      case vs => copyPos(x)(Type.Tuple(vs))
    }
  )

  private def convertTypeToTermParamClause(tpe: Type) = tpe.becomeOr[Term.ParamClause](x =>
    copyPos(x)(Term.ParamClause(copyPos(x)(Term.Param(Nil, anonNameAt(x), Some(x), None)) :: Nil))
  )

  // TmplDef           ::= ‘extension’ [DefTypeParamClause] ‘(’ DefParam ‘)’ {UsingParamClause} ExtMethods
  // ExtMethods        ::=  ExtMethod | [nl] ‘{’ ExtMethod {semi ExtMethod ‘}’
  // ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
  def extensionGroupDecl(mods: List[Mod]): Defn.ExtensionGroup = autoEndPos(mods) {
    next() // 'extension'

    val pcg = ExtensionSigContext.within(memberParamClauseGroup(isFirst = true))

    newLinesOpt()

    def getStats() = statSeq(templateStat)
    val body: Stat = currToken match {
      case _: LeftBrace => blockOnBrace(getStats())
      case _: Indentation.Indent => autoPosOpt(indentedOnOpen(getStats() match {
          case t :: Nil if !isPrecededByDetachedComment(currIndex, t.endIndex) => t
          case stats => toBlockRaw(stats)
        }))
      case _ if isDefIntro(currIndex) => nonLocalDefOrDcl()
      case _ => syntaxError("Extension without extension method", currToken)
    }
    Defn.ExtensionGroup(pcg, body)
  }

  private def funDefRestAfterKwDef(mods: List[Mod]): Stat = {
    val name = termName()

    def procedureSyntaxDeclType: Type = {
      val hint = s"Convert procedure `$name` to method by adding `: Unit =`."
      if (dialect.allowProcedureSyntax)
        deprecationWarning(s"Procedure syntax is deprecated. $hint", at = name)
      else syntaxError(s"Procedure syntax is not supported. $hint", at = name)
      atCurPosEmpty(Type.Name("Unit"))
    }

    val paramClauses: List[Member.ParamClauseGroup] =
      if (dialect.allowParamClauseInterleaving) memberParamClauseGroups()
      else memberParamClauseGroup(isFirst = true).toList

    def defn(declType: Option[Type]) = Defn.Def(mods, name, paramClauses, declType, expr())

    val restype = ReturnTypeContext.within(typedOpt())
    if (acceptOpt[Equals])
      if (!acceptOpt[KwMacro]) defn(restype) else Defn.Macro(mods, name, paramClauses, restype, expr())
    else if (StatSeqEnd(currToken) || StatSep(currToken)) Decl
      .Def(mods, name, paramClauses, restype.getOrElse(procedureSyntaxDeclType))
    else if (restype.isEmpty && isAfterOptNewLine[LeftBrace]) defn(Some(procedureSyntaxDeclType))
    else syntaxErrorExpected[Equals]
  }

  def typeDefOrDcl(mods: List[Mod]): Stat.TypeDef = autoEndPos(mods) {
    accept[KwType]
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt()
    val bounds = typeBounds()
    if (acceptOpt[Equals]) Defn.Type(mods, name, tparams, typeIndentedOpt(), bounds)
    else Decl.Type(mods, name, tparams, bounds)
  }

  /** Hook for IDE, for top-level classes/objects. */
  def topLevelTmplDef: Stat = tmplDef(listBy[Mod] { buf =>
    annotsBuf(buf, skipNewLines = true)
    modifiersBuf(buf)
  })

  private def tmplDef(mods: List[Mod], okTopLevel: Boolean = true): Stat = autoEndPosOpt(mods) {
    currToken match {
      case _: KwTrait => traitDef(mods)
      case _: KwEnum => enumDef(mods)
      case _: KwClass => classDef(mods)
      case _: KwObject => objectDef(mods)
      case _: At => syntaxError("Annotations must precede keyword modifiers", at = currToken)
      case _ if okTopLevel && dialect.allowToplevelStatements && isDefIntro(currIndex) =>
        defOrDclOrSecondaryCtor(mods)
      case _ => syntaxError(s"expected start of definition", at = currToken)
    }
  }

  private def traitDef(mods: List[Mod]): Defn.Trait = {
    next()
    val traitName = typeName()
    TemplateOwnerContext.within(OwnedByTrait)(
      Defn.Trait(mods, traitName, typeParamClauseOpt(), primaryCtor(), templateOpt())
    )
  }

  def classDef(mods: List[Mod]): Defn.Class = {
    next()

    val className = typeName()
    val typeParams = typeParamClauseOpt()
    val modCase = mods.has[Mod.Case]

    TemplateOwnerContext.within(if (modCase) OwnedByCaseClass else OwnedByClass) {
      val ctor = primaryCtor()

      if (modCase && !dialect.allowCaseClassWithoutParameterList && ctor.paramClauses.isEmpty)
        syntaxError(
          s"case classes must have a parameter list; try 'case class $className()' or 'case object $className'",
          at = currToken
        )

      Defn.Class(mods, className, typeParams, ctor, templateOpt())
    }
  }

  // EnumDef           ::=  id ClassConstr InheritClauses EnumBody
  private def enumDef(mods: List[Mod]): Defn.Enum = TemplateOwnerContext.within(OwnedByEnum) {
    next()
    val enumName = typeName()
    val typeParams = typeParamClauseOpt()
    val ctor = primaryCtor()
    val tmpl = templateOpt()
    Defn.Enum(mods, enumName, typeParams, ctor, tmpl)
  }

  // EnumCase          ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps]] | ids)
  // ids               ::=  id {‘,’ id}
  // ClassConstr       ::=  [ClsTypeParamClause] [ConstrMods] ClsParamClauses

  def enumRepeatedCaseDef(mods: List[Mod]): Defn.RepeatedEnumCase = {
    val values = commaSeparated(termName())
    Defn.RepeatedEnumCase(mods, values)
  }

  def enumSingleCaseDef(mods: List[Mod]): Defn.EnumCase = TemplateOwnerContext.within(OwnedByEnum) {
    val name = termName()
    val tparams = typeParamClauseOpt()
    val ctor = primaryCtor()
    val inits = if (acceptOpt[KwExtends]) templateParents(afterExtend = true) else List()
    Defn.EnumCase(mods, name, tparams, ctor, inits)
  }

  def objectDef(mods: List[Mod]): Defn.Object = {
    next()
    val objectName = termName()
    TemplateOwnerContext.within(OwnedByObject)(Defn.Object(mods, objectName, templateOpt()))
  }

  /* -------- CONSTRUCTORS ------------------------------------------- */

  def primaryCtor(): Ctor.Primary = autoPos {
    val noCtor = !TemplateOwnerContext.owner.isPrimaryCtorAllowed
    val mods =
      if (noCtor) Nil
      else listBy[Mod] { buf =>
        annotsBuf(buf, skipNewLines = false, allowArgss = false, insidePrimaryCtorAnnot = true)
        ctorModifiers(buf)
      }
    val name = anonName()
    val paramss = if (noCtor) Nil else termParamClauses()
    Ctor.Primary(mods, name, paramss)
  }

  def quasiquoteCtor(): Ctor = autoPos {
    val mods = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = true)
      modifiersBuf(buf)
    }
    accept[KwDef]
    val thisPos = currIndex
    accept[KwThis]
    val paramss = termParamClauses()
    newLineOptWhenFollowedBy[LeftBrace]
    if (at[EOF]) Ctor.Primary(mods, anonNameAt(thisPos), paramss)
    else secondaryCtorRest(mods, atPos(thisPos)(Name.This()), paramss)
  }

  def entrypointCtor(): Ctor = ???

  def secondaryCtorRest(
      mods: List[Mod],
      name: Name.This,
      paramss: Seq[Term.ParamClause]
  ): Ctor.Secondary = {
    val hasLeftBrace = isAfterOptNewLine[LeftBrace] || {
      accept[Equals]
      at[LeftBrace]
    }
    val body = autoPos(
      if (hasLeftBrace) inBracesOnOpen(constrInternal())
      else indentedOr(constrInternal())(Ctor.Block(initInsideConstructor(), Nil))
    )
    Ctor.Secondary(mods, name, paramss, body)
  }

  def constrInternal(): Ctor.Block = {
    val init = initInsideConstructor()
    val stats = if (acceptIfStatSep()) blockStatSeq() else Nil
    Ctor.Block(init, stats)
  }

  def initInsideConstructor(): Init = {
    def tpe = {
      expect[KwThis]
      val t = anonThis()
      copyPos(t)(Type.Singleton(t))
    }
    initRest(tpe, allowArgss = true, allowBraces = true)
  }

  def initInsideTemplate(): Init = initRest(startModType(), allowArgss = true)

  def quasiquoteInit(): Init = entrypointInit()

  def entrypointInit(): Init = currToken match {
    case _: KwThis => initInsideConstructor()
    case _ => initInsideTemplate()
  }

  private def initImpl(startPos: StartPos, tpe: Type)(argss: => List[Term.ArgClause]): Init =
    try autoEndPos(startPos)(Init(tpe, anonName(), argss))
    catch {
      case _: InvariantFailedException =>
        syntaxError(s"class type required but $tpe found", at = tpe)
    }

  def initRestAt(startPos: StartPos, tpe: Type)(
      allowArgss: Boolean,
      insidePrimaryCtorAnnot: Boolean = false,
      allowBraces: Boolean = false
  ): Init = {
    def getArgss = listBy[Term.ArgClause] { argss =>
      def allowParens: Boolean = peekToken match {
        case _ if !insidePrimaryCtorAnnot => true
        // explained here:
        // https://github.com/lampepfl/dotty/blob/675ae0c6440d5527150d650ad45d20fda5e03e69/compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L2581
        case _: RightParen => argss.isEmpty
        case _: Ident => !ahead(peek[Colon])
        case _: At => false
        case _ => !isModifier(peekIndex)
      }
      def maybeBody() = {
        if (at[AtEOL]) nextIf(peekToken match {
          case _: LeftParen => isIndentingOrEOL(false)
          case _: LeftBrace => isIndentingOrEOL(allowBraces)
          case _ => false
        })
        currToken match {
          case _: LeftParen if allowParens =>
            argss += getArgClauseOnParen()
            true
          case _: LeftBrace if allowBraces =>
            argss += getArgClauseOnBrace()
            true
          case _ => false
        }
      }
      if (maybeBody() && allowArgss) while (maybeBody()) {}
    }
    initImpl(startPos, tpe)(getArgss)
  }

  def initRest(
      typeParser: => Type,
      allowArgss: Boolean,
      insidePrimaryCtorAnnot: Boolean = false,
      allowBraces: Boolean = false
  ): Init = unquoteOpt[Init](!(peek[LeftParen] || allowBraces && peek[LeftBrace]))
    .getOrElse(initRestAt(currIndex, typeParser)(
      allowArgss = allowArgss,
      insidePrimaryCtorAnnot = insidePrimaryCtorAnnot,
      allowBraces = allowBraces
    ))

  /* ---------- SELFS --------------------------------------------- */

  def quasiquoteSelf(): Self = self(quasiquote = true)

  def entrypointSelf(): Self = self(quasiquote = false)

  private def self(quasiquote: Boolean): Self = selfEither(quasiquote)
    .fold(syntaxError(_, currToken), identity)

  private def selfEither(quasiquote: Boolean = false): Either[String, Self] = {
    val startPos = currIndex
    def withSelf(self: => Self) = Either
      .cond(acceptOpt[RightArrow] || quasiquote && at[EOF], self, "expected `=>`")
    def withDeclTpe(name: Name, declTpe: Option[Type]) =
      withSelf(autoEndPos(startPos)(Self(name, declTpe)))
    def withName(name: Name) = // possible fewer braces after colon
      if (!peek[Indentation, EOL]) withDeclTpe(name, getDeclTpeOpt(fullTypeOK = false))
      else if (!at[Colon]) withDeclTpe(name, None)
      else Left("missing type after self")
    currToken match {
      case t: Ident => withName(termName(t))
      case _: KwThis => withName(nameThis())
      case _: Underscore => withName(namePlaceholder())
      case t: Unquote =>
        val self = unquote[Self](t)
        if (at[Colon]) withName(self.become[Name]) else withSelf(self)
      case _ => Left("expected identifier, `this' or unquote")
    }
  }

  /* -------- TEMPLATES ------------------------------------------- */

  sealed trait TemplateOwner {
    def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean
    def isSecondaryCtorAllowed: Boolean
  }
  object OwnedByTrait extends TemplateOwner {
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean =
      dialect.allowTraitParameters
    override final def isSecondaryCtorAllowed: Boolean = false
  }
  object OwnedByCaseClass extends TemplateOwner {
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = true
    override final def isSecondaryCtorAllowed: Boolean = true
  }
  object OwnedByClass extends TemplateOwner {
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = true
    override final def isSecondaryCtorAllowed: Boolean = true
  }
  object OwnedByEnum extends TemplateOwner {
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = true
    override final def isSecondaryCtorAllowed: Boolean = true
  }
  object OwnedByObject extends TemplateOwner {
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = false
    override final def isSecondaryCtorAllowed: Boolean = false
  }
  object OwnedByGiven extends TemplateOwner {
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = false
    override final def isSecondaryCtorAllowed: Boolean = false
  }

  def init() = currToken match {
    case t: Ellipsis => ellipsis[Init](t, 1)
    case _ => initInsideTemplate()
  }

  def templateParentsWithFirst(allowComma: Boolean, allowWithBody: Boolean = false)(
      first: Init
  ): List[Init] = {
    def impl(isSeparator: => Boolean) =
      if (!isSeparator) None
      else Some(listBy[Init] { x =>
        x += first
        doWhile {
          next()
          x += init()
        }(isSeparator)
      })
    // whitespace includes Indent and AtEOL
    (if (allowWithBody) impl(at[KwWith] && !peek[Whitespace, LeftBrace]) else impl(at[KwWith]))
      .orElse(if (allowComma && dialect.allowCommaSeparatedExtend) impl(at[Comma]) else None)
      .getOrElse(first :: Nil)
  }

  def templateParents(afterExtend: Boolean = false): List[Init] =
    templateParentsWithFirst(allowComma = afterExtend)(init())

  def derivesClasses(): List[Type] =
    if (atOrPeekAfterEOL(soft.KwDerives(_))) {
      next()
      newLineOpt()
      val deriving = ListBuffer[Type]()
      doWhile(currToken match {
        case t: Ellipsis => deriving += ellipsis[Type](t, 1)
        case _ => deriving += startModType()
      })(acceptOpt[Comma])
      deriving.toList
    } else Nil

  private def templateAfterExtends(
      parents: List[Init] = Nil,
      edefs: Option[Stat.Block] = None
  ): Template = {
    val derived = derivesClasses()
    val body = templateBodyOpt()
    Template(edefs, parents, body, derived)
  }

  def template(afterExtend: Boolean = false): Template = autoPos {
    if (isAfterOptNewLine[LeftBrace]) {
      // @S: pre template body cannot stub like post body can!
      val body = templateBodyOnLeftBrace()
      if (at[KwWith] && body.selfOpt.isEmpty) {
        val edefs = body.stats
        edefs.foreach {
          case _: Quasi | _: Defn.Val | _: Defn.Var | _: Defn.Type =>
          case other => syntaxError("not a valid early definition", at = other)
        }
        next()
        val parents = templateParents(afterExtend)
        val early = copyPos(body)(toStatsBlockRaw(edefs))
        templateAfterExtends(parents, Some(early))
      } else Template(None, Nil, body, Nil)
    } else {
      val parents = if (at[Colon]) Nil else templateParents(afterExtend)
      templateAfterExtends(parents)
    }
  }

  def quasiquoteTemplate(): Template = entrypointTemplate()

  def entrypointTemplate(): Template = TemplateOwnerContext.within(OwnedByClass)(autoPos(template()))

  def templateOpt(): Template = autoPos(
    unquoteOpt[Template](peekToken match {
      case _: Dot | _: Hash | _: At | _: Ellipsis | _: LeftParen | _: LeftBracket | _: LeftBrace |
          _: KwWith => false
      case _ => true
    }).getOrElse {
      val onExtends =
        acceptOpt[KwExtends] || (TemplateOwnerContext.owner eq OwnedByTrait) && acceptOpt[Subtype]
      if (onExtends) template(afterExtend = true) else templateAfterExtends()
    }
  )

  @inline
  private def emptyTemplateBody(): Template.Body = atCurPosEmpty(Template.Body(None, Nil))

  @inline
  private def templateBodyOnLeftBrace(): Template.Body = autoPos(inBracesOnOpen(templateStatSeq()))

  @inline
  private def templateBodyOnIndentRaw(): Template.Body = indentedOnOpen(templateStatSeq())

  def templateBodyOpt(): Template.Body =
    if (isAfterOptNewLine[LeftBrace]) templateBodyOnLeftBrace()
    else if (at[Colon] && peek[Indentation, EOL]) autoPos {
      next()
      expect[Indentation.Indent]("template body")
      templateBodyOnIndentRaw()
    }
    else if (at[LeftParen]) {
      val owner = TemplateOwnerContext.owner
      if (owner.isPrimaryCtorAllowed) syntaxError("unexpected opening parenthesis", at = currToken)
      else {
        val what = owner.getClass.getSimpleName.stripPrefix("OwnedBy")
        syntaxError(s"$what may not have parameters", at = currToken)
      }
    } else emptyTemplateBody()

  private def toStatsBlockRaw(stats: List[Stat]): Stat.Block = stats.reduceWith(Stat.Block.apply)
  private def toStatsBlock(startPos: Int)(stats: List[Stat]): Stat.Block =
    autoEndPos(startPos)(toStatsBlockRaw(stats))

  private def refineWith(innerType: Option[Type], statsPos: Int, stats: => List[Stat]) =
    autoEndPos(innerType)(Type.Refine(innerType, toStatsBlock(statsPos)(stats)))

  private def refinement(innerType: Option[Type]): Option[Type] = {
    def maybeRefineIndented(startPos: => Int) =
      if (!tryAhead[Indentation.Indent]) innerType
      else Some(refineWith(innerType, startPos, indented(refineStatSeq())))
    currToken match {
      case _ if !dialect.allowSignificantIndentation => refinementInBraces(innerType, -1)
      case _: Colon => maybeRefineIndented(prevIndex)
      case _: KwWith => maybeRefineIndented(currIndex)
      case _ => refinementInBraces(innerType, in.previousIndentation + 1)
    }
  }

  @tailrec
  private def refinementInBraces(innerType: Option[Type], minIndent: Int): Option[Type] = {
    val notRefined = currToken match {
      case t: LeftBrace => minIndent > 0 && t.pos.startColumn < minIndent &&
        prevToken.pos.endLine < t.pos.endLine
      case _: EOL => minIndent > 0 && peekToken.pos.startColumn < minIndent || !tryAhead[LeftBrace]
      case _ => true
    }
    if (notRefined) innerType
    else refinementInBraces(
      Some(refineWith(innerType, currIndex, inBraces(refineStatSeq()))),
      minIndent
    )
  }

  private def existentialTypeOnForSome(t: Type): Type.Existential = {
    next()
    val statsClause = toStatsBlock(currIndex) {
      val stats = inBraces(refineStatSeq())
      stats
        .foreach(x => if (!x.isExistentialStat) syntaxError("not a legal existential clause", at = x))
      stats
    }
    atPos(t, statsClause)(Type.Existential(t, statsClause))
  }

  /* -------- STATSEQS ------------------------------------------- */

  private val consumeStat: PartialFunction[Token, Stat] = {
    case _: KwImport => importStmt()
    case _: KwExport => exportStmt()
    case _: KwPackage =>
      packageOrPackageObjectDef(if (dialect.allowToplevelTerms) consumeStat else topStat)
    case _ if isDefIntro(currIndex) => TemplateOwnerContext.within(OwnedByClass)(nonLocalDefOrDcl())
    case _ if isAtEndMarker() => endMarker()
    case _ if isIdentOrExprIntro(currToken) => stat(expr(location = NoStat, allowRepeated = true))
    case t: Ellipsis => ellipsis[Stat](t, 1)
  }

  def quasiquoteStat(): Stat = {
    def failEmpty() = syntaxError("unexpected end of input", at = currToken)
    def failMix(advice: Option[String]) = {
      val message = "these statements can't be mixed together"
      val addendum = advice.fold("")(", " + _)
      syntaxError(message + addendum, at = tokens.head)
    }
    statSeq(consumeStat) match {
      case Nil => failEmpty()
      case (stat @ Stat.Quasi(1, _)) :: Nil => toBlockRaw(List(stat))
      case stat :: Nil => stat
      case stats if stats.forall(_.isBlockStat) => toBlockRaw(stats)
      case stats if stats.forall(_.isTopLevelStat) => failMix(Some("try source\"...\" instead"))
      case _ => failMix(None)
    }
  }

  def entrypointStat(): Stat = {
    val stat = consumeStat
      .applyOrElse(currToken, (t: Token) => syntaxError("unexpected start of statement", at = t))
    skipAllStatSep()
    expect[EOF]
    stat
  }

  def stat(body: => Stat): Stat = body.become[Stat]

  def statSeq[T <: Tree](
      statpf: PartialFunction[Token, T],
      errorMsg: String = "illegal start of definition"
  ): List[T] = listBy[T](statSeqBuf(_, statpf, errorMsg))

  def statSeqBuf[T <: Tree](
      stats: ListBuffer[T],
      statpf: PartialFunction[Token, T],
      errorMsg: String = "illegal start of definition"
  ): Boolean = {
    val isIndented = acceptOpt[Indentation.Indent]
    val statpfAdd = statpf.runWith(stats += _)

    while (!StatSeqEnd(currToken))
      if (statpfAdd(currToken)) acceptExtendedStatSepOpt()
      else if (!acceptIfStatSep()) syntaxError(errorMsg + s" `${currToken.name}`", at = currToken)

    if (isIndented) accept[Indentation.Outdent]
    isIndented
  }

  val topStat: PartialFunction[Token, Stat] = {
    case t: Ellipsis => ellipsis[Stat](t, 1)
    case t: Unquote => unquote[Stat](t)
    case _: KwPackage => packageOrPackageObjectDef(topStat)
    case _: KwImport => importStmt()
    case _: KwExport => exportStmt()
    case _ if isTemplateIntro(currIndex) => topLevelTmplDef
    case _ if isAtEndMarker() => endMarker()
    case _ if dialect.allowToplevelStatements && isDefIntro(currIndex) => nonLocalDefOrDcl()
  }

  private def templateStatSeq(): Template.Body = {
    val selfTreeOpt = tryParse(selfEither().right.toOption)
    if (TemplateOwnerContext.owner eq OwnedByGiven) selfTreeOpt
      .foreach(_.decltpe.foreach(x => syntaxError("given cannot have a self type", at = x)))
    val stats = listBy[Stat] { buf =>
      def getStats() = statSeqBuf(buf, templateStat)
      val wasIndented = getStats() // some stats could be indented relative to self-type
      if (wasIndented && selfTreeOpt.isDefined) getStats() // and the rest might not be
    }
    Template.Body(selfTreeOpt, stats)
  }

  val templateStat: PartialFunction[Token, Stat] = {
    case _: KwImport => importStmt()
    case _: KwExport => exportStmt()
    case _ if isDefIntro(currIndex) => nonLocalDefOrDcl()
    case t: Unquote => unquote[Stat](t)
    case t: Ellipsis => ellipsis[Stat](t, 1)
    case _ if isAtEndMarker() => endMarker()
    case _ if isIdentOrExprIntro(currToken) => expr(location = TemplateStat, allowRepeated = false)
  }

  private def refineStatSeq(): List[Stat] = listBy[Stat] { stats =>
    def cond(tok: Token): Boolean = !StatSeqEnd(tok) && {
      def fail(err: String) = syntaxError(err, at = tok)
      if (tok.is[Ellipsis]) stats += ellipsis[Stat](tok.asInstanceOf[Ellipsis], 1)
      else if (isDclIntro(currIndex)) {
        val stat = defOrDclOrSecondaryCtor(Nil)
        if (stat.isRefineStat) stats += stat else fail("is not a valid refinement declaration")
      } else if (ReturnTypeContext.isInside()) fail(
        "illegal start of declaration (possible cause: missing `=' in front of current method body)"
      )
      else fail("illegal start of declaration")
      acceptExtendedStatSepOpt()
    }
    doWhile(skipAllStatSep())(cond(currToken))
  }

  def localDef(implicitMod: Option[Mod.Implicit]): Stat = {
    val mods = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = true)
      implicitMod.foreach(buf += _)
      modifiersBuf(buf, isLocal = true)
    }
    if (mods.forall {
        case _: Mod.Implicit | _: Mod.Lazy | _: Mod.Inline | _: Mod.Infix | _: Mod.Annot => true
        case _ => false
      }) defOrDclOrSecondaryCtor(mods)
    else tmplDef(mods) match {
      case stat: Decl.Type if dialect.allowTypeInBlock => stat
      case stat: Decl.Type => syntaxError("is not a valid block statement", at = stat)
      case stat if stat.isBlockStat => stat
      case other => syntaxError("is not a valid block statement", at = other)
    }
  }

  def blockStatSeq(allowRepeated: Boolean = false): List[Stat] = listBy[Stat] { stats =>
    def getStat(): Stat = currToken match {
      case _: Indentation.Outdent => null
      case _: KwCase if isCaseIntroOnKwCase() => null
      case _: Ellipsis if peek[KwCase] => null
      case _: KwExport => exportStmt()
      case _: KwImport => importStmt()
      case _: KwImplicit =>
        val implicitPos = currIndex
        next()
        if (at[Ident] && !isSoftModifier(currIndex)) implicitClosure(BlockStat)
        else localDef(Some(atPos(implicitPos)(Mod.Implicit())))
      case t if !isNonlocalModifier(t) && isDefIntro(currIndex) => localDef(None)
      case _ if isAtEndMarker() => endMarker()
      case _ if isIdentOrExprIntro(currToken) =>
        stat(expr(location = BlockStat, allowRepeated = allowRepeated))
      case t: Ellipsis => ellipsis[Stat](t, 1)
      case t if !mightStartStat(t, closeDelimOK = false) => null
      case t => syntaxError("illegal start of statement", at = t)
    }

    while ({
      skipAllStatSep()
      val stat = getStat()
      (null ne stat) && {
        stats += stat
        acceptIfExtendedStatSep()
      }
    }) {}

    if (allowRepeated && stats.length > 1) stats.foreach {
      case t: Term.Repeated => syntaxError("repeated argument not allowed here", at = t)
      case _ =>
    }
  }

  private def toPkgBody(startPos: Int)(stats: List[Stat]): Pkg.Body =
    autoEndPos(startPos)(stats.reduceWith(Pkg.Body.apply))

  private def packageOrPackageObjectDef(statpf: PartialFunction[Token, Stat]): Stat = autoPos {
    next()
    if (acceptOpt[KwObject]) TemplateOwnerContext
      .within(OwnedByObject)(Pkg.Object(Nil, termName(), templateOpt()))
    else {
      def packageBody = toPkgBody(currIndex)(
        if (nextIfColonIndent()) indentedOnOpen(statSeq(statpf)) else inBracesOrNil(statSeq(statpf))
      )
      Pkg(qualId(), packageBody)
    }
  }

  def source(): Source = autoPos {
    acceptOpt[Shebang]
    val statpf = if (dialect.allowToplevelTerms) consumeStat else topStat

    @tailrec
    def bracelessPackageStats(f: List[Stat] => Source): Source = {
      skipAllStatSep()
      if (at[KwPackage] && tryAheadNot[KwObject]) {
        val startPos = prevIndex
        val qid = qualId()
        def getPackage(pkgDelimPos: Int)(stats: => List[Stat]) =
          autoEndPos(startPos)(Pkg(qid, toPkgBody(pkgDelimPos)(stats)))
        def inPackageOnOpen[T <: Token: ClassTag](pkgDelimPos: Int) = f(listBy[Stat] { buf =>
          val pkg = getPackage(pkgDelimPos) {
            next()
            statSeqBuf(buf, statpf)
            acceptAfterOptNL[T]
            buf.toList
          }
          buf.clear()
          buf += pkg
          if (!at[EOF]) {
            acceptExtendedStatSep()
            statSeqBuf(buf, statpf)
          }
        })
        if (nextIfColonIndent()) inPackageOnOpen[Indentation.Outdent](prevIndex)
        else if (isAfterOptNewLine[LeftBrace]) inPackageOnOpen[RightBrace](currIndex)
        else {
          val inPkgPos = currIndex
          bracelessPackageStats(stats => f(getPackage(inPkgPos)(stats) :: Nil))
        }
      } else f(statSeq(statpf))
    }

    bracelessPackageStats(Source.apply)
  }

  def entrypointSource(): Source = source()

  def quasiquoteSource(): Source = entrypointSource()

}

object ScalametaParser {

  def doWhile(body: => Unit)(cond: => Boolean): Unit = {
    body
    while (cond) body
  }

  @inline
  private def toBlockRaw(stats: List[Stat]): Term.Block = Term.Block(stats)

  private def maybeAnonymousFunction(t: Term): Term = {
    val ok = PlaceholderChecks.hasPlaceholder(t, includeArg = false)
    if (ok) copyPos(t)(Term.AnonymousFunction(t)) else t
  }

  private def maybeAnonymousLambda(t: Type)(implicit dialect: Dialect): Type = {
    val ok = dialect.allowTypeLambdas && PlaceholderChecks.hasAnonymousParam(t, includeArg = false)
    if (ok) copyPos(t)(Type.AnonymousLambda(t)) else t
  }

  private def toParamClause(mod: Option[Mod.ParamsType]): List[Term.Param] => Term.ParamClause =
    if (mod.isDefined) Term.ParamClause(_, mod)
    else { v => Term.ParamClause(v, Term.ParamClause.getMod(v)) }

  private implicit class ImplicitTree[T <: Tree](private val tree: T) extends AnyVal {

    def become[A >: T <: Tree: AstInfo]: A = tree match {
      case q: Quasi => q.become[A]
      case _ => tree
    }

    def becomeOr[A <: Tree: AstInfo](f: T => A): A = tree match {
      case q: Quasi => q.become[A]
      case _ => f(tree)
    }

    def becomeOrOpt[A <: Tree: AstInfo](f: T => Option[A]): Option[A] = tree match {
      case q: Quasi => Some(q.become[A])
      case _ => f(tree)
    }

  }

  private def copyPos[T <: Tree](tree: Tree)(body: => T): T = body.withOrigin(tree.origin)

  @inline
  private def quasi[T <: Tree](rank: Int, tree: Tree)(implicit astInfo: AstInfo[T]): T with Quasi =
    astInfo.quasi(rank, tree)

  private def reellipsis[T <: Tree: AstInfo](q: Quasi, rank: Int): T = {
    val became = q.become[T].asInstanceOf[T with Quasi]
    if (became.rank != rank) copyPos(became)(quasi[T](rank, became.tree)) else became
  }

  private implicit class XtensionList[A <: Tree](private val list: List[A]) extends AnyVal {
    def reduceWith[B <: Tree: AstInfo](f: List[A] => B, reduceRank: Int = 1): B = ScalametaParser
      .reduceAs[A, B, B](list, f, reduceRank)
  }

  private def reduceAs[A <: Tree, B <: Tree, C <: B: AstInfo](
      list: List[A],
      f: List[A] => B,
      reduceRank: Int = 1
  ): B = list match {
    case (t: Quasi) :: Nil if t.rank >= reduceRank => reellipsis[C](t, t.rank - reduceRank)
    case v => f(v)
  }

  private def getTokenName[T <: Token](implicit ctag: ClassTag[T]): String = {
    val name = ctag.runtimeClass.getName
    val simplerName = name.substring(name.lastIndexOf('.') + 1)
    simplerName.stripPrefix("Token$").replace('$', '.') match {
      case "Semicolon" => ";"
      case "Hash" => "#"
      case "Colon" => ":"
      case "Viewbound" => "<%"
      case "LeftArrow" => "<-"
      case "Subtype" => "<:"
      case "Equals" => "="
      case "RightArrow" => "=>"
      case "Supertype" => ">:"
      case "At" => "@"
      case "Underscore" => "_"
      case "TypeLambdaArrow" => "=>>"
      case "ContextArrow" => "?=>"
      case "MacroQuote" => "'"
      case "LeftParen" => "("
      case "RightParen" => ")"
      case "Comma" => ","
      case "Dot" => "."
      case "LeftBracket" => "["
      case "RightBracket" => "]"
      case "LeftBrace" => "{"
      case "RightBrace" => "}"
      case "Ident" => "identifier"
      case "EOF" => "end of file"
      case "BOF" => "beginning of file"
      case other =>
        val kw = other.stripPrefix("Kw").stripPrefix("Indentation.")
        if (kw eq other) kw else kw.toLowerCase
    }
  }
  private def syntaxExpectedMessage[T <: Token: ClassTag](tok: Token): String =
    s"`${getTokenName[T]}` expected but `${tok.name}` found"
  private def syntaxNotExpectedMessage[T <: Token: ClassTag]: String =
    s"not expected `${getTokenName[T]}`"

  private object TParamVariantStr {
    def unapply(ident: String): Option[Mod.Variant] = ident match {
      case "+" => Some(Mod.Covariant())
      case "-" => Some(Mod.Contravariant())
      case _ => None
    }
  }

  private object TParamVariant {
    def unapply(ident: Token.Ident): Option[Mod.Variant] = TParamVariantStr.unapply(ident.text)
  }

  private case class GivenSig(
      name: Name,
      pcg: List[Member.ParamClauseGroup] = Nil,
      declType: Option[Type] = None,
      declPos: Option[Int] = None
  )

}

class Location private (
    val value: Int,
    val anonFuncOK: Boolean = true,
    val funcParamOK: Boolean = false,
    val fullTypeOK: Boolean = false
)
object Location {
  val NoStat = new Location(0, funcParamOK = true, fullTypeOK = true)
  val BlockStat = new Location(1, funcParamOK = true)
  val TemplateStat = new Location(2)
  val PostfixStat = new Location(3, anonFuncOK = false, fullTypeOK = true)
  val UnquoteStat = new Location(4, anonFuncOK = false)
}

object InfixMode extends Enumeration {
  val FirstOp, LeftOp, RightOp = Value
}
