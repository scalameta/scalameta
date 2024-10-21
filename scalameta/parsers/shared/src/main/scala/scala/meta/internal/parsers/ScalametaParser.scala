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
import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ScalametaParser(input: Input)(implicit dialect: Dialect) {
  parser =>

  import ScalametaParser._

  private val scannerTokens: ScannerTokens = ScannerTokens(input)
  import scannerTokens._

  /* ------------- NESTED CONTEXT OBJECTS ----------------------------------------- */
  // must all be parser-specific, to avoid sharing state with other parsers
  private object QuotedSpliceContext extends NestedContext
  private object QuotedPatternContext extends NestedContext
  private object ReturnTypeContext extends NestedContext
  private object TypeBracketsContext extends NestedContext
  private object PatternTypeContext extends NestedContext

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
  def parseStat(): Stat =
    parseRule(if (dialect.allowUnquotes) quasiquoteStat() else entrypointStat())

  def parseTerm(): Term =
    parseRule(if (dialect.allowUnquotes) quasiquoteExpr() else entrypointExpr())

  def parseUnquoteTerm(): Term = parseRule(unquoteExpr())

  def parseTermParam(): Term.Param =
    parseRule(if (dialect.allowUnquotes) quasiquoteTermParam() else entrypointTermParam())

  def parseType(): Type =
    parseRule(if (dialect.allowUnquotes) quasiquoteType() else entrypointType())

  def parseTypeParam(): Type.Param =
    parseRule(if (dialect.allowUnquotes) quasiquoteTypeParam() else entrypointTypeParam())

  def parsePat(): Pat =
    parseRule(if (dialect.allowUnquotes) quasiquotePattern() else entrypointPattern())

  def parseUnquotePat(): Pat = parseRule(unquotePattern())

  def parseCase(): Case =
    parseRule(if (dialect.allowUnquotes) quasiquoteCase() else entrypointCase())

  def parseCtor(): Ctor =
    parseRule(if (dialect.allowUnquotes) quasiquoteCtor() else entrypointCtor())

  def parseInit(): Init =
    parseRule(if (dialect.allowUnquotes) quasiquoteInit() else entrypointInit())

  def parseSelf(): Self =
    parseRule(if (dialect.allowUnquotes) quasiquoteSelf() else entrypointSelf())

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
    require(input.isInstanceOf[Input.Ammonite])
    val builder = List.newBuilder[Source]

    doWhile(builder += parseRuleAfterBOF(parseSourceImpl())) {
      currToken match {
        case t: Token.EOF if t.end < input.chars.length =>
          in.next()
          accept[Token.At]
          accept[Token.BOF]
          true
        case _ => false
      }
    }
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
  def nextTwice() = { next(); next() }

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
  private def tryAheadNot[T: ClassTag]: Boolean = nextIf(!peek[T])

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
  def atCurPosNext[T <: Tree](body: => T): T =
    try atCurPos(body)
    finally next()

  private val originSource = new Origin.ParsedSource(input)

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
    body.withOrigin(Origin.Parsed(originSource, start, endExcl))
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
    def is[T: ClassTag] = classTag[T].runtimeClass.isAssignableFrom(tok.getClass())
  }

  @inline
  private def at[T: ClassTag]: Boolean = currToken.is[T]
  @inline
  private def peek[T: ClassTag]: Boolean = peekToken.is[T]
  @inline
  private def prev[T: ClassTag]: Boolean = prevToken.is[T]

  private def syntaxErrorExpected[T <: Token: ClassTag]: Nothing = syntaxErrorExpected[T](currToken)
  private def syntaxErrorExpected[T <: Token: ClassTag](tok: Token): Nothing =
    syntaxError(syntaxExpectedMessage[T](tok), at = tok)
  private def expectAt[T <: Token: ClassTag](tok: Token, msg: => String, exists: Boolean): Unit =
    if (tok.is[T] != exists) syntaxError(msg, at = tok)

  @inline
  private def expectAt[T <: Token: ClassTag](tok: Token): Unit =
    expectAt[T](tok, syntaxExpectedMessage[T](tok), exists = true)
  @inline
  private def expect[T <: Token: ClassTag]: Unit = expectAt[T](currToken)
  @inline
  private def expect[T <: Token: ClassTag](msg: => String): Unit =
    expectAt[T](currToken, msg, exists = true)

  @inline
  private def expectNotAt[T <: Token: ClassTag](tok: Token): Unit =
    expectAt[T](tok, syntaxNotExpectedMessage[T], exists = false)
  @inline
  private def expectNot[T <: Token: ClassTag]: Unit = expectNotAt[T](currToken)
  @inline
  private def expectNot[T <: Token: ClassTag](msg: => String): Unit =
    expectAt[T](currToken, msg, exists = false)

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept[T <: Token: ClassTag]: Unit = {
    expect[T]
    if (currToken.isNot[EOF]) next()
  }

  /** If current token is T consume it. */
  @inline
  private def acceptOpt[T: ClassTag]: Boolean = nextIf(at[T])

  /** If current token is T consume it. */
  @inline
  private def acceptIf(unapply: Token => Boolean): Boolean = nextIf(unapply(currToken))

  private def acceptIfAfterOpt(unapplyA: Token => Boolean, unapplyB: Token => Boolean): Boolean =
    if (unapplyA(currToken)) { next(); true }
    else if (unapplyB(currToken) && unapplyA(peekToken)) { nextTwice(); true }
    else false

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

  def acceptStatSep(): Boolean = {
    val ok = acceptIfStatSep() || isAtEndMarker() || isImplicitStatSep() && !at[Indentation.Outdent]
    if (!ok) syntaxErrorExpected[Semicolon]
    ok
  }
  def acceptStatSepOpt() = !StatSeqEnd(currToken) && acceptStatSep()

  def skipAllStatSep(): Boolean = {
    val ok = acceptIfStatSep()
    while (acceptIfStatSep()) {}
    ok
  }

  /* -------------- TOKEN CLASSES ------------------------------------------- */

  @inline
  def isStar: Boolean = isStar(currToken)
  def isStar(tok: Token): Boolean = Keywords.Star(tok)

  private trait MacroIdent {
    protected def ident(tok: Token): Option[String]
    final def unapply(tok: Token): Option[String] =
      if (dialect.allowSpliceAndQuote && QuotedSpliceContext.isInside()) ident(tok) else None
  }

  private object MacroSplicedIdent extends MacroIdent {
    protected def ident(tok: Token): Option[String] = tok match {
      case Keywords(value) if value.length > 1 && value.charAt(0) == '$' => Some(value.substring(1))
      case _ => None
    }
  }

  private object MacroQuotedIdent extends MacroIdent {
    protected def ident(tok: Token): Option[String] = tok match {
      case Constant.Symbol(value) => Some(value.name)
      case _ => None
    }
  }

  private object VarArgTypeParam {
    // we assume that this is a type specification for a vararg parameter
    private def check(tok: Token): Boolean = peekToken match {
      case _: RightParen | _: Comma | _: Equals | _: RightBrace | _: EOF => tok.text == "*"
      case _ => false
    }
    @inline
    def unapply(tok: Token.Ident): Boolean = check(tok)
    object Non {
      def unapply(tok: Token.Ident): Boolean = !check(tok)
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

  private def unquote[T <: Tree: AstInfo](unquote: Unquote): T with Quasi = {
    require(unquote.input.chars(unquote.start + 1) != '$')
    val unquoteDialect = dialect.unquoteParentDialect
    if (null eq unquoteDialect) syntaxError(s"$dialect doesn't support unquotes", at = unquote)
    // NOTE: I considered having Input.Slice produce absolute positions from the get-go,
    // but then such positions wouldn't be usable with Input.Slice.chars.
    val unquotedTree = atCurPosNext {
      try {
        val unquoteInput = Input.Slice(input, unquote.start + 1, unquote.end)
        val unquoteParser = new ScalametaParser(unquoteInput)(unquoteDialect)
        if (dialect.allowTermUnquotes) unquoteParser.parseUnquoteTerm()
        else if (dialect.allowPatUnquotes) unquoteParser.parseUnquotePat()
        else unreachable
      } catch { case ex: Exception => throw ex.absolutize }
    }
    copyPos(unquotedTree)(quasi[T](0, unquotedTree))
  }

  def unquote[T <: Tree: AstInfo]: T with Quasi = currToken match {
    case t: Unquote => unquote[T](t)
    case _ => unreachable(currToken)
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

  private def makeTupleTerm(
      single: Term => Either[List[Term], Term]
  )(lpPos: Int, body: List[Term]): Term =
    makeTuple(lpPos, body, Lit.Unit(), Term.Tuple.apply)(single)

  private def makeTupleTerm(lpPos: Int, body: List[Term]): Term =
    makeTupleTerm(Right(_))(lpPos, body)

  private def makeTupleType(lpPos: Int, body: List[Type], zero: => Type, wrap: Boolean): Type =
    makeTuple(lpPos, body, zero, Type.Tuple.apply) {
      maybeAnonymousLambda(_) match {
        case t: Type.Tuple if wrap && t.args.lengthCompare(1) > 0 => Left(t :: Nil)
        case t => Right(t)
      }
    }

  private def makeTupleType(lpPos: Int, body: List[Type]): Type = {
    def invalidLiteralUnitType =
      syntaxError("illegal literal type (), use Unit instead", at = currToken.pos)
    makeTupleType(lpPos, body, invalidLiteralUnitType, wrap = false)
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
    makeTupleTerm(x => Right(maybeAnonymousFunction(x)))(lpPos, maybeTupleArgs)
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
      var hasParams = false
      var hasImplicits = false
      var hasTypes = false

      @tailrec
      def paramOrType(modsBuf: mutable.Builder[Mod, List[Mod]]): Type = currToken match {
        case t: Ellipsis => ellipsis[Type](t)
        case t: Unquote => unquote[Type](t)
        case _: KwImplicit if !hasImplicits =>
          next()
          hasImplicits = true
          paramOrType(modsBuf)
        case t: Ident if tryAhead[Colon] =>
          if (hasTypes) syntaxError(
            "can't mix function type and dependent function type syntaxes",
            at = currToken
          )
          hasParams = true
          val startPos = prevIndex
          next() // skip colon
          val mods = modsBuf.result()
          val modPos = if (mods.isEmpty) startPos else mods.head.begIndex
          val name = atPos(startPos)(Type.Name(t.value))
          autoEndPos(modPos)(Type.TypedParam(name, typ(), mods))
        case soft.KwErased() if allowFunctionType =>
          paramOrType(modsBuf += atCurPosNext(Mod.Erased()))
        case _ =>
          if (hasParams) syntaxError(
            "can't mix function type and dependent function type syntaxes",
            at = currToken
          )
          hasTypes = true
          val mods = modsBuf.result()
          val tpe = paramType()
          if (mods.isEmpty) tpe else autoEndPos(mods.head)(Type.FunctionArg(mods, tpe))
      }

      val openParenPos = currIndex
      val ts = inParensOr(commaSeparated(paramOrType(List.newBuilder[Mod])))(Nil)
      // NOTE: can't have this, because otherwise we run into #312
      // newLineOptWhenFollowedBy[LeftParen]

      if (hasParams && !dialect.allowDependentFunctionTypes)
        syntaxError("dependent function types are not supported", at = currToken)
      if (!hasTypes && !hasImplicits && at[LeftParen]) {
        val message = "can't have multiple parameter lists in function types"
        syntaxError(message, at = currToken)
      }

      def maybeFunc = getAfterOptNewLine(currToken match {
        case _: RightArrow => Some(typeFuncOnArrow(openParenPos, ts, Type.Function(_, _)))
        case _: ContextArrow => Some(typeFuncOnArrow(openParenPos, ts, Type.ContextFunction(_, _)))
        case _ => None
      })
      (if (allowFunctionType) maybeFunc else None).getOrElse {
        ts.find {
          case _: Type.ByName | _: Type.Repeated => true
          case t: Type.FunctionParamOrArg => t.mods.nonEmpty
          case _ => false
        }.foreach(t => syntaxError(s"'${t.productPrefix}' type not allowed here", at = t))
        val simple = simpleTypeRest(makeTupleType(openParenPos, ts), openParenPos)
        val compound = compoundTypeRest(annotTypeRest(simple, openParenPos), openParenPos)
        infixTypeRest(compound) match {
          case `compound` => compound
          case t => autoEndPos(openParenPos)(t)
        }
      }
    }

    private def typeLambdaOrPoly(): Type = {
      val quants = typeParamClauseOpt(ownerIsType = true)
      newLineOpt()
      currToken match {
        case _: TypeLambdaArrow => next(); Type.Lambda(quants, typeIndentedOpt())
        case t: RightArrow =>
          next()
          typeIndentedOpt() match {
            case tpe: Type.FunctionType => Type.PolyFunction(quants, tpe)
            case _ => syntaxError("polymorphic function types must have a value parameter", at = t)
          }
        case t => syntaxError("expected =>> or =>", at = t)
      }
    }

    def typeIndentedOpt(): Type =
      if (acceptOpt[Indentation.Indent]) indentedAfterOpen(typ()) else typ()

    def typ(): Type = autoPosOpt {
      val startPos = currIndex
      val t: Type =
        if (at[LeftBracket] && dialect.allowTypeLambdas) typeLambdaOrPoly() else infixTypeOrTuple()

      getAfterOptNewLine(currToken match {
        case _: RightArrow => Some(typeFuncOnArrow(startPos, t :: Nil, Type.Function(_, _)))
        case _: ContextArrow => Some(typeFuncOnArrow(startPos, t :: Nil, Type.ContextFunction(_, _)))
        case _: KwForsome => Some(existentialTypeOnForSome(t))
        case _: KwMatch if dialect.allowTypeMatch => next(); Some(Type.Match(t, typeCaseClauses()))
        case _ => None
      }).getOrElse(t)
    }

    def typeBlock(): Type =
      // TypeBlock, https://dotty.epfl.ch/docs/internals/syntax.html#expressions-3
      if (dialect.allowQuotedTypeVariables && at[KwType]) autoPos {
        val typeDefs = listBy[Stat.TypeDef] { buf =>
          doWhile(buf += typeDefOrDcl(Nil))(acceptIfStatSep() && at[KwType])
        }
        Type.Block(typeDefs, typ())
      }
      else typ()

    private def typeFuncOnArrow(
        paramPos: Int,
        params: List[Type],
        ctor: (Type.FuncParamClause, Type) => Type.FunctionType
    ): Type.FunctionType = {
      val funcParams = autoEndPos(paramPos)(params.reduceWith(Type.FuncParamClause.apply))
      next()
      ctor(funcParams, typeIndentedOpt())
    }

    private def typeCaseClauses(): Type.CasesBlock = autoPos {
      def cases() = listBy[TypeCase] { allCases =>
        while (at[KwCase]) {
          allCases += autoPos {
            next()
            val pat = infixTypeOrTuple(inMatchType = true)
            accept[RightArrow]
            TypeCase(pat, typeIndentedOpt())
          }
          newLinesOpt()
        }
      }
      (if (at[Indentation.Indent]) indentedOnOpen(cases()) else inBraces(cases()))
        .reduceWith(Type.CasesBlock.apply)
    }

    def quasiquoteType(): Type = entrypointType()

    def entrypointType(): Type = paramType()

    def typeArgs(): Type.ArgClause = TypeBracketsContext
      .within(autoPos(inBrackets(types()).reduceWith(Type.ArgClause.apply)))

    def infixTypeOrTuple(inMatchType: Boolean = false): Type =
      if (at[LeftParen]) tupleInfixType(allowFunctionType = !inMatchType)
      else infixType(inMatchType = inMatchType)

    @inline
    def infixType(inMatchType: Boolean = false): Type = maybeAnonymousLambda(
      infixTypeRest(compoundType(inMatchType = inMatchType), inMatchType = inMatchType)
    )

    @inline
    private def infixTypeRest(t: Type, inMatchType: Boolean = false): Type =
      if (dialect.useInfixTypePrecedence) infixTypeRestWithPrecedence(t, inMatchType = inMatchType)
      else
        infixTypeRestWithMode(t, InfixMode.FirstOp, t.begIndex, identity, inMatchType = inMatchType)

    @tailrec
    private final def infixTypeRestWithMode(
        t: Type,
        mode: InfixMode.Value,
        startPos: Int,
        f: Type => Type,
        inMatchType: Boolean = false
    ): Type = {
      val ok = currToken match {
        case _: Unquote | VarArgTypeParam.Non() => true
        case _ => false
      }
      if (ok) {
        val op = typeName()
        val leftAssoc = op.isLeftAssoc
        if (mode != InfixMode.FirstOp) checkAssoc(op, leftAssoc, mode == InfixMode.LeftOp)
        newLineOptWhenFollowedBy(TypeIntro)
        val typ = compoundType(inMatchType = inMatchType)
        def mkOp(t1: Type) = atPos(startPos, t1)(Type.ApplyInfix(t, op, t1))
        if (leftAssoc)
          infixTypeRestWithMode(mkOp(typ), InfixMode.LeftOp, startPos, f, inMatchType = inMatchType)
        else infixTypeRestWithMode(
          typ,
          InfixMode.RightOp,
          typ.begIndex,
          f.compose(mkOp),
          inMatchType = inMatchType
        )
      } else f(t)
    }

    private final def infixTypeRestWithPrecedence(t: Type, inMatchType: Boolean = false): Type = {
      val ctx = TypeInfixContext
      val base = ctx.stack
      @inline
      def reduce(rhs: ctx.Typ, op: Option[ctx.Op]): ctx.Typ = ctx.reduceStack(base, rhs, rhs, op)
      def getNextRhs(rhs: ctx.Typ)(op: ctx.Op): ctx.Typ = {
        newLineOptWhenFollowedBy(TypeIntro)
        ctx.push(ctx.UnfinishedInfix(reduce(rhs, Some(op)), op))
        compoundType(inMatchType = inMatchType)
      }
      @tailrec
      def loop(rhs: ctx.Typ): ctx.Typ = (currToken match {
        case lf: InfixLF => getLeadingInfix(lf)(Type.Name.apply)(getNextRhs(rhs))
        case VarArgTypeParam() => None
        case _: Ident | _: Unquote => Some(getNextRhs(rhs)(typeName()))
        case _ => None
      }) match {
        case Some(x) => loop(x)
        case None => reduce(rhs, None)
      }
      loop(t)
    }

    def compoundType(inMatchType: Boolean = false): Type = refinement(innerType = None).getOrElse {
      val startPos = currIndex
      compoundTypeRest(annotType(startPos, inMatchType = inMatchType), startPos)
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
        if (currToken.isNot[Dot]) ref match {
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
        else {
          next()
          accept[KwType]
          Type.Singleton(ref.become[Term.Ref])
        }
      }
      val res = currToken match {
        case _: Ident if peek[Dot] => pathSimpleType()
        case _: LeftParen => makeTupleType(startPos, inParensOnOpen(types()))
        case MacroSplicedIdent(ident) => Type.Macro(macroSplicedIdent(ident))
        case _: MacroSplice => Type.Macro(macroSplice())
        case _: Underscore if inMatchType => next(); Type.PatWildcard()
        case _: Underscore
            if dialect.allowUnderscoreAsTypePlaceholder ||
              dialect.allowTypeLambdas && !PatternTypeContext.isInside() &&
              TypeBracketsContext.isDeeper(1) && !peekToken.isAny[Supertype, Subtype] =>
          next(); Type.AnonymousParam(None)
        case _: Underscore => wildcardType()
        case _: Literal =>
          if (dialect.allowLiteralTypes) literal()
          else syntaxError(s"$dialect doesn't support literal types", at = path())
        case Unary.Numeric(unary) if dialect.allowLiteralTypes && tryAhead[NumericConstant[_]] =>
          numericLiteral(prevIndex, unary)
        case t: Token.Ident if !inMatchType =>
          t.text match {
            case soft.QuestionMarkAsTypeWildcard() => wildcardType()
            case soft.StarAsTypePlaceholder(value) => next(); anonymousParamWithVariant(value)
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
      simpleTypeRest(autoEndPosOpt(startPos)(res), startPos)
    }

    @tailrec
    private final def simpleTypeRest(t: Type, startPos: Int): Type = currToken match {
      case _: Hash =>
        next()
        simpleTypeRest(autoEndPos(startPos)(Type.Project(t, typeName())), startPos)
      case _: LeftBracket =>
        simpleTypeRest(autoEndPos(startPos)(Type.Apply(t, typeArgs())), startPos)
      case _ => t
    }

    def types(): List[Type] = commaSeparated(typ())

    def patternTyp(allowInfix: Boolean, allowImmediateTypevars: Boolean): Type = {
      def setPos(outerTpe: Type)(innerTpe: Type): Type =
        if (innerTpe eq outerTpe) outerTpe else copyPos(outerTpe)(innerTpe)

      def convertFuncParamClause(tree: Type.FuncParamClause): Type.FuncParamClause = tree match {
        case t: Quasi => t
        case t: Type.FuncParamClause =>
          copyPos(t)(Type.FuncParamClause(t.values.map(loop(_, convertTypevars = true))))
      }

      def loop(outerTpe: Type, convertTypevars: Boolean): Type = setPos(outerTpe)(outerTpe match {
        case q: Quasi => q
        case tpe @ Type.Name(value) if convertTypevars && value(0).isLower => Type.Var(tpe)
        case tpe: Type.Name => tpe
        case tpe: Type.Select => tpe
        case Type.Project(qual, name) =>
          val qual1 = loop(qual, convertTypevars = false)
          val name1 = name
          Type.Project(qual1, name1)
        case tpe: Type.Singleton => tpe
        case t: Type.Apply =>
          val underlying1 = loop(t.tpe, convertTypevars = false)
          val args1 = t.argClause match {
            case q: Type.ArgClause.Quasi => q
            case x: Type.ArgClause =>
              copyPos(x)(Type.ArgClause(x.values.map(loop(_, convertTypevars = true))))
          }
          Type.Apply(underlying1, args1)
        case Type.ApplyInfix(lhs, op, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val op1 = op
          val rhs1 = loop(rhs, convertTypevars = false)
          Type.ApplyInfix(lhs1, op1, rhs1)
        case t: Type.ContextFunction =>
          val params1 = convertFuncParamClause(t.paramClause)
          val res1 = loop(t.res, convertTypevars = false)
          Type.ContextFunction(params1, res1)
        case t: Type.Function =>
          val params1 = convertFuncParamClause(t.paramClause)
          val res1 = loop(t.res, convertTypevars = false)
          Type.Function(params1, res1)
        case t: Type.PolyFunction =>
          val res1 = loop(t.tpe, convertTypevars = false)
          Type.PolyFunction(t.tparamClause, res1)
        case Type.Tuple(elements) =>
          val elements1 = elements.map(loop(_, convertTypevars = true))
          Type.Tuple(elements1)
        case Type.With(lhs, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val rhs1 = loop(rhs, convertTypevars = false)
          Type.With(lhs1, rhs1)
        case Type.Refine(tpe, stats) =>
          val tpe1 = tpe.map(loop(_, convertTypevars = false))
          val stats1 = stats
          Type.Refine(tpe1, stats1)
        case Type.Existential(underlying, stats) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val stats1 = stats
          Type.Existential(underlying1, stats1)
        case Type.Annotate(underlying, annots) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val annots1 = annots
          Type.Annotate(underlying1, annots1)
        case t: Type.Wildcard => Type.Wildcard(t.bounds)
        case t: Type.AnonymousLambda => Type.AnonymousLambda(loop(t.tpe, convertTypevars = false))
        case t: Type.AnonymousParam => Type.AnonymousParam(t.variant)
        case t: Type.Placeholder =>
          val bounds1 = t.bounds
          Type.Placeholder(bounds1)
        case tpe: Lit => tpe
      })
      val t: Type = PatternTypeContext.within {
        if (allowInfix) {
          val t = if (at[LeftParen]) tupleInfixType() else compoundType()
          currToken match {
            case _: KwForsome => existentialTypeOnForSome(t)
            case _: Unquote | Keywords.NotPatAlt() => infixTypeRest(t)
            case _ => t
          }
        } else compoundType()
      }
      loop(t, convertTypevars = allowImmediateTypevars)
    }

    def patternTypeArgs() = autoPos(
      inBrackets(commaSeparated(patternTyp(allowInfix = true, allowImmediateTypevars = true)))
        .reduceWith(Type.ArgClause.apply)
    )
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
  private def termName(t: Ident): Term.Name = identName(t, Term.Name.apply)
  private def typeName(t: Ident): Type.Name = identName(t, Type.Name.apply)
  @inline
  private def anonNameEmpty(): Name.Anonymous = autoPos(Name.Anonymous())
  @inline
  private def nameThis(): Name.This = atCurPosNext(Name.This())
  @inline
  private def namePlaceholder(): Name.Placeholder = atCurPosNext(Name.Placeholder())
  private def anonThis(): Term.This = atCurPosNext(Term.This(anonNameEmpty()))

  def path(thisOK: Boolean = true): Term.Ref = {
    val startsAtBof = prev[BOF]
    def afterDot[A <: Term.Ref](ref: A)(f: PartialFunction[Token, A => Term.Ref]): Term.Ref =
      if (!at[Dot]) ref
      else f.lift(peekToken) match { case Some(f) => next(); f(ref); case _ => ref }
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
    def getAnonQual(): Name =
      try anonNameEmpty()
      finally next()
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

  def selector(t: Term, startPos: Int): Term.Select =
    autoEndPos(startPos)(Term.Select(t, termName()))
  @tailrec
  private final def selectors(t: Term, startPos: Int): Term.Ref = {
    val t1 = selector(t, startPos)
    if (at[Dot] && tryAhead[Ident]) selectors(t1, startPos) else t1
  }
  private final def selectors(t: Term): Term.Ref = selectors(t, t.begIndex)

  def mixinQualifier(): Name =
    if (acceptOpt[LeftBracket]) inBracketsAfterOpen {
      typeName().becomeOr[Name](x => copyPos(x)(Name.Indeterminate(x.value)))
    }
    else anonNameEmpty()

  def stableId(): Term.Ref = path(thisOK = false)

  def qualId(): Term.Ref = {
    val name = termName().become[Term.Ref]
    if (acceptOpt[Dot]) selectors(name) else name
  }

  private def numericLiteral(startPos: Int, unary: Unary.Numeric): Lit = {
    val number = currToken.asInstanceOf[NumericConstant[_]]
    next()
    autoEndPos(startPos)(numericLiteralWithUnaryAt(number, unary))
  }

  private def numericLiteralMaybeWithUnaryAt(
      tok: NumericConstant[_],
      unary: Unary.Numeric
  ): Either[Lit, Lit] = {
    def getBigInt(tok: NumericConstant[BigInt], dec: BigInt, hex: BigInt, typ: String) = {
      // decimal never starts with `0` as octal was removed in 2.11; "hex" includes `0x` or `0b`
      // non-decimal literals allow signed overflow within unsigned range
      val max = if (tok.text(0) != '0') dec else hex
      // token value is always positive as it doesn't take into account a sign
      val value = tok.value
      val result = unary(value)
      if (result.signum < 0) {
        if (value > max) syntaxError(s"integer number too small for $typ", at = tok)
      } else if (value >= max) syntaxError(s"integer number too large for $typ", at = tok)
      result
    }
    def getBigDecimal(tok: NumericConstant[BigDecimal], f: BigDecimal => Lit) = {
      val number = tok.value
      unary(number).fold[Either[Lit, Lit]](Left(f(number)))(x => Right(f(x)))
    }
    tok match {
      case tok: Constant.Int =>
        Right(Lit.Int(getBigInt(tok, bigIntMaxInt, bigIntMaxUInt, "Int").intValue))
      case tok: Constant.Long =>
        Right(Lit.Long(getBigInt(tok, bigIntMaxLong, bigIntMaxULong, "Long").longValue))
      case tok: Constant.Float => getBigDecimal(tok, Lit.Float.apply)
      case tok: Constant.Double => getBigDecimal(tok, Lit.Double.apply)
      case t => unreachable(t)
    }
  }

  private def numericLiteralWithUnaryAt(tok: NumericConstant[_], unary: Unary.Numeric): Lit =
    numericLiteralMaybeWithUnaryAt(tok, unary) match {
      case Right(x) => x
      case _ => syntaxError(s"bad unary op `${unary.op}` for floating-point", at = tok)
    }

  def literal(): Lit = atCurPosNext(currToken match {
    case number: NumericConstant[_] => numericLiteralWithUnaryAt(number, Unary.Noop)
    case Constant.Char(value) => Lit.Char(value)
    case Constant.String(value) => Lit.String(value)
    case t: Constant.Symbol =>
      if (dialect.allowSymbolLiterals) Lit.Symbol(t.value)
      else syntaxError("Symbol literals are no longer allowed", at = t)
    case x: BooleanConstant => Lit.Boolean(x.value)
    case _: KwNull => Lit.Null()
    case t => unreachable(t)
  })

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
      case Interpolation.Id(value) => next(); Term.Name(value)
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

  def newLineOptWhenFollowedBy(unapply: Token => Boolean): Unit = at[EOL] &&
    tryAhead(unapply(currToken))

  def newLineOptWhenFollowedBy[T: ClassTag]: Unit = at[EOL] && tryAhead[T]

  def newLineOptWhenFollowedBySignificantIndentationAnd(cond: Token => Boolean): Boolean =
    nextIf(in.indenting && cond(peekToken))

  def isAfterOptNewLine[T: ClassTag]: Boolean = if (at[EOL]) tryAhead[T] else at[T]

  def isAfterOptNewLine(body: Token => Boolean): Boolean =
    if (at[EOL]) tryAhead(body(currToken)) else body(currToken)

  def getAfterOptNewLine[A](body: => Option[A]): Option[A] = if (at[EOL]) tryAhead(body) else body

  /* ------------- TYPES ---------------------------------------------------- */

  def typedOpt(): Option[Type] =
    if (acceptOpt[Colon]) Some {
      if (at[At] && peek[Ident]) {
        val startPos = currIndex
        outPattern.annotTypeRest(autoEndPos(startPos)(Type.AnonymousName()), startPos)
      } else typ()
    }
    else None

  private def getDeclTpeOpt(fullTypeOK: Boolean): Option[Type] =
    if (acceptOpt[Colon]) Some(typeOrInfixType(fullTypeOK)) else None

  private def typeOrInfixType(fullTypeOK: Boolean): Type =
    if (fullTypeOK) typ() else startInfixType()

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
      case t: Term.Match => copyPos(t)(t.fullCopy(mods = inlineMods))
      case other => syntaxError("`inline` must be followed by an `if` or a `match`", at = other.pos)
    }

  private def matchClause(t: Term, startPos: Int) = {
    val cases = autoPos {
      if (at[Indentation.Indent]) indentedOnOpen(casesBlock()) else inBraces(casesBlock())
    }
    autoEndPos(startPos)(Term.Match(t, cases, Nil))
  }

  def ifClause(mods: List[Mod] = Nil) = autoEndPos(mods) {
    accept[KwIf]
    val (cond, thenp) = condExprWithBody[KwThen]
    val elsep = if (acceptIfAfterOpt[KwElse](StatSep)) expr() else autoPos(Lit.Unit())
    Term.If(cond, thenp, elsep, mods)
  }

  private def condExprWithBody[T <: Token: ClassTag]: (Term, Term) = {
    val (cond, bodyOpt) =
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
              val args = copyPos(t)(termInfixContext.toArgClause(t))
              autoEndPos(startPos)(Term.Apply(simpleExpr, args))
            }
            val simpleRest = simpleExprRest(simpleExprWithArgs, canApply = true, startPos = startPos)
            Try(postfixExpr(startPos, simpleRest, allowRepeated = false)).toOption.flatMap { x =>
              val exprCond = exprOtherRest(startPos, x, location = NoStat, allowRepeated = false)
              if (acceptIfAfterOptNL[T]) Some(exprCond -> None) else None
            }
          }
          complexExpr.getOrElse {
            if (argsOrInitBody.isEmpty) newLinesOpt()
            simpleExpr -> argsOrInitBody.map { t =>
              val startPos = t.begIndex
              val rest = simpleExprRest(t, canApply = true, startPos = startPos)
              val init = postfixExpr(startPos, rest, allowRepeated = false)
              exprOtherRest(startPos, init, NoStat, allowRepeated = false)
            }
          }
        }
      }
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
    val res = autoPosOpt(currToken match {
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
          case _: KwCase =>
            tryWithCases(Some(autoPos { next(); toCasesBlock(caseClause(true) :: Nil) }))
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
          } else if (acceptOpt[Indentation.Indent]) indentedAfterOpen(enumerators())
          else enumerators()
        val enums = autoPos(enumList.reduceWith(Term.EnumeratorsBlock.apply))

        newLinesOpt()
        if (acceptOpt[KwDo]) Term.For(enums, expr())
        else if (acceptOpt[KwYield]) Term.ForYield(enums, expr())
        else Term.For(enums, expr())
      case _: KwReturn =>
        next()
        if (isExprIntro(currToken, currIndex)) Term.Return(expr())
        else Term.Return(autoPos(Lit.Unit()))
      case _: KwThrow =>
        next()
        Term.Throw(expr())
      case _: KwImplicit =>
        next()
        implicitClosure(location)
      case _ =>
        val startPos = currIndex
        val t: Term = postfixExpr(allowRepeated)
        exprOtherRest(startPos, t, location, allowRepeated)
    })
    if (location.anonFuncOK) maybeAnonymousFunction(res) else res
  }

  private def exprOtherRest(
      startPos: Int,
      prefix: Term,
      location: Location,
      allowRepeated: Boolean
  ): Term = {
    @inline
    def addPos[T <: Tree](body: T) = autoEndPos(startPos)(body)
    def repeatedTerm(t: Term, nextTokens: () => Unit): Term =
      if (allowRepeated) addPos { nextTokens(); Term.Repeated(t) }
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
            if (at[At] || (at[Ellipsis] && peek[At]))
              iter(addPos(Term.Annotate(t, annots(skipNewLines = false))))
            else if (at[Underscore] && isStar(peekToken)) repeatedTerm(t, nextTwice)
            else
              // this does not necessarily correspond to syntax, but is necessary to accept lambdas
              // check out the `if (token.is[RightArrow]) { ... }` block below
              iter(addPos(Term.Ascribe(t, typeOrInfixType(location))))
        }
      case soft.StarSplice() if allowRepeated && peek[RightParen] => repeatedTerm(t, next)
      case _: KwMatch =>
        next()
        matchClause(t, startPos)
      case _ => t
    }

    val res: Term = iter(prefix)

    // Note the absense of `else if` here!!
    val contextFunction = at[ContextArrow]
    if (contextFunction || at[RightArrow])
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
      convertToParamClause(res)(
        isNameAllowed = location != TemplateStat,
        isParamAllowed = location.funcParamOK || tokens(startPos).is[LeftParen] && prev[RightParen]
      ).fold(res) { pc =>
        val params = addPos(pc)
        next()
        val trm = termFunctionBody(location)
        addPos {
          if (contextFunction) Term.ContextFunction(params, trm) else Term.Function(params, trm)
        }
      }
    // if couldn't convert to params:
    // do nothing, which will either allow self-type annotation parsing to kick in
    // or will trigger an unexpected token error down the line
    else res
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
      case t: Term.Select => getModFromName(t.name) match {
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
      case t: Term.Select => getMod(t.qual).map((t.name, _))
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

    tree match {
      case q: Quasi => Some(q.become[Term.Param])
      case _: Lit.Unit => None
      case t: Term.Ascribe => getNameAndMod(t.expr).map { case (name, mod) =>
          copyPos(t)(Term.Param(mod, name, Some(t.tpe), None))
        }
      case t => getNameAndMod(t).map { case (name, mod) =>
          copyPos(t)(Term.Param(mod, name, None, None))
        }
    }
  }

  private def convertToParamClause(
      tree: Term
  )(isNameAllowed: => Boolean, isParamAllowed: => Boolean): Option[Term.ParamClause] = (tree match {
    case _: Lit.Unit => Some(Nil)
    case q: Quasi => q.rank match {
        case 0 if isNameAllowed => Some(q.become[Term.Param] :: Nil)
        case 1 if isParamAllowed => Some(q.become[Term.Param] :: Nil)
        case _ => None
      }
    case t: Term.Tuple =>
      val params = new ListBuffer[Term.Param]
      @tailrec
      def iter(ts: List[Term]): Option[List[Term.Param]] = ts match {
        case Nil => Some(params.toList)
        case head :: tail => convertToParam(head) match {
            case None => None
            case Some(p) => params += p; iter(tail)
          }
      }
      iter(t.args)
    case t => convertToParam(t)
        .filter(p => if (p.decltpe.isEmpty && p.mods.isEmpty) isNameAllowed else isParamAllowed)
        .map(_ :: Nil)
  }).map(_.reduceWith(toParamClause(None)))

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
    def head = stack.head
    def push(unfinishedInfix: UnfinishedInfix): Unit = stack ::= unfinishedInfix
    def pop(): UnfinishedInfix =
      try head
      finally stack = stack.tail

    def reduceStack(stack: List[UnfinishedInfix], curr: Typ, currEnd: EndPos, op: Option[Op]): Typ =
      if (isDone(stack)) curr
      else {
        val opPrecedence = op.fold(0)(_.precedence)
        val leftAssoc = op.forall(_.isLeftAssoc)

        def lowerPrecedence = opPrecedence < this.head.precedence
        def samePrecedence = opPrecedence == this.head.precedence
        def canReduce = lowerPrecedence || leftAssoc && samePrecedence

        if (samePrecedence) checkAssoc(this.head.op, leftAssoc)

        // Pop off an unfinished infix expression off the stack and finish it with the rhs.
        // Then convert the result, so that it can become someone else's rhs.
        // Repeat while precedence and associativity allow.
        @tailrec
        def loop(rhs: Typ): Typ =
          if (!canReduce) rhs
          else {
            val lhs = pop()
            val fin = finishInfixExpr(lhs, rhs, currEnd)
            if (isDone(stack)) fin else loop(fin)
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
  object termInfixContext extends InfixContext {
    type Typ = Term
    type Op = Term.Name

    // We need to carry lhsStart/lhsEnd separately from lhs.pos
    // because their extent may be bigger than lhs because of parentheses or whatnot.
    case class UnfinishedInfix(lhs: Typ, op: Op, targs: Type.ArgClause) extends Unfinished {
      override def toString = s"[$lhs $op$targs]"
    }

    def toArgClause(rhs: Typ): Term.ArgClause = copyPos(rhs)(
      (rhs match {
        case _: Lit.Unit => Nil
        case t: Term.Tuple => t.args
        case _ => rhs :: Nil
      }).reduceWith(Term.ArgClause(_))
    )

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhsExt, op, targs) = unf
      val lhs = lhsExt match {
        case Term.Tuple(arg :: Nil) => arg
        case x => x
      }

      if (lhs.is[Term.Repeated])
        syntaxError("repeated argument not allowed here", at = lhs.tokens.last)

      atPos(lhsExt, rhsEnd)(Term.ApplyInfix(lhs, op, targs, toArgClause(rhs)))
    }
  }

  // In comparison with terms, patterns are trivial.
  implicit object patInfixContext extends InfixContext {
    type Typ = Pat
    type Op = Term.Name

    case class UnfinishedInfix(lhs: Typ, op: Op) extends Unfinished

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhsExt, op) = unf
      val lhs = lhsExt match {
        case Pat.Tuple(arg :: Nil) => arg
        case x => x
      }
      val args = copyPos(rhs)(
        (rhs match {
          case _: Lit.Unit => Nil
          case t: Pat.Tuple => t.args
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

  @inline
  def checkAssoc(op: Name, leftAssoc: Boolean): Unit = checkAssoc(op, op.isLeftAssoc, leftAssoc)

  @inline
  private def checkAssoc(at: Tree, opLeftAssoc: Boolean, leftAssoc: Boolean): Unit =
    if (opLeftAssoc != leftAssoc) syntaxError(
      "left- and right-associative operators with same precedence may not be mixed",
      at = at
    )

  private def getLeadingInfix[A <: Name, B](lf: InfixLF)(f: String => A)(g: A => B): Option[B] =
    peekToken match {
      case op: Ident =>
        def res = Some(g(atCurPos { next(); newLineOpt(); f(op.value) }))
        tryAhead(if (peek[Indentation]) None else lf.invalid.fold(res)(syntaxError(_, at = op)))
      case _ => None
    }

  def postfixExpr(allowRepeated: Boolean): Term = {
    val startPos = currIndex

    // Start the infix chain.
    // We'll use `a + b` as our running example.
    val rhs0 = prefixExpr(allowRepeated)

    postfixExpr(startPos, rhs0, allowRepeated)
  }

  private def postfixExpr(startPos: Int, rhs0: Term, allowRepeated: Boolean): Term = {
    val ctx = termInfixContext
    val base = ctx.stack

    def getLhsStartPos(lhs: ctx.Typ): Int = if (lhs eq rhs0) startPos else lhs.begIndex

    // Skip to later in the `postfixExpr` method to start mental debugging.
    // rhsStartK/rhsEndK may be bigger than then extent of rhsK,
    // so we really have to track them separately.
    @tailrec
    def loop(rhsK: ctx.Typ): ctx.Typ = {
      val rhsEndK = prevIndex

      def getPrevLhs(op: Term.Name): Term = ctx.reduceStack(base, rhsK, rhsEndK, Some(op))

      def getNextRhs(targs: => Type.ArgClause)(op: Term.Name) =
        getNextRhsWith(op, targs, argumentExprsOrPrefixExpr(PostfixStat))

      def getNextRhsWith(op: Term.Name, targs: Type.ArgClause, rhs: Term) = {
        val lhs = getPrevLhs(op)
        val wrap = (lhs eq rhs0) && lhs.begIndex != startPos
        val lhsExt = if (wrap) atPosWithBody(startPos, Term.Tuple(lhs :: Nil), rhsEndK) else lhs
        ctx.push(ctx.UnfinishedInfix(lhsExt, op, targs))
        Right(rhs)
      }

      def getPostfix(op: Term.Name, targs: Type.ArgClause) = {
        // Infix chain has ended with a postfix expression.
        // This never happens in the running example.
        if (targs.nonEmpty)
          syntaxError("type application is not allowed for postfix operators", at = currToken)
        val finQual = getPrevLhs(op)
        val term: Term = atPos(getLhsStartPos(finQual), op)(Term.Select(finQual, op))
        Left(term)
      }

      def emptyTypeArgs = autoPos(Type.ArgClause(Nil))

      def getPostfixOrNextRhs(op: Term.Name): Either[Term, Term] = {
        // Infix chain continues.
        // In the running example, we're at `a [+] b`.
        val targs = if (at[LeftBracket]) exprTypeArgs() else emptyTypeArgs

        // Check whether we're still infix or already postfix by testing the current token.
        // In the running example, we're at `a + [b]` (infix).
        // If we were parsing `val c = a b`, then we'd be at `val c = a b[]` (postfix).
        if (if (at[EOL]) nextIf(isExprIntro(peekToken, peekIndex))
          else isIdentOrExprIntro(currToken))
          // Infix chain continues, so we need to reduce the stack.
          // In the running example, base = List(), rhsK = [a].
          getNextRhs(targs)(op) // [a]
        // afterwards, ctx.stack = List([a +])
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
        case t: Ident if !(allowRepeated && soft.StarSplice(t) && peek[RightParen]) =>
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
    if (rhs0 == rhsN && ctx.isDone(base)) rhs0
    else {
      val endPos = prevIndex
      atPosWithBody(startPos, ctx.reduceStack(base, rhsN, endPos, None), endPos)
    }
  }

  def prefixExpr(allowRepeated: Boolean): Term = currToken match {
    case Unary((ident, unary)) =>
      val startPos = currIndex
      next()
      def op = atPos(startPos)(Term.Name(ident))
      def addPos(tree: Term) = autoEndPos(startPos)(tree)
      def rest(tree: Term) = simpleExprRest(tree, canApply = true, startPos = startPos)
      def applyUnary(term: Term) = addPos(Term.ApplyUnary(op, term))
      def otherwise = simpleExpr0(allowRepeated = true) match {
        case Success(result) => applyUnary(result)
        case Failure(_) =>
          // maybe it is not unary operator but simply an ident `trait - {...}`
          // we would fail here anyway, let's try to treat it as ident
          rest(op)
      }
      (currToken, unary) match {
        case (tok: NumericConstant[_], unary: Unary.Numeric) =>
          next(); rest(numericLiteralMaybeWithUnaryAt(tok, unary).fold(applyUnary, addPos))
        case (tok: BooleanConstant, unary: Unary.Logical) =>
          next(); rest(addPos(Lit.Boolean(unary(tok.value))))
        case _ => otherwise
      }
    case _ => simpleExpr(allowRepeated)
  }

  def simpleExpr(allowRepeated: Boolean): Term = simpleExpr0(allowRepeated).get

  private def simpleExpr0(allowRepeated: Boolean): Try[Term] = {
    var canApply = true
    val startPos = currIndex
    (currToken match {
      case _: MacroQuote => Success(macroQuote())
      case _: MacroSplice => Success(macroSplice())
      case MacroQuotedIdent(ident) => Success(macroQuotedIdent(ident))
      case MacroSplicedIdent(ident) => Success(macroSplicedIdent(ident))
      case _: Literal => Success(literal())
      case _: Interpolation.Id => Success(interpolateTerm())
      case _: Xml.Start => Success(xmlTerm())
      case _: Ident | _: KwThis | _: KwSuper | _: Unquote => Success(path().become[Term])
      case _: Underscore => Success(atCurPosNext(Term.Placeholder()))
      case _: LeftParen => Success(inParensOrTupleOrUnitExpr(allowRepeated = allowRepeated))
      case _: LeftBrace => canApply = false; Success(blockExprOnBrace())
      case _: Indentation.Indent => canApply = false; Success(blockExprOnIndent())
      case _: KwNew =>
        canApply = false
        Success(autoPos {
          next()
          val tpl = template(OwnedByTrait)
          tpl.inits match {
            case init :: Nil if !prev[RightBrace] && tpl.earlyClause.isEmpty && tpl.body.isEmpty =>
              Term.New(init)
            case _ => Term.NewAnonymous(tpl)
          }
        })
      case _: LeftBracket if dialect.allowPolymorphicFunctions => Success(polyFunction())
      case _ => Failure(ParseException(currToken.pos, "illegal start of simple expression"))
    }) match {
      case Success(x) => Success(simpleExprRest(x, canApply = canApply, startPos = startPos))
      case x: Failure[_] => x
    }
  }

  def polyFunction() = autoPos {
    val quants = typeParamClauseOpt(ownerIsType = true)
    accept[RightArrow]
    val term = expr()
    Term.PolyFunction(quants, term)
  }

  private def macroSplice(): Term = autoPos(QuotedSpliceContext.within {
    next()
    if (QuotedPatternContext.isInside()) Term.SplicedMacroPat(autoPos(inBraces(pattern())))
    else Term.SplicedMacroExpr(autoPos(inBraces(blockRaw())))
  })

  private def macroQuote(): Term = autoPos(QuotedSpliceContext.within {
    next()
    currToken match {
      case _: LeftBrace => Term.QuotedMacroExpr(autoPos(inBracesOnOpen(blockRaw())))
      case _: LeftBracket => Term.QuotedMacroType(inBracketsOnOpen(typeBlock()))
      case t => syntaxError("Quotation only works for expressions and types", at = t)
    }
  })

  @inline
  private def macroQuotedIdent(ident: String): Term = macroIdent(ident, Term.QuotedMacroExpr.apply)

  @inline
  private def macroSplicedIdent(ident: String): Term = macroIdent(ident, Term.SplicedMacroExpr.apply)

  private def macroIdent(ident: String, f: Term.Name => Term): Term = {
    val curpos = currIndex
    next()
    autoEndPos(curpos)(f(atPos(curpos)(Term.Name(ident))))
  }

  @tailrec
  private def simpleExprRest(t: Term, canApply: Boolean, startPos: Int): Term = {
    @inline
    def addPos(body: Term): Term = autoEndPos(startPos)(body)
    if (canApply)
      if (dialect.allowSignificantIndentation)
        newLineOptWhenFollowedBySignificantIndentationAnd(_.isAny[LeftBrace, LeftParen])
      else newLineOptWhenFollowedBy[LeftBrace]
    currToken match {
      case _: Dot =>
        next()
        if (dialect.allowMatchAsOperator && acceptOpt[KwMatch]) {
          val clause = matchClause(t, startPos)
          // needed if match uses significant identation
          newLineOptWhenFollowedBy[Dot]
          simpleExprRest(clause, canApply = false, startPos = startPos)
        } else simpleExprRest(selector(t, startPos), canApply = true, startPos = startPos)
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
          while (at[LeftBracket]) app = addPos(Term.ApplyType(app, exprTypeArgs()))
          simpleExprRest(app, canApply = true, startPos = startPos)
        } else addPos(t)
      case tok @ (_: LeftParen | _: LeftBrace) if canApply =>
        def argClause = if (tok.is[LeftBrace]) getArgClauseOnBrace() else getArgClauseOnParen()
        val arguments = addPos(Term.Apply(t, argClause))
        simpleExprRest(arguments, canApply = true, startPos = startPos)
      case _: Colon if canApply => getFewerBracesApplyOnColon(t, startPos).getOrElse(t)
      case _: Underscore if canApply =>
        next()
        addPos(Term.Eta(t))
      case _ => t
    }
  }

  private def getFewerBracesArgOnColon(): Option[Term] =
    if (!dialect.allowFewerBraces) None
    else {
      val colonPos = currIndex
      def addPos(term: Term) = autoEndPos(colonPos)(term)
      def tryGetArgAsLambdaBlock(postCheck: => Boolean) = tryGetArgAsLambda().flatMap { arg =>
        if (postCheck) Some(addPos(toBlockRaw(arg :: Nil))) else None
      }
      peekToken match {
        case _: Indentation.Indent =>
          next()
          tryAhead(tryGetArgAsLambdaBlock(acceptIfAfterOptNL[Indentation.Outdent])).orElse {
            Some(addPos(blockExprOnIndent(keepBlock = true)))
          }
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

  private def tryGetArgAsLambda(): Option[Term.FunctionTerm] = Try {
    val paramPos = currIndex

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
    @tailrec
    def getParamClause(pt: Option[Mod.ParamsType], mods: List[Mod]): Term.ParamClause =
      currToken match {
        case _: KwImplicit if pt.isEmpty =>
          val mod = atCurPosNext(Mod.Implicit())
          getParamClause(Some(mod), mod :: mods)
        case _ => commaSeparated(getParamWithPos(mods, fullTypeOK = true))
            .reduceWith(toParamClause(pt))
      }
    def getParamAsClause(pt: Option[Mod.ParamsType], mods: List[Mod]): Option[Term.ParamClause] = {
      val param = getParamWithPos(mods, fullTypeOK = false)
      Some(copyPos(param)(reduceAs(param :: Nil, toParamClause(pt))))
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

    val paramClauseOpt = currToken match {
      case _: LeftParen =>
        Some(autoPos(inParensOnOpenOr(getParamClause(None, Nil))(Term.ParamClause(Nil))))
      case t: Ellipsis if t.rank == 2 => Some(ellipsis[Term.ParamClause](t))
      case _: Ident | _: Underscore | _: Ellipsis => getParamAsClause(None, Nil)
      case _: KwImplicit =>
        val mod = atCurPosNext(Mod.Implicit())
        getParamAsClause(Some(mod), mod :: Nil)
      case _ => None
    }

    paramClauseOpt.flatMap { params =>
      val contextFunction = at[ContextArrow]
      if ((contextFunction || at[RightArrow]) && nextIfIndentAhead()) Some {
        val trm = blockExprOnIndent()
        autoEndPos(paramPos) {
          if (contextFunction) Term.ContextFunction(params, trm) else Term.Function(params, trm)
        }
      }
      else None
    }
  }.getOrElse(None)

  private def getFewerBracesApplyOnColon(fun: Term, startPos: Int): Option[Term] = {
    val colonPos = currIndex
    getFewerBracesArgOnColon().map { arg =>
      val endPos = AutoPos.endIndex
      val argClause = atPos(colonPos, endPos)(Term.ArgClause(arg :: Nil))
      val arguments = atPos(startPos, endPos)(Term.Apply(fun, argClause))
      simpleExprRest(arguments, canApply = true, startPos = startPos)
    }
  }

  private def argumentExprsOrPrefixExpr(location: Location): Term = {
    val isBrace = at[LeftBrace]
    if (!isBrace && currToken.isNot[LeftParen]) prefixExpr(allowRepeated = false)
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
            if (isBrace) Right(res) else Left(res :: Nil)
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

  private def argumentExprsInParens(location: Location = NoStat): List[Term] = {
    @tailrec
    def checkRep(exprsLeft: List[Term]): Unit = exprsLeft match {
      case head :: tail =>
        if (!head.is[Term.Repeated]) checkRep(tail)
        else if (tail.nonEmpty) syntaxError("repeated argument not allowed here", at = head)
      case _ =>
    }
    val exprs = commaSeparated(argumentExpr(location))
    checkRep(exprs)
    exprs
  }

  private def checkNoTripleDot[T <: Tree](tree: T): T = tree match {
    case q: Quasi if q.rank == 2 => syntaxError(Messages.QuasiquoteRankMismatch(q.rank, 1), at = q)
    case t => t
  }

  private def isCaseIntro(): Boolean = at[KwCase] && isCaseIntroOnKwCase()

  // call it only if token is KwCase
  private def isCaseIntroOnKwCase(): Boolean = !peekToken.isClassOrObject

  private def blockExprPartial[T <: Token: ClassTag](orElse: => Term): Term = {
    val isPartial = peekToken match {
      case _: KwCase => ahead(isCaseIntroOnKwCase())
      case _: Ellipsis => ahead(peek[KwCase])
      case _ => false
    }
    if (isPartial) autoPos(next {
      try Term.PartialFunction(caseClauses())
      finally acceptAfterOptNL[T]
    })
    else orElse
  }

  private def blockRaw(allowRepeated: Boolean = false): Term.Block =
    toBlockRaw(blockStatSeq(allowRepeated = allowRepeated))

  private def blockOnIndent(keepBlock: Boolean = false): Term = autoPosOpt {
    indentedOnOpen(blockStatSeq() match {
      case (t: Term) :: Nil if !(keepBlock || isPrecededByDetachedComment(currIndex, t.endIndex)) =>
        t
      case stats => toBlockRaw(stats)
    })
  }
  private def blockExprOnIndent(keepBlock: Boolean = false): Term =
    blockExprPartial[Indentation.Outdent](blockOnIndent(keepBlock))

  private def blockOnBrace(fstats: => List[Stat]): Term = autoPos(toBlockRaw(inBracesOnOpen(fstats)))
  private def blockOnBrace(allowRepeated: Boolean = false): Term =
    blockOnBrace(blockStatSeq(allowRepeated = allowRepeated))
  private def blockExprOnBrace(allowRepeated: Boolean = false, isOptional: Boolean = false): Term =
    blockExprPartial[RightBrace] {
      if (isOptional) blockOnOther(allowRepeated) else blockOnBrace(allowRepeated)
    }

  private def blockOnOther(allowRepeated: Boolean = false): Term = autoPosOpt {
    blockStatSeq(allowRepeated = allowRepeated) match {
      case (term: Term) :: Nil => term
      case stats => toBlockRaw(stats)
    }
  }

  def caseClause(forceSingleExpr: Boolean = false): Case = {
    expectNot[KwCase]("Unexpected `case`")
    autoEndPos(prevIndex) {
      def caseBody() = {
        accept[RightArrow]
        val start = currIndex
        def parseStatSeq() = blockStatSeq() match {
          case List(q: Quasi) => q.become[Term]
          case List(term: Term) => term
          case other => autoEndPos(start)(Term.Block(other))
        }
        if (acceptOpt[Indentation.Indent]) indentedAfterOpen(parseStatSeq())
        else if (forceSingleExpr) expr(location = BlockStat, allowRepeated = false)
        else parseStatSeq()
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
        currToken match {
          case t: Unquote =>
            cases += unquote[Case](t)
            skipAllStatSep()
          case _ =>
            cases += caseClause()
            if (StatSep(currToken)) tryAhead(isCaseIntro())
        }
        iter()
      case _ =>
    }
    iter()
    if (cases.isEmpty) None else Some(cases.toList)
  }

  private def guardOnIf(): Term = {
    next()
    autoPos(postfixExpr(allowRepeated = false))
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
    } {
      if (StatSep(currToken)) nextIf(notEnumsEnd(peekToken))
      else isImplicitStatSep() && notEnumsEnd(currToken)
    }
  }

  private def enumerator(isFirst: Boolean = false): Enumerator = currToken match {
    case _: KwIf if !isFirst => enumeratorGuardOnIf()
    case t: Ellipsis => ellipsis[Enumerator](t, 1)
    case t: Unquote if !peekToken.isAny[Equals, LeftArrow] => unquote[Enumerator](t) // support for q"for ($enum1; ..$enums; $enum2)"
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
      if (hasEq)
        deprecationWarning("val keyword in for comprehension is deprecated", at = currToken)
      else syntaxError("val in for comprehension must be followed by assignment", at = currToken)

    if (hasEq) next() else accept[LeftArrow]
    val rhs = expr()

    autoEndPos(startPos) {
      if (hasEq) Enumerator.Val(pat, rhs)
      else if (isCase) Enumerator.CaseGenerator(pat, rhs)
      else Enumerator.Generator(pat, rhs)
    }
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
        val isArgListEnd = currToken.isAny[RightParen, RightBrace, EOF]
        if (isArgListEnd) Some(Pat.SeqWildcard()) else None
      }
      else None

    def pattern1(isForComprehension: Boolean = false): Pat = {
      val p = pattern2(isForComprehension)
      @inline
      def typed() = Pat
        .Typed(p, super.patternTyp(allowInfix = false, allowImmediateTypevars = false))
      val pat = p match {
        case _ if currToken.isNot[Colon] => p
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
        case _ if currToken.isNot[At] => p
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
      val ctx = patInfixContext
      val lhs = simplePattern(badPattern3, isForComprehension = isForComprehension)
      val base = ctx.stack
      @tailrec
      def loop(rhs: ctx.Typ): ctx.Typ = {
        @inline
        def lhs(opOpt: Option[Term.Name]) = ctx.reduceStack(base, rhs, rhs, opOpt)
        currToken match {
          case _: Unquote | Keywords.NotPatAlt() =>
            val op = termName()
            expectNot[LeftBracket]("infix patterns cannot have type arguments")
            ctx.push(ctx.UnfinishedInfix(lhs(Some(op)), op))
            loop(simplePattern(badPattern3, isRhs = true))
          case _ => lhs(None)
        }
      }
      loop(lhs)
    }

    def badPattern3(tok: Token): Nothing = {
      import patInfixContext._
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
    ): Pat = {
      val startPos = currIndex
      autoEndPos(startPos)(currToken match {
        case sidToken @ (_: Ident | _: KwThis | _: Unquote) =>
          val sid = stableId()
          (currToken, sidToken) match {
            case (_: NumericConstant[_], Unary.Numeric(unary)) if prevIndex == startPos =>
              return numericLiteral(startPos, unary)
            case _ =>
          }
          val targs = if (at[LeftBracket]) Some(super.patternTypeArgs()) else None
          if (at[LeftParen]) {
            val ref = sid.become[Term]
            Pat.Extract(
              targs.fold(ref)(x => autoEndPos(sid)(Term.ApplyType(ref, x))),
              autoPos(argumentPatterns().reduceWith(Pat.ArgClause.apply))
            )
          } else {
            targs.foreach { x =>
              syntaxError(s"pattern must be a value or have parens: $sid$x", at = currToken)
            }
            sid match {
              case name: Term.Name if isNamedTupleOk && acceptOpt[Equals] =>
                Pat.Assign(name, noSeqWithNamed.pattern())
              case name: Term.Name.Quasi => name.become[Pat]
              case name: Term.Name =>
                if (soft.StarSplice(currToken) && tryAheadNot[Ident]) Pat.Repeated(name)
                else if (if (!isForComprehension && sidToken.isBackquoted) currToken.isAny[Colon, At]
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
        case _: Literal => literal()
        case _: Interpolation.Id => interpolatePat()
        case _: Xml.Start => xmlPat()
        case _: LeftParen =>
          val lpPos = currIndex
          val patterns = inParensOnOpenOr(noSeqWithNamed.patterns())(Nil)
          makeTuple(lpPos, patterns, Lit.Unit(), Pat.Tuple.apply) {
            case t if !isRhs => Right(t)
            case t @ Pat.Tuple(_ :: Nil) => Right(t)
            case t => Left(t :: Nil)
          }
        case _: MacroQuote => QuotedPatternContext.within(Pat.Macro(macroQuote()))
        case MacroQuotedIdent(ident) => Pat.Macro(macroQuotedIdent(ident))
        case _: KwGiven =>
          next()
          Pat.Given(super.patternTyp(allowInfix = false, allowImmediateTypevars = false))
        case t => onError(t)
      })
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
  object noSeqWithNamed extends SeqContextSensitive {
    val isSequenceOK = false
    override def isNamedTupleOk: Boolean = true
  }

  /**
   * These are default entry points into the pattern context sensitive methods: they are all
   * initiated from non-pattern context.
   */
  def typ() = outPattern.typ()
  private def typeBlock() = outPattern.typeBlock()
  def typeIndentedOpt() = outPattern.typeIndentedOpt()
  def quasiquoteType() = outPattern.quasiquoteType()
  def entrypointType() = outPattern.entrypointType()
  def startInfixType() = outPattern.infixType()
  def startModType() = outPattern.annotType()
  def exprTypeArgs() = outPattern.typeArgs()
  def exprSimpleType() = outPattern.simpleType()

  /** Default entry points into some pattern contexts. */
  def pattern(): Pat = noSeq.pattern()
  def quasiquotePattern(): Pat = seqOK.quasiquotePattern()
  def entrypointPattern(): Pat = seqOK.entrypointPattern()
  def unquotePattern(): Pat = noSeq.unquotePattern()
  def unquoteSeqPattern(): Pat = seqOK.unquotePattern()
  def seqPatterns(): List[Pat] = seqOK.patterns()
  def argumentPattern(): Pat = seqOK.pattern()
  def argumentPatterns(): List[Pat] = inParens(if (at[RightParen]) Nil else seqPatterns())
  def xmlLiteralPattern(): Pat = syntaxError("XML literals are not supported", at = currToken)
  def patternTyp() = noSeq.patternTyp(allowInfix = true, allowImmediateTypevars = false)
  def patternTypeArgs() = noSeq.patternTypeArgs()

  /* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  private def privateModifier(): Mod = accessModifier(Mod.Private(_))

  private def protectedModifier(): Mod = accessModifier(Mod.Protected(_))

  private def accessModifier(mod: Ref => Mod): Mod = autoPos {
    next()
    if (!acceptOpt[LeftBracket]) mod(anonNameEmpty())
    else {
      val result = mod {
        if (at[KwThis]) anonThis()
        else termName().becomeOr[Ref](x => copyPos(x)(Name.Indeterminate(x.value)))
      }
      accept[RightBracket]
      result
    }
  }

  private def modifier(isLocal: Boolean): Mod = {
    val mod = currToken match {
      case t: Unquote => unquote[Mod](t)
      case t: Ellipsis => ellipsis[Mod](t, 1)
      case _: KwAbstract => atCurPosNext(Mod.Abstract())
      case _: KwFinal => atCurPosNext(Mod.Final())
      case _: KwSealed => atCurPosNext(Mod.Sealed())
      case _: KwImplicit => atCurPosNext(Mod.Implicit())
      case _: KwLazy => atCurPosNext(Mod.Lazy())
      case _: KwOverride if !isLocal => atCurPosNext(Mod.Override())
      case _: KwPrivate if !isLocal => privateModifier()
      case _: KwProtected if !isLocal => protectedModifier()
      case _ => currToken.text match {
          case soft.KwInline() => atCurPosNext(Mod.Inline())
          case soft.KwInfix() => atCurPosNext(Mod.Infix())
          case soft.KwOpen() if !isLocal => atCurPosNext(Mod.Open())
          case soft.KwOpaque() => atCurPosNext(Mod.Opaque())
          case soft.KwTransparent() => atCurPosNext(Mod.Transparent())
          case soft.KwErased() => atCurPosNext(Mod.Erased())
          case soft.KwTracked() => atCurPosNext(Mod.Tracked())
          case n =>
            val local = if (isLocal) "local " else ""
            syntaxError(s"${local}modifier expected but $n found", at = currToken)
        }
    }
    newLinesOpt()
    mod
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
    case t: Unquote => if (peekToken.isAny[Ident, Unquote]) buf += unquote[Mod](t)
    case t: Ellipsis => buf += ellipsis[Mod](t, 1)
    case TParamVariant(mod) => buf += atCurPosNext(mod)
    case _ =>
  }

  def modifiersBuf(
      buf: ListBuffer[Mod],
      isLocal: Boolean = false,
      isParams: Boolean = false
  ): Unit = {
    val mods = buf.view.drop(buf.length)
    def appendMod: Unit = {
      val mod = modifier(isLocal)
      buf += mod
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
      case NonParamsModifier() if isParams =>
      case _: Unquote if continueLoop =>
      case _: AtEOL if !isLocal => next(); loop
      case _: Unquote | _: Ellipsis => appendMod; loop
      case _: KwCase if peekToken.isAny[KwObject, KwClass] => buf += atCurPosNext(Mod.Case())
      case _ if isModifier(currIndex) => appendMod; loop
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
        annots +=
          (currToken match {
            case t: Unquote => unquote[Mod.Annot](t)
            case _ => autoEndPos(prevIndex) {
                Mod.Annot(initRest(exprSimpleType(), allowArgss, insidePrimaryCtorAnnot))
              }
          })
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

  def memberParamClauseGroups(ownerIsType: Boolean): List[Member.ParamClauseGroup] =
    listBy[Member.ParamClauseGroup] { groups =>
      @tailrec
      def iter(): Unit = getAfterOptNewLine(currToken match {
        case _: LeftParen =>
          val tparams = emptyTypeParams
          Some(autoPos {
            termParamClausesOnParen(ownerIsType, ellipsisMaxRank = 3) match {
              case (x: Quasi) :: Nil if x.rank == 2 => reellipsis[Member.ParamClauseGroup](x, 1)
              case x => Member.ParamClauseGroup(tparams, x)
            }
          })
        case _: LeftBracket => Some(autoPos {
            val tparamClause = typeParamClauseOnBracket(ownerIsType)
            val paramClauses = termParamClauses(ownerIsType)
            Member.ParamClauseGroup(tparamClause, paramClauses)
          })
        case _ => None
      }) match {
        case Some(group) =>
          groups += group
          // can't have consecutive type clauses (so params must be present)
          // also, only the very last param may contain implicit
          val ok = group.is[Quasi] || group.paramClauses.lastOption.exists { x =>
            x.is[Quasi] || !x.mod.is[Mod.Implicit]
          }
          if (ok) iter()
        case _ =>
      }
      iter()
    }

  def termParamClauses(ownerIsType: Boolean, ownerIsCase: Boolean = false): List[Term.ParamClause] =
    if (!isAfterOptNewLine[LeftParen]) Nil else termParamClausesOnParen(ownerIsType, ownerIsCase)

  private def termParamClausesOnParen(
      ownerIsType: Boolean,
      ownerIsCase: Boolean = false,
      ellipsisMaxRank: Int = 2
  ): List[Term.ParamClause] = {
    var hadModImplicit = false
    def paramClause(first: Boolean) = autoPos(
      inParensOnOpenOr {
        def reduceParams(params: List[Term.Param], mod: Option[Mod.ParamsType] = None) = params
          .reduceWith { x =>
            onlyLastParameterCanBeRepeated(x)
            toParamClause(mod)(x)
          }
        def parseParams(mod: Option[Mod.ParamsType] = None) = reduceParams(
          commaSeparatedWithIndex(termParam(ownerIsCase && first, ownerIsType, mod = mod)),
          mod
        )
        currToken match {
          case t @ Ellipsis(rank) if rank >= 2 && rank <= ellipsisMaxRank =>
            reduceParams(List(ellipsis[Term.Param](t)))
          case _: KwImplicit =>
            hadModImplicit = true
            parseParams(Some(atCurPosNext(Mod.Implicit())))
          case soft.KwUsing() => parseParams(Some(atCurPosNext(Mod.Using())))
          case _ => parseParams()
        }
      }(Term.ParamClause(Nil))
    )

    listBy[Term.ParamClause] { paramss =>
      paramss += paramClause(true)
      while (isAfterOptNewLine[LeftParen] && !hadModImplicit) paramss += paramClause(false)
    }
  }

  def paramType(): Type = autoPosOpt(currToken match {
    case _: RightArrow =>
      val t = autoPos {
        next()
        Type.ByName(typ())
      }
      if (isStar && dialect.allowByNameRepeatedParameters) {
        next()
        Type.Repeated(t)
      } else t
    case _ =>
      val t = typ()
      if (!isStar) t
      else {
        next()
        Type.Repeated(t)
      }
  })

  def termParam(ownerIsCase: Boolean, ownerIsType: Boolean, mod: Option[Mod.ParamsType] = None)(
      paramIdx: Int
  ): Term.Param = autoPos {
    val mods = new ListBuffer[Mod]
    annotsBuf(mods, skipNewLines = false)
    val numAnnots = mods.length
    if (ownerIsType) modifiersBuf(mods, isParams = true)
    else {
      @tailrec
      def otherMods(): Unit = if (peekToken.isNot[Colon]) {
        val mod = currToken.text match {
          case soft.KwInline() => Mod.Inline()
          case soft.KwErased() => Mod.Erased()
          case _ => null
        }
        if (mod ne null) {
          mods += atCurPosNext(mod)
          otherMods()
        }
      }
      otherMods()
    }
    val hasExplicitMods = mods.view.drop(numAnnots).exists {
      case _: Mod.Quasi => false
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
      case _ if !ownerIsType => None
      case _: KwVal => Some(atCurPosNext(Mod.ValParam()))
      case _: KwVar => Some(atCurPosNext(Mod.VarParam()))
      case _ => if (hasExplicitMods) syntaxErrorExpected[KwVal]; None
    }
    varOrVarParamMod.foreach(mods += _)

    def endParamQuasi = currToken.isAny[RightParen, Comma]
    mods.headOption.collect { case q: Mod.Quasi if endParamQuasi => q.become[Term.Param] }
      .orElse(currToken match {
        case t: Ellipsis => Some(ellipsis[Term.Param](t, 1))
        case _ => None
      }).getOrElse {
        val anonymousUsing = mod.is[Mod.Using] && !peek[Colon]
        val name = if (anonymousUsing) anonNameEmpty() else termName().become[Name]
        name match {
          case q: Quasi if endParamQuasi => q.become[Term.Param]
          case _ =>
            val tpt =
              if (currToken.isNot[Colon] && name.is[Name.Quasi]) None
              else {
                if (!anonymousUsing) accept[Colon]
                val tpt = paramType()
                if (tpt.is[Type.ByName]) {
                  def mayNotBeByName(mod: Mod) =
                    syntaxError(s"`$mod' parameters may not be call-by-name", at = name)
                  val isLocalToThis: Boolean = (!ownerIsCase && varOrVarParamMod.isEmpty) ||
                    mods.exists { case Mod.Private(_: Term.This) => true; case _ => false }
                  val badMod =
                    if (ownerIsType && !isLocalToThis) varOrVarParamMod
                    else if (dialect.allowImplicitByNameParameters) None
                    else mod.find(_.is[Mod.Implicit])
                  badMod.foreach(mayNotBeByName)
                }
                Some(tpt)
              }
            val default = if (acceptOpt[Equals]) Some(expr()) else None
            Term.Param(mods.toList, name, tpt, default)
        }
      }
  }

  def quasiquoteTermParam(): Term.Param = entrypointTermParam()

  def entrypointTermParam(): Term.Param = termParam(ownerIsCase = false, ownerIsType = true)(0)

  private def emptyTypeParams: Type.ParamClause = autoPos(Type.ParamClause(Nil))

  private def typeParamClauseOpt(
      ownerIsType: Boolean,
      allowUnderscore: Boolean = true
  ): Type.ParamClause =
    if (!isAfterOptNewLine[LeftBracket]) emptyTypeParams
    else typeParamClauseOnBracket(ownerIsType, allowUnderscore)

  private def typeParamClauseOnBracket(
      ownerIsType: Boolean,
      allowUnderscore: Boolean = true
  ): Type.ParamClause = TypeBracketsContext.within(autoPos(inBrackets(
    commaSeparated(typeParam(ownerIsType, allowUnderscore)).reduceWith(Type.ParamClause.apply)
  )))

  def typeParam(ownerIsType: Boolean, allowUnderscore: Boolean = true): Type.Param = autoPos {
    val mods: List[Mod] = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = false)
      if (ownerIsType) tparamModifiers(buf)
    }
    def endTparamQuasi = currToken.isAny[RightBracket, Comma]
    mods.headOption match {
      case Some(q: Mod.Quasi) if endTparamQuasi => q.become[Type.Param]
      case _ =>
        val name = currToken match {
          case t: Ident => typeName(t)
          case t: Unquote => unquote[Name](t)
          case _: Underscore if allowUnderscore => namePlaceholder()
          case _ =>
            if (allowUnderscore) syntaxError("identifier or `_' expected", at = currToken)
            else syntaxError("identifier expected", at = currToken)
        }
        name match {
          case q: Quasi if endTparamQuasi => q.become[Type.Param]
          case _ =>
            val tparams = typeParamClauseOpt(ownerIsType = true)
            val tbounds = typeBounds()
            val vbounds = new ListBuffer[Type]
            val cbounds = new ListBuffer[Type]
            @inline
            def getBound(allowAlias: Boolean): Type = currToken match {
              case t: Ellipsis => ellipsis[Type](t, 1)
              case _ => contextBoundOrAlias(allowAlias)
            }
            while (acceptOpt[Viewbound]) vbounds += getBound(allowAlias = false)
            if (acceptOpt[Colon]) {
              def addContextBounds[T: ClassTag]: Unit = doWhile(
                cbounds += getBound(allowAlias = dialect.allowImprovedTypeClassesSyntax)
              )(acceptOpt[T])
              if (acceptOpt[LeftBrace]) inBracesAfterOpen(addContextBounds[Comma])
              else addContextBounds[Colon]
            }

            Type.Param(mods, name, tparams, tbounds, vbounds.toList, cbounds.toList)
        }
    }
  }

  def contextBoundOrAlias(allowAlias: Boolean) = typ() match {
    case Type.ApplyInfix(nm: Type.Name, Type.Name("as"), alias: Type.Name)
        if allowAlias && dialect.allowImprovedTypeClassesSyntax => Type.BoundsAlias(alias, nm)
    case tpe => tpe
  }

  def quasiquoteTypeParam(): Type.Param = entrypointTypeParam()

  def entrypointTypeParam(): Type.Param = typeParam(ownerIsType = true)

  def typeBounds() = autoPos(Type.Bounds(bound[Supertype], bound[Subtype]))

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
        Importer(autoPos(Term.Anonymous()), importeeRename(name(tn)) :: Nil)
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
      case Wildcard() => next(); Importee.Wildcard()
      case _: KwGiven => next(); if (at[Ident]) Importee.Given(typ()) else Importee.GivenAll()
      case t: Unquote => Importee.Name(unquote[Name.Quasi](t))
      case t: Ident => next(); Importee.Name(atPos(startPos)(Name.Indeterminate(t.value)))
      case _ => syntaxErrorExpected[Ident]
    })
  }

  def importeeRename(from: Name) = autoEndPos(from) {
    importWildcardOrName() match {
      case to: Importee.Name => Importee.Rename(from, to.name)
      case _: Importee.Wildcard => Importee.Unimport(from)
      case other => unreachable(Map("importees" -> other, "importeesStructure" -> other.structure))
    }
  }

  def importee(): Importee = autoPos {
    importWildcardOrName() match {
      case from: Importee.Name if at[RightArrow] || soft.KwAs(currToken) =>
        next()
        importeeRename(from.name)
      // NOTE: this is completely nuts
      case from: Importee.Wildcard
          if (at[RightArrow] || soft.KwAs(currToken)) && nextIf(Wildcard.unapply(peekToken)) =>
        next()
        from
      case other => other
    }
  }

  def quasiquoteImportee(): Importee = entrypointImportee()

  def entrypointImportee(): Importee = importee()

  def nonLocalDefOrDcl(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): Stat = {
    val mods = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = true)
      modifiersBuf(buf)
    }
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
    def onlyInline() = mods match {
      case (_: Mod.Inline) :: Nil => true
      case _ => false
    }
    currToken match {
      case _: KwVal | _: KwVar => patDefOrDcl(mods)
      case _: KwGiven => givenDecl(mods)
      case t: KwDef =>
        if (!secondaryConstructorAllowed && peek[KwThis])
          syntaxError("Illegal secondary constructor", at = t)
        funDefOrDclOrExtensionOrSecondaryCtor(mods)
      case _: KwType => typeDefOrDcl(mods)
      case t: KwCase if dialect.allowEnums && peek[Ident] =>
        if (!enumCaseAllowed) syntaxError("Enum cases are only allowed in enums", at = t)
        mods.find(mod => !mod.isAccessMod && !mod.is[Mod.Annot]) match {
          case Some(mod) => syntaxError("Only access modifiers allowed on enum case", at = mod.pos)
          case None => enumCaseDef(mods)
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
      lhs.foreach { x =>
        if (!x.is[Quasi] && !x.is[Pat.Var])
          syntaxError("pattern definition may not be abstract", at = x)
      }
      tpOpt.fold(syntaxError("declaration requires a type", at = currToken)) { tp =>
        if (isVal) Decl.Val(mods, lhs, tp) else Decl.Var(mods, lhs, tp)
      }
    }
  }

  private def getParamClauseGroup(
      tparamClause: Type.ParamClause,
      paramClauses: List[Term.ParamClause]
  ): Option[Member.ParamClauseGroup] = {
    def pcGroup = Member.ParamClauseGroup(tparamClause, paramClauses)
    if (paramClauses.nonEmpty) Some(atPos(tparamClause, paramClauses.last)(pcGroup))
    else if (tparamClause.is[Quasi] || tparamClause.values.nonEmpty)
      Some(copyPos(tparamClause)(pcGroup))
    else None
  }

  /**
   * ```scala
   * Given             ::= 'given' GivenDef
   * GivenDef          ::=  [GivenSig] (Type [‘=’ Expr] | StructuralInstance)
   * GivenSig          ::=  [id] [DefTypeParamClause] {UsingParamClause} ‘:’
   * StructuralInstance ::=  ConstrApp {‘with’ ConstrApp} ‘with’ TemplateBody
   * ```
   */
  private def givenDecl(mods: List[Mod]): Stat = autoEndPos(mods) {
    accept[KwGiven]
    val (sigName, paramClauseGroup) = tryParse {
      Try {
        val name: meta.Name = currToken match {
          case t: Ident => termName(t)
          case _ => anonNameEmpty()
        }
        val tparams =
          typeParamClauseOpt(ownerIsType = false, allowUnderscore = dialect.allowTypeParamUnderscore)
        val uparamss =
          if (at[LeftParen] && soft.KwUsing(peekToken)) termParamClausesOnParen(ownerIsType = false)
          else Nil
        if (acceptOpt[Colon]) Some((name, getParamClauseGroup(tparams, uparamss))) else None
      }.getOrElse {

        /**
         * We are first trying to parse non-anonymous given, which
         *   - requires `:` for type, if none is found it means it's anonymous
         *   - might fail in cases that anonymous type looks like type param
         *     {{{given Conversion[AppliedName, Expr.Apply[Id]] = ???}}} This will fail because type
         *     params cannot have `.`
         */
        None
      }
    }.getOrElse((anonNameEmpty(), None))

    val decltype = refinement(None).getOrElse(startModType())

    def parents() = {
      val parents = ListBuffer[Init](
        autoEndPos(decltype)(initRest(decltype, allowArgss = true, allowTypeSingleton = false))
      )
      while (at[KwWith] && tryAhead[Ident]) parents += init()
      parents.toList
    }

    if (acceptOpt[Equals]) Defn.GivenAlias(mods, sigName, paramClauseGroup, decltype, expr())
    else if (currToken.isAny[KwWith, LeftParen]) {
      val inits = parents()
      val body =
        if (acceptOpt[KwWith])
          if (isAfterOptNewLine[LeftBrace]) templateBodyOnLeftBrace(OwnedByGiven)
          else if (at[Indentation.Indent]) autoPos(templateBodyOnIndentRaw(OwnedByGiven))
          else syntaxError("expected '{' or indentation", at = currToken)
        else {
          if (inits.lengthCompare(1) > 0) syntaxError("expected 'with' <body>", at = prevToken)
          emptyTemplateBody()
        }
      val rhs = autoEndPos(decltype)(Template(None, inits, body, Nil))
      Defn.Given(mods, sigName, paramClauseGroup, rhs)
    } else sigName match {
      case name: Term.Name => Decl.Given(mods, name, paramClauseGroup, decltype)
      case _ => syntaxError("abstract givens cannot be anonymous", at = sigName.pos)
    }
  }

  def funDefOrDclOrExtensionOrSecondaryCtor(mods: List[Mod]): Stat =
    if (peekToken.isNot[KwThis]) funDefRest(mods) else secondaryCtor(mods)

  // TmplDef           ::= ‘extension’ [DefTypeParamClause] ‘(’ DefParam ‘)’ {UsingParamClause} ExtMethods
  // ExtMethods        ::=  ExtMethod | [nl] ‘{’ ExtMethod {semi ExtMethod ‘}’
  // ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
  def extensionGroupDecl(mods: List[Mod]): Defn.ExtensionGroup = autoEndPos(mods) {
    next() // 'extension'

    val tparams =
      typeParamClauseOpt(ownerIsType = false, allowUnderscore = dialect.allowTypeParamUnderscore)

    newLineOptWhenFollowedBy[LeftParen]

    val paramss = ListBuffer[Term.ParamClause]()

    def collectUparams(): Unit =
      while (isAfterOptNewLine[LeftParen] && nextIf(soft.KwUsing(peekToken))) paramss += autoPrevPos {
        val mod = Some(atCurPos(Mod.Using()))
        inParensOnOpen(commaSeparatedWithIndex {
          termParam(ownerIsCase = false, ownerIsType = true, mod = mod)
        }).reduceWith(toParamClause(mod))
      }

    collectUparams()

    paramss += autoPos(
      inParens(List(currToken match {
        case t @ Ellipsis(2) => ellipsis[Term.Param](t)
        case _ => termParam(ownerIsCase = false, ownerIsType = false)(0)
      })).reduceWith(toParamClause(None))
    )

    collectUparams()
    newLinesOpt()

    def getStats() = statSeq(templateStat())
    val body: Stat = currToken match {
      case _: LeftBrace => blockOnBrace(getStats())
      case _: Indentation.Indent => autoPosOpt(indentedOnOpen(getStats() match {
          case t :: Nil if !isPrecededByDetachedComment(currIndex, t.endIndex) => t
          case stats => toBlockRaw(stats)
        }))
      case _ if isDefIntro(currIndex) => nonLocalDefOrDcl()
      case _ => syntaxError("Extension without extension method", currToken)
    }
    Defn.ExtensionGroup(getParamClauseGroup(tparams, paramss.toList), body)
  }

  def funDefRest(mods: List[Mod]): Stat = autoEndPos(mods) {
    accept[KwDef]
    val name = termName()

    def procedureSyntaxDeclType: Type = {
      val hint = s"Convert procedure `$name` to method by adding `: Unit =`."
      if (dialect.allowProcedureSyntax)
        deprecationWarning(s"Procedure syntax is deprecated. $hint", at = name)
      else syntaxError(s"Procedure syntax is not supported. $hint", at = name)
      autoPos(Type.Name("Unit"))
    }

    def nonInterleavedParamClauses = {
      val tparams = typeParamClauseOpt(ownerIsType = false)
      val paramss = termParamClauses(ownerIsType = false)
      getParamClauseGroup(tparams, paramss)
    }
    val paramClauses: List[Member.ParamClauseGroup] =
      if (dialect.allowParamClauseInterleaving) memberParamClauseGroups(ownerIsType = false)
      else nonInterleavedParamClauses.toList

    def defn(declType: Option[Type]) = Defn.Def(mods, name, paramClauses, declType, expr())

    val restype = ReturnTypeContext.within(typedOpt())
    if (acceptOpt[Equals])
      if (!acceptOpt[KwMacro]) defn(restype)
      else Defn.Macro(mods, name, paramClauses, restype, expr())
    else if (StatSeqEnd(currToken) || StatSep(currToken)) Decl
      .Def(mods, name, paramClauses, restype.getOrElse(procedureSyntaxDeclType))
    else if (restype.isEmpty && isAfterOptNewLine[LeftBrace]) defn(Some(procedureSyntaxDeclType))
    else syntaxErrorExpected[Equals]
  }

  def typeDefOrDcl(mods: List[Mod]): Stat.TypeDef = autoEndPos(mods) {
    accept[KwType]
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt(ownerIsType = true)

    def aliasType() = {
      // empty bounds also need to have origin
      val emptyBounds = autoPos(Type.Bounds(None, None))
      Defn.Type(mods, name, tparams, typeIndentedOpt(), emptyBounds)
    }

    def abstractType() = {
      val bounds = typeBounds()
      if (acceptOpt[Equals]) {
        val tpe = typeIndentedOpt()
        if (tpe.is[Type.Match]) Defn.Type(mods, name, tparams, tpe, bounds)
        else syntaxError("cannot combine bound and alias", at = tpe.pos)
      } else Decl.Type(mods, name, tparams, bounds)
    }
    if (mods.exists(_.is[Mod.Opaque])) {
      val bounds = typeBounds()
      accept[Equals]
      Defn.Type(mods, name, tparams, typeIndentedOpt(), bounds)
    } else currToken match {
      case _: Equals => next(); aliasType()
      case _: Supertype | _: Subtype | _: Comma | StatSep() | StatSeqEnd() => abstractType()
      case _ => syntaxError("`=', `>:', or `<:' expected", at = currToken)
    }
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
    Defn.Trait(
      mods,
      traitName,
      typeParamClauseOpt(ownerIsType = true, allowUnderscore = dialect.allowTypeParamUnderscore),
      primaryCtor(OwnedByTrait),
      templateOpt(OwnedByTrait)
    )
  }

  def classDef(mods: List[Mod]): Defn.Class = {
    next()

    val className = typeName()
    def culprit = Some(s"class $className")
    val typeParams =
      typeParamClauseOpt(ownerIsType = true, allowUnderscore = dialect.allowTypeParamUnderscore)
    val ctor = primaryCtor(if (mods.has[Mod.Case]) OwnedByCaseClass else OwnedByClass)

    if (!dialect.allowCaseClassWithoutParameterList && mods.has[Mod.Case] &&
      ctor.paramClauses.isEmpty) syntaxError(
      s"case classes must have a parameter list; try 'case class $className()' or 'case object $className'",
      at = currToken
    )

    val tmpl = templateOpt(OwnedByClass)
    Defn.Class(mods, className, typeParams, ctor, tmpl)
  }

  // EnumDef           ::=  id ClassConstr InheritClauses EnumBody
  private def enumDef(mods: List[Mod]): Defn.Enum = {
    next()
    val enumName = typeName()
    val typeParams =
      typeParamClauseOpt(ownerIsType = true, allowUnderscore = dialect.allowTypeParamUnderscore)
    val ctor = primaryCtor(OwnedByEnum)
    val tmpl = templateOpt(OwnedByEnum)
    Defn.Enum(mods, enumName, typeParams, ctor, tmpl)
  }

  // EnumCase          ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps]] | ids)
  // ids               ::=  id {‘,’ id}
  // ClassConstr       ::=  [ClsTypeParamClause] [ConstrMods] ClsParamClauses
  def enumCaseDef(mods: List[Mod]): Stat = autoEndPos(mods) {
    accept[KwCase]
    if (at[Ident] && peek[Comma]) enumRepeatedCaseDef(mods) else enumSingleCaseDef(mods)
  }

  def enumRepeatedCaseDef(mods: List[Mod]): Defn.RepeatedEnumCase = {
    val values = commaSeparated(termName())
    Defn.RepeatedEnumCase(mods, values)
  }

  def enumSingleCaseDef(mods: List[Mod]): Defn.EnumCase = {
    val name = termName()
    val tparams =
      typeParamClauseOpt(ownerIsType = true, allowUnderscore = dialect.allowTypeParamUnderscore)
    val ctor = primaryCtor(OwnedByEnum)
    val inits = if (acceptOpt[KwExtends]) templateParents(afterExtend = true) else List()
    Defn.EnumCase(mods, name, tparams, ctor, inits)
  }

  def objectDef(mods: List[Mod]): Defn.Object = {
    next()
    val objectName = termName()
    val culprit = s"object $objectName"
    Defn.Object(mods, objectName, templateOpt(OwnedByObject))
  }

  /* -------- CONSTRUCTORS ------------------------------------------- */

  def primaryCtor(owner: TemplateOwner): Ctor.Primary = autoPos {
    if (owner.isPrimaryCtorAllowed) {
      val mods = listBy[Mod] { buf =>
        annotsBuf(buf, skipNewLines = false, allowArgss = false, insidePrimaryCtorAnnot = true)
        ctorModifiers(buf)
      }
      val name = anonNameEmpty()
      val paramss = termParamClauses(ownerIsType = true, owner == OwnedByCaseClass)
      Ctor.Primary(mods, name, paramss)
    } else Ctor.Primary(Nil, anonNameEmpty(), Seq.empty[Term.ParamClause])
  }

  def secondaryCtor(mods: List[Mod]): Ctor.Secondary = autoEndPos(mods) {
    accept[KwDef]
    expect[KwThis]
    val name = nameThis()
    if (currToken.isNot[LeftParen])
      syntaxError("auxiliary constructor needs non-implicit parameter list", at = currToken.pos)
    else {
      val paramss = termParamClauses(ownerIsType = true)
      secondaryCtorRest(mods, name, paramss)
    }
  }

  def quasiquoteCtor(): Ctor = autoPos {
    val mods = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = true)
      modifiersBuf(buf)
    }
    accept[KwDef]
    val beforeThisPos = prevIndex
    val thisPos = currIndex
    accept[KwThis]
    val paramss = termParamClauses(ownerIsType = true)
    newLineOptWhenFollowedBy[LeftBrace]
    if (at[EOF]) Ctor.Primary(mods, atPos(thisPos, beforeThisPos)(Name.Anonymous()), paramss)
    else secondaryCtorRest(mods, atPos(thisPos)(Name.This()), paramss)
  }

  def entrypointCtor(): Ctor = ???

  def secondaryCtorRest(
      mods: List[Mod],
      name: Name.This,
      paramss: Seq[Term.ParamClause]
  ): Ctor.Secondary = {
    val hasLeftBrace = isAfterOptNewLine[LeftBrace] || { accept[Equals]; at[LeftBrace] }
    val body = autoPos {
      if (hasLeftBrace) inBracesOnOpen(constrInternal())
      else if (acceptOpt[Indentation.Indent]) indentedAfterOpen(constrInternal())
      else Ctor.Block(initInsideConstructor(), Nil)
    }
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

  def initInsideTemplate(): Init =
    initRest(startModType(), allowArgss = true, allowTypeSingleton = false)

  def quasiquoteInit(): Init = entrypointInit()

  def entrypointInit(): Init = currToken match {
    case _: KwThis => initInsideConstructor()
    case _ => initInsideTemplate()
  }

  def initRest(
      typeParser: => Type,
      allowArgss: Boolean,
      insidePrimaryCtorAnnot: Boolean = false,
      allowBraces: Boolean = false,
      allowTypeSingleton: Boolean = true
  ): Init = autoPosOpt {
    def isPendingArglist(t: Token) = t.is[LeftParen] || (t.is[LeftBrace] && allowBraces)
    def newlineOpt() =
      if (dialect.allowSignificantIndentation)
        newLineOptWhenFollowedBySignificantIndentationAnd(_.isAny[LeftBrace, LeftParen])
      else if (allowBraces) newLineOptWhenFollowedBy[LeftBrace]
    currToken match {
      case t: Unquote if !isPendingArglist(peekToken) => unquote[Init](t)
      case _ =>
        val tpe = typeParser
        if (!allowTypeSingleton && tpe.is[Type.Singleton])
          syntaxError(s"class type required but $tpe found", at = tpe.pos)
        val name = anonNameEmpty()
        val argss = listBy[Term.ArgClause] { argss =>
          def isLegalAnnotArg(): Boolean = peekToken match {
            // explained here:
            // https://github.com/lampepfl/dotty/blob/675ae0c6440d5527150d650ad45d20fda5e03e69/compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L2581
            case _: RightParen => argss.isEmpty
            case _: Ident => !ahead(peek[Colon])
            case _: At => false
            case _ => !isModifier(peekIndex)
          }
          def maybeBody() = {
            newlineOpt()
            currToken match {
              case _: LeftParen if !insidePrimaryCtorAnnot || isLegalAnnotArg() =>
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
        Init(tpe, name, argss)
    }
  }

  /* ---------- SELFS --------------------------------------------- */

  def quasiquoteSelf(): Self = self(quasiquote = true)

  def entrypointSelf(): Self = self(quasiquote = false)

  private def self(quasiquote: Boolean): Self = selfEither(quasiquote)
    .fold(syntaxError(_, currToken), identity)

  private def selfEitherImpl(): Either[String, Self] = {
    val name = currToken match {
      case t: Ident => termName(t)
      case _: KwThis => nameThis()
      case _: Underscore => namePlaceholder()
      case t: Unquote =>
        if (peek[Colon]) unquote[Name.Quasi](t) else return Right(unquote[Self.Quasi](t))
      case _ => return Left("expected identifier, `this' or unquote")
    }
    // possible fewer braces after colon
    if (!peekToken.isAny[Indentation, EOL]) Right(Self(name, getDeclTpeOpt(fullTypeOK = false)))
    else if (!at[Colon]) Right(Self(name, None))
    else Left("missing type after self")
  }

  private def selfEither(quasiquote: Boolean = false): Either[String, Self] = {
    val startPos = currIndex
    selfEitherImpl().right.map { self =>
      currToken match {
        case _: RightArrow => next()
        case _: EOF if quasiquote =>
        case _ => return Left("expected `=>`")
      }
      autoEndPos(startPos)(self)
    }
  }

  /* -------- TEMPLATES ------------------------------------------- */

  sealed trait TemplateOwner {
    def isEnumCaseAllowed: Boolean
    def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean
    def isSecondaryCtorAllowed: Boolean
  }
  object OwnedByTrait extends TemplateOwner {
    override final def isEnumCaseAllowed: Boolean = false
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean =
      dialect.allowTraitParameters
    override final def isSecondaryCtorAllowed: Boolean = false
  }
  object OwnedByCaseClass extends TemplateOwner {
    override final def isEnumCaseAllowed: Boolean = false
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = true
    override final def isSecondaryCtorAllowed: Boolean = true
  }
  object OwnedByClass extends TemplateOwner {
    override final def isEnumCaseAllowed: Boolean = false
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = true
    override final def isSecondaryCtorAllowed: Boolean = true
  }
  object OwnedByEnum extends TemplateOwner {
    override final def isEnumCaseAllowed: Boolean = true
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = true
    override final def isSecondaryCtorAllowed: Boolean = true
  }
  object OwnedByObject extends TemplateOwner {
    override final def isEnumCaseAllowed: Boolean = false
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = false
    override final def isSecondaryCtorAllowed: Boolean = false
  }
  object OwnedByGiven extends TemplateOwner {
    override final def isEnumCaseAllowed: Boolean = false
    override final def isPrimaryCtorAllowed(implicit dialect: Dialect): Boolean = false
    override final def isSecondaryCtorAllowed: Boolean = false
  }

  def init() = currToken match {
    case t: Ellipsis => ellipsis[Init](t, 1)
    case _ => initInsideTemplate()
  }

  def templateParents(afterExtend: Boolean = false): List[Init] = {
    val isSeparator: Token => Boolean =
      if (afterExtend && dialect.allowCommaSeparatedExtend) _.isAny[KwWith, Comma] else _.is[KwWith]
    listBy[Init](x => doWhile(x += init())(nextIf(isSeparator(currToken))))
  }

  def derivesClasses(): List[Type] =
    if (isAfterOptNewLine(soft.KwDerives(_))) {
      next()
      newLineOpt()
      val deriving = ListBuffer[Type]()
      doWhile {
        currToken match {
          case t: Ellipsis => deriving += ellipsis[Type](t, 1)
          case _ => deriving += startModType()
        }
      }(acceptOpt[Comma])
      deriving.toList
    } else Nil

  private def templateAfterExtends(
      owner: TemplateOwner,
      parents: List[Init] = Nil,
      edefs: Option[Stat.Block] = None
  ): Template = {
    val derived = derivesClasses()
    val body = templateBodyOpt(owner)
    Template(edefs, parents, body, derived)
  }

  def template(owner: TemplateOwner, afterExtend: Boolean = false): Template = autoPos {
    if (isAfterOptNewLine[LeftBrace]) {
      // @S: pre template body cannot stub like post body can!
      val body = templateBodyOnLeftBrace(owner)
      if (at[KwWith] && body.selfOpt.isEmpty) {
        val edefs = body.stats
        edefs.foreach(_ match {
          case _: Quasi | _: Defn.Val | _: Defn.Var | _: Defn.Type =>
          case other => syntaxError("not a valid early definition", at = other)
        })
        next()
        val parents = templateParents(afterExtend)
        val early = copyPos(body)(toStatsBlockRaw(edefs))
        templateAfterExtends(owner, parents, Some(early))
      } else Template(None, Nil, body, Nil)
    } else {
      val parents = if (at[Colon]) Nil else templateParents(afterExtend)
      templateAfterExtends(owner, parents)
    }
  }

  def quasiquoteTemplate(): Template = entrypointTemplate()

  def entrypointTemplate(): Template = autoPos(template(OwnedByClass))

  def templateOpt(owner: TemplateOwner): Template = autoPos {
    if (at[Unquote] &&
      (peekToken match {
        case _: Dot | _: Hash | _: At | _: Ellipsis | _: LeftParen | _: LeftBracket | _: LeftBrace |
            _: KwWith => false
        case _ => true
      })) unquote[Template](currToken.asInstanceOf[Unquote])
    else if (acceptOpt[KwExtends] || (owner eq OwnedByTrait) && acceptOpt[Subtype])
      template(owner, afterExtend = true)
    else templateAfterExtends(owner)
  }

  @inline
  private def emptyTemplateBody(): Template.Body = autoPos(Template.Body(None, Nil))

  @inline
  private def templateBodyOnLeftBrace(owner: TemplateOwner): Template.Body =
    autoPos(inBracesOnOpen(templateStatSeq(owner)))

  @inline
  private def templateBodyOnIndentRaw(owner: TemplateOwner): Template.Body =
    indentedOnOpen(templateStatSeq(owner))

  def templateBodyOpt(owner: TemplateOwner): Template.Body =
    if (isAfterOptNewLine[LeftBrace]) templateBodyOnLeftBrace(owner)
    else if (at[Colon] && peekToken.isAny[Indentation, EOL]) autoPos {
      next()
      expect[Indentation.Indent]("expected template body")
      templateBodyOnIndentRaw(owner)
    }
    else if (at[LeftParen])
      if (owner.isPrimaryCtorAllowed) syntaxError("unexpected opening parenthesis", at = currToken)
      else {
        val what = owner.getClass.getSimpleName.stripPrefix("OwnedBy")
        syntaxError(s"$what may not have parameters", at = currToken)
      }
    else emptyTemplateBody()

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
      stats.foreach { x =>
        if (!x.isExistentialStat) syntaxError("not a legal existential clause", at = x)
      }
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
    case _ if isDefIntro(currIndex) => nonLocalDefOrDcl(secondaryConstructorAllowed = true)
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
      if (statpfAdd(currToken)) acceptStatSepOpt()
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

  private def templateStatSeq(owner: TemplateOwner): Template.Body = {
    val enumCaseAllowed = owner.isEnumCaseAllowed
    val secondaryConstructorAllowed = owner.isSecondaryCtorAllowed
    val selfTreeOpt = tryParse(selfEither().right.toOption)
    if (owner eq OwnedByGiven) selfTreeOpt.foreach(_.decltpe.foreach { x =>
      syntaxError("given cannot have a self type", at = x)
    })
    val stats = listBy[Stat] { buf =>
      def getStats() = statSeqBuf(buf, templateStat(enumCaseAllowed, secondaryConstructorAllowed))
      val wasIndented = getStats() // some stats could be indented relative to self-type
      if (wasIndented && selfTreeOpt.isDefined) getStats() // and the rest might not be
    }
    Template.Body(selfTreeOpt, stats)
  }

  def templateStat(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): PartialFunction[Token, Stat] = {
    case _: KwImport => importStmt()
    case _: KwExport => exportStmt()
    case _ if isDefIntro(currIndex) => nonLocalDefOrDcl(enumCaseAllowed, secondaryConstructorAllowed)
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
      acceptStatSepOpt()
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
    def notCaseDefEnd(): Boolean = currToken match {
      case _: RightParen | StatSeqEnd() => false
      case _: KwCase => !isCaseIntroOnKwCase()
      case _: Ellipsis => !peek[KwCase]
      case _ => true
    }
    def cond(): Boolean = {
      skipAllStatSep()
      notCaseDefEnd()
    }
    def getStat(): Stat = currToken match {
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
      case _ => syntaxError("illegal start of statement", at = currToken)
    }

    if (cond()) doWhile(stats += getStat())(notCaseDefEnd() && { acceptStatSep(); cond() })
    if (allowRepeated && stats.length > 1) stats.foreach {
      case t: Term.Repeated => syntaxError("repeated argument not allowed here", at = t)
      case _ =>
    }
  }

  private def toPkgBody(startPos: Int)(stats: List[Stat]): Pkg.Body =
    autoEndPos(startPos)(stats.reduceWith(Pkg.Body.apply))

  private def packageOrPackageObjectDef(statpf: PartialFunction[Token, Stat]): Stat = autoPos {
    next()
    if (acceptOpt[KwObject]) Pkg.Object(Nil, termName(), templateOpt(OwnedByObject))
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
            acceptStatSep()
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

  }

  private def copyPos[T <: Tree](tree: Tree)(body: => T): T = body.withOrigin(tree.origin)

  @inline
  private def quasi[T <: Tree](rank: Int, tree: Tree)(implicit astInfo: AstInfo[T]): T with Quasi =
    astInfo.quasi(rank, tree)

  private def reellipsis[T <: Tree: AstInfo](q: Quasi, rank: Int): T = {
    val became = q.become[T]
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

  private val bigIntMaxInt = BigInt(Int.MaxValue) + 1
  private val bigIntMaxUInt = bigIntMaxInt << 1

  private val bigIntMaxLong = BigInt(Long.MaxValue) + 1
  private val bigIntMaxULong = bigIntMaxLong << 1

  private def getTokenName[T <: Token: ClassTag]: String =
    classTag[T].runtimeClass.getSimpleName match {
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
      case "MacroSplice" => "$"
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
      case other => other.toLowerCase().stripPrefix("kw")
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
