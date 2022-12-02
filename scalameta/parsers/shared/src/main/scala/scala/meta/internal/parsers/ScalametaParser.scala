package scala.meta
package internal
package parsers

import scala.language.implicitConversions
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
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal

class ScalametaParser(input: Input)(implicit dialect: Dialect) { parser =>

  import ScalametaParser._

  private val scannerTokens: ScannerTokens = ScannerTokens(input)
  import scannerTokens.Implicits._
  import scannerTokens.Classifiers._

  /* ------------- PARSER ENTRY POINTS -------------------------------------------- */

  def parseRule[T <: Tree](rule: this.type => T): T = {
    parseRule(rule(this))
  }

  def parseRule[T <: Tree](rule: => T): T = {
    // NOTE: can't require in.tokenPos to be at -1, because TokIterator auto-rewinds when created
    // require(in.tokenPos == -1 && debug(in.tokenPos))
    accept[BOF]
    parseRuleAfterBOF(rule)
  }

  private def parseRuleAfterBOF[T <: Tree](rule: => T): T = {
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

  def parseSource(): Source = parseRule(parseSourceImpl())

  private def parseSourceImpl(): Source = {
    if (dialect.allowUnquotes) quasiquoteSource() else entrypointSource()
  }

  def parseAmmonite(): MultiSource = parseRule(entryPointAmmonite())

  def entryPointAmmonite(): MultiSource = {
    require(input.isInstanceOf[Input.Ammonite])
    val builder = List.newBuilder[Source]
    do {
      builder += parseRuleAfterBOF(parseSourceImpl())
    } while (in.token match {
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

  def isColonEol(token: Token): Boolean = {
    token.is[Colon] && isEolAfterColon(token)
  }

  def isEolAfterColon(token: Token): Boolean = {

    @tailrec
    def isNextEOL(t: Token): Boolean = {
      t.nextSafe match {
        case _: AtEOL | MultilineComment() => true
        case x: Trivia => isNextEOL(x)
        case _ => false
      }
    }

    dialect.allowSignificantIndentation && isNextEOL(token)
  }

  /* ------------- PARSER-SPECIFIC TOKENS -------------------------------------------- */

  var in: TokenIterator = {
    LazyTokenIterator(scannerTokens)
  }

  private def currentIndentation: Int = {
    in.currentIndentation
  }

  private def previousIndentation: Int = {
    in.previousIndentation
  }

  def token = in.token
  def next() = in.next()
  def nextTwice() = { next(); next() }

  @inline private def nextIf(cond: Boolean): Boolean = {
    if (cond) next()
    cond
  }

  /* ------------- PARSER COMMON -------------------------------------------- */

  /**
   * Scoping operator used to temporarily look into the future. Backs up token iterator before
   * evaluating a block and restores it after.
   */
  @inline final def ahead[T](body: => T): T = {
    val forked = in.fork
    try next(body)
    finally in = forked
  }

  @inline private def tryAhead[T: TokenClassifier]: Boolean =
    tryAhead(token.is[T])

  private def tryAhead(cond: => Boolean): Boolean = {
    val forked = in.fork
    next()
    val ok = cond
    if (!ok) in = forked
    ok
  }

  private def tryAhead[A](bodyFunc: => Option[A]): Option[A] = {
    val forked = in.fork
    next()
    val body = bodyFunc
    if (body.isEmpty) in = forked
    body
  }

  /** evaluate block after shifting next */
  @inline private def next[T](body: => T): T = {
    next()
    body
  }

  @inline final def inParens[T](body: => T): T = {
    accept[LeftParen]
    inParensAfterOpen(body)
  }
  @inline final def inParensOr[T](body: => T)(ifEmpty: => T): T = {
    accept[LeftParen]
    inParensAfterOpenOr(body)(ifEmpty)
  }
  @inline private def inParensOnOpen[T](body: => T): T = {
    next()
    inParensAfterOpen(body)
  }
  @inline private def inParensOnOpenOr[T](body: => T)(ifEmpty: => T): T = {
    next()
    inParensAfterOpenOr(body)(ifEmpty)
  }
  @inline private def inParensAfterOpen[T](body: T): T = {
    newLineOpt()
    accept[RightParen]
    body
  }
  @inline private def inParensAfterOpenOr[T](body: => T)(ifEmpty: => T): T =
    if (acceptOpt[RightParen]) ifEmpty
    else inParensAfterOpen(body)

  @inline final def inBraces[T](body: => T): T = {
    inBracesOr(body)(syntaxErrorExpected[LeftBrace])
  }
  @inline final def inBracesOr[T](body: => T)(ifEmpty: => T): T = {
    newLineOpt()
    if (acceptOpt[LeftBrace]) inBracesAfterOpen(body) else ifEmpty
  }
  @inline private def inBracesOnOpen[T](body: => T): T = {
    next()
    inBracesAfterOpen(body)
  }
  @inline private def inBracesAfterOpen[T](body: T): T = {
    newLineOpt()
    accept[RightBrace]
    body
  }

  @inline final def indented[T](body: => T): T = {
    accept[Indentation.Indent]
    indentedAfterOpen(body)
  }
  @inline private def indentedOnOpen[T](body: => T): T = {
    next()
    indentedAfterOpen(body)
  }
  @inline private def indentedAfterOpen[T](body: T): T = {
    newLinesOpt()
    if (!acceptOpt[Indentation.Outdent]) {
      in.observeOutdented()
      accept[Indentation.Outdent]
    }
    body
  }

  @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOr(body)(Nil)
  @inline final def dropAnyBraces[T](body: => T): T =
    inBracesOr(body)(body)

  @inline final def inBrackets[T](body: => T): T = {
    accept[LeftBracket]
    inBracketsAfterOpen(body)
  }
  @inline private def inBracketsAfterOpen[T](body: T): T = {
    accept[RightBracket]
    body
  }

  /* ------------- POSITION HANDLING ------------------------------------------- */

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
  implicit def intToIndexPos(index: Int): Pos = new IndexPos(index)
  implicit def tokenToTokenPos(token: Token): Pos = new IndexPos(token.index)
  implicit def treeToTreePos(tree: Tree): Pos = new TreePos(tree)
  implicit def optionTreeToPos(tree: Option[Tree]): Pos = tree.fold[Pos](AutoPos)(treeToTreePos)
  implicit def modsToPos(mods: List[Mod]): Pos = mods.headOption
  def auto = AutoPos

  def atPos[T <: Tree](token: Token)(body: => T): T = {
    atPos(token.index)(body)
  }
  def atPos[T <: Tree](start: StartPos, end: EndPos)(body: => T): T = {
    atPos(start.startTokenPos, end)(body)
  }
  def atPosOpt[T <: Tree](start: StartPos, end: EndPos)(body: => T): T = {
    atPosOpt(start.startTokenPos, end)(body)
  }

  @inline
  def atPos[T <: Tree](start: Int, end: EndPos)(body: T): T = {
    atPosWithBody(start, body, end.endTokenPos)
  }
  def atPosOpt[T <: Tree](start: Int, end: EndPos)(body: T): T = {
    if (body.origin ne Origin.None) body
    else atPos(start, end)(body)
  }
  def atPos[T <: Tree](pos: Int)(body: => T): T = {
    atPosWithBody(pos, body, pos)
  }
  def atCurPos[T <: Tree](body: => T): T = {
    atPos(in.tokenPos)(body)
  }
  def atCurPosNext[T <: Tree](body: => T): T = {
    try atCurPos(body)
    finally next()
  }

  def atPosWithBody[T <: Tree](startTokenPos: Int, body: T, endPos: Int): T = {
    val endTokenPos = if (endPos < startTokenPos) startTokenPos - 1 else endPos

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
    body.map(atPos(startTokenPos, end))
  }
  def atPosTryOpt[T <: Tree](start: StartPos, end: EndPos)(body: => Try[T]): Try[T] = {
    val startTokenPos = start.startTokenPos
    body.map(atPosOpt(startTokenPos, end))
  }

  def autoPos[T <: Tree](body: => T): T = atPos(start = auto, end = auto)(body)
  def autoPosOpt[T <: Tree](body: => T): T = atPosOpt(start = auto, end = auto)(body)
  @inline
  def autoEndPos[T <: Tree](start: Int)(body: => T): T = atPos(start = start, end = auto)(body)
  @inline
  def autoEndPosOpt[T <: Tree](start: Int)(body: => T): T =
    atPosOpt(start = start, end = auto)(body)
  @inline
  def autoEndPos[T <: Tree](start: StartPos)(body: => T): T = autoEndPos(start.startTokenPos)(body)
  @inline
  def autoPrevPos[T <: Tree](body: => T) = autoEndPos(in.prevTokenPos)(body)

  def autoPosTry[T <: Tree](body: => Try[T]): Try[T] =
    atPosTry(start = auto, end = auto)(body)

  /* ------------- ERROR HANDLING ------------------------------------------- */

  lazy val reporter = Reporter()
  import reporter._

  def syntaxErrorExpected[T <: Token](implicit tokenInfo: TokenInfo[T]): Nothing =
    syntaxError(s"${tokenInfo.name} expected but ${token.name} found", at = token)

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept[T <: Token: TokenInfo]: Unit =
    if (!token.is[T]) syntaxErrorExpected[T]
    else if (token.isNot[EOF]) next()

  /** If current token is T consume it. */
  @inline private def acceptOpt[T: TokenClassifier]: Boolean =
    nextIf(token.is[T])

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
      mods: collection.Iterable[Mod],
      errorMsg: String
  )(implicit classifier: Classifier[Mod, M], tag: ClassTag[M]) = {
    mods.first[M].foreach(m => syntaxError(errorMsg, at = m))
  }

  def rejectModCombination[M1 <: Mod: ClassTag, M2 <: Mod: ClassTag](
      mods: collection.Iterable[Mod],
      culpritOpt: => Option[String] = None
  )(implicit classifier1: Classifier[Mod, M1], classifier2: Classifier[Mod, M2]) =
    mods.first[M2].foreach(rejectModWith[M1](_, mods, culpritOpt))

  def rejectModWith[M <: Mod: ClassTag](
      m2: Mod,
      mods: collection.Iterable[Mod],
      culpritOpt: => Option[String] = None
  )(implicit classifier: Classifier[Mod, M]) = mods.first[M].foreach { m =>
    val errorMsg = Messages.IllegalCombinationModifiers(m, m2)
    val forCulprit = culpritOpt.fold("")(formatCulprit)
    val enrichedErrorMsg = errorMsg + forCulprit
    syntaxError(enrichedErrorMsg, at = m)
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

  def onlyAcceptMod[M <: Mod: ClassTag, T: TokenClassifier](
      mods: List[Mod],
      errorMsg: String
  )(implicit classifier: Classifier[Mod, M]) = {
    if (token.isNot[T]) {
      mods.first[M].foreach(m => syntaxError(errorMsg, at = m))
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
    case Ident(x) => pred(x)
    case _ => false
  }

  def followedByToken[T: TokenClassifier]: Boolean = {
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
  def isBackquoted: Boolean = {
    val syntax = token.syntax
    syntax.startsWith("`") && syntax.endsWith("`")
  }
  def isVarargStarParam() =
    dialect.allowPostfixStarVarargSplices && isStar && token.next.is[RightParen]

  private trait MacroIdent {
    protected def ident(token: Token): Option[String]
    final def unapply(token: Token): Option[String] =
      if (dialect.allowSpliceAndQuote && QuotedSpliceContext.isInside()) ident(token) else None
  }

  private object MacroSplicedIdent extends MacroIdent {
    protected def ident(token: Token): Option[String] = token match {
      case Ident(value) if value.length > 1 && value.charAt(0) == '$' => Some(value.substring(1))
      case _ => None
    }
  }

  private object MacroQuotedIdent extends MacroIdent {
    protected def ident(token: Token): Option[String] = token match {
      case Constant.Symbol(value) => Some(value.name)
      case _ => None
    }
  }

  /* ---------- TREE CONSTRUCTION ------------------------------------------- */

  private def listBy[T](f: ListBuffer[T] => Unit): List[T] = {
    val buf = new ListBuffer[T]
    f(buf)
    buf.toList
  }

  def ellipsis[T <: Tree: AstInfo](ell: Ellipsis, rank: Int, extraSkip: => Unit = {}): T = {
    if (ell.rank != rank) {
      syntaxError(Messages.QuasiquoteRankMismatch(ell.rank, rank), at = ell)
    }
    ellipsis(ell, extraSkip)
  }

  def ellipsis[T <: Tree: AstInfo](ell: Ellipsis): T = ellipsis(ell, {})

  def ellipsis[T <: Tree: AstInfo](ell: Ellipsis, extraSkip: => Unit): T = autoPos {
    if (!dialect.allowUnquotes) {
      syntaxError(s"$dialect doesn't support ellipses", at = ell)
    }
    next()
    extraSkip
    // unquote returns a rank=0 quasi tree
    val tree = token match {
      case LeftParen() => inParensOnOpen(unquote[T])
      case LeftBrace() => inBracesOnOpen(unquote[T])
      case t: Unquote => unquote[T](t)
      case t => syntaxError(s"$$, ( or { expected but ${t.name} found", at = t)
    }
    // NOTE: In the case of an unquote nested directly under ellipsis, we get a bit of a mixup.
    // Unquote's pt may not be directly equal unwrapped ellipsis's pt, but be its refinement instead.
    // For example, in `new { ..$stats }`, ellipsis's pt is List[Stat], but quasi's pt is Term.
    // This is an artifact of the current implementation, so we just need to keep it mind and work around it.
    require(classTag[T].runtimeClass.isAssignableFrom(tree.pt) && debug(ell, tree, tree.structure))
    quasi[T](ell.rank, tree)
  }

  private def unquote[T <: Tree: AstInfo](unquote: Unquote): T with Quasi = {
    require(unquote.input.chars(unquote.start + 1) != '$')
    if (!dialect.allowUnquotes) {
      syntaxError(s"$dialect doesn't support unquotes", at = unquote)
    }
    // NOTE: I considered having Input.Slice produce absolute positions from the get-go,
    // but then such positions wouldn't be usable with Input.Slice.chars.
    val unquotedTree = atPos(unquote) {
      try {
        val unquoteInput = Input.Slice(input, unquote.start + 1, unquote.end)
        val unquoteParser = new ScalametaParser(unquoteInput)(dialect.unquoteVariant())
        if (dialect.allowTermUnquotes) unquoteParser.parseUnquoteTerm()
        else if (dialect.allowPatUnquotes) unquoteParser.parseUnquotePat()
        else unreachable
      } catch {
        case ex: Exception => throw ex.absolutize
      }
    }
    next()
    copyPos(unquotedTree)(quasi[T](0, unquotedTree))
  }

  def unquote[T <: Tree: AstInfo]: T with Quasi =
    token match {
      case t: Unquote => unquote[T](t)
      case _ => unreachable(debug(token))
    }

  final def tokenSeparated[Sep: TokenClassifier, T <: Tree: AstInfo](
      sepFirst: Boolean,
      part: => T
  ): List[T] = listBy[T] { ts =>
    @tailrec
    def iter(sep: Boolean): Unit = token match {
      case t: Ellipsis =>
        ts += ellipsis[T](t, 1)
        iter(false)
      case _ if sep =>
        ts += part
        iter(false)
      case _ if acceptOpt[Sep] =>
        iter(true)
      case _ =>
    }
    iter(!sepFirst)
  }

  @inline final def commaSeparated[T <: Tree: AstInfo](part: => T): List[T] =
    tokenSeparated[Comma, T](sepFirst = false, part)

  private def makeTuple[A <: Tree](lpPos: Int, body: List[A], zero: => A, tuple: List[A] => A)(
      single: A => Either[List[A], A]
  ): A = body match {
    case Nil => autoEndPos(lpPos)(zero)
    case (q: Quasi) :: Nil if q.rank == 1 => copyPos(q)(tuple(body))
    case t :: Nil =>
      single(t) match {
        case Right(x) => x
        case Left(x) => autoEndPos(lpPos)(tuple(x))
      }
    case _ => autoEndPos(lpPos)(tuple(body))
  }

  private def makeTupleTerm(
      single: Term => Either[List[Term], Term]
  )(lpPos: Int, body: List[Term]): Term = {
    makeTuple(lpPos, body, Lit.Unit(), Term.Tuple.apply)(single)
  }

  private def makeTupleTerm(lpPos: Int, body: List[Term]): Term = {
    makeTupleTerm(Right(_))(lpPos, body)
  }

  private def makeTupleType(lpPos: Int, body: List[Type], zero: => Type, wrap: Boolean): Type = {
    makeTuple(lpPos, body, zero, Type.Tuple.apply) {
      maybeAnonymousLambda(_) match {
        case t: Type.Tuple if wrap && t.args.lengthCompare(1) > 0 => Left(t :: Nil)
        case t => Right(t)
      }
    }
  }

  private def makeTupleType(lpPos: Int, body: List[Type]): Type = {
    def invalidLiteralUnitType =
      syntaxError("illegal literal type (), use Unit instead", at = token.pos)
    makeTupleType(lpPos, body, invalidLiteralUnitType, wrap = false)
  }

  private def inParensOrTupleOrUnitExpr(allowRepeated: Boolean): Term = {
    val lpPos = auto.startTokenPos
    val maybeTupleArgs = inParensOnOpenOr(
      commaSeparated(expr(location = PostfixStat, allowRepeated = allowRepeated))
    )(Nil)
    if (maybeTupleArgs.lengthCompare(1) > 0) maybeTupleArgs.foreach {
      case arg: Term.Repeated =>
        syntaxError("repeated argument not allowed here", at = arg.tokens.last.prev)
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
      def paramOrType(): Type = token match {
        case t: Ellipsis =>
          ellipsis[Type](t)
        case t: Unquote =>
          unquote[Type](t)
        case KwImplicit() if !hasImplicits =>
          next()
          hasImplicits = true
          paramOrType()
        case t: Ident if tryAhead[Colon] =>
          if (hasTypes)
            syntaxError(
              "can't mix function type and dependent function type syntaxes",
              at = token
            )
          hasParams = true
          val startPos = in.prevTokenPos
          next() // skip colon
          autoEndPos(startPos)(Type.TypedParam(identName(t, startPos, Type.Name(_)), typ()))
        case _ =>
          if (hasParams)
            syntaxError(
              "can't mix function type and dependent function type syntaxes",
              at = token
            )
          hasTypes = true
          paramType()
      }

      val openParenPos = in.tokenPos
      val ts = inParensOr(commaSeparated(paramOrType()))(Nil)
      // NOTE: can't have this, because otherwise we run into #312
      // newLineOptWhenFollowedBy[LeftParen]

      if (hasParams && !dialect.allowDependentFunctionTypes)
        syntaxError("dependent function types are not supported", at = token)
      if (!hasTypes && !hasImplicits && token.is[LeftParen]) {
        val message = "can't have multiple parameter lists in function types"
        syntaxError(message, at = token)
      }

      def funcParams = autoEndPos(openParenPos)(ts.reduceWith(Type.FuncParamClause.apply))
      if (allowFunctionType && token.is[RightArrow]) {
        Type.Function(funcParams, { next(); typeIndentedOpt() })
      } else if (allowFunctionType && token.is[ContextArrow]) {
        Type.ContextFunction(funcParams, { next(); typeIndentedOpt() })
      } else {
        ts.foreach {
          case t: Type.ByName => syntaxError("by name type not allowed here", at = t)
          case t: Type.Repeated => syntaxError("repeated type not allowed here", at = t)
          case _ =>
        }
        val simple = simpleTypeRest(makeTupleType(openParenPos, ts), openParenPos)
        val compound = compoundTypeRest(annotTypeRest(simple, openParenPos), openParenPos)
        infixTypeRest(compound) match {
          case `compound` => compound
          case t => autoEndPos(openParenPos)(t)
        }
      }
    }

    private def typeLambdaOrPoly(): Type = {
      val quants = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
      if (acceptOpt[TypeLambdaArrow]) {
        val tpe = typeIndentedOpt()
        Type.Lambda(quants, tpe)
      } else if (acceptOpt[RightArrow]) {
        val tpe = typeIndentedOpt()
        if (tpe.is[Type.FunctionType])
          Type.PolyFunction(quants, tpe)
        else
          syntaxError("polymorphic function types must have a value parameter", at = token)
      } else {
        syntaxError("expected =>> or =>", at = token)
      }
    }

    def typeIndentedOpt(): Type = {
      if (acceptOpt[Indentation.Indent]) {
        indentedAfterOpen(typ())
      } else {
        typ()
      }
    }

    def typ(): Type = autoPosOpt {
      val startPos = auto.startTokenPos
      val t: Type =
        if (token.is[LeftBracket] && dialect.allowTypeLambdas) typeLambdaOrPoly()
        else infixTypeOrTuple()

      def funcParams = autoEndPos(startPos)((t :: Nil).reduceWith(Type.FuncParamClause.apply))
      token match {
        case RightArrow() => Type.Function(funcParams, { next(); typeIndentedOpt() })
        case ContextArrow() => Type.ContextFunction(funcParams, { next(); typeIndentedOpt() })
        case KwForsome() => Type.Existential(t, { next(); existentialStats() })
        case KwMatch() if dialect.allowTypeMatch => Type.Match(t, { next(); typeCaseClauses() })
        case _ => t
      }
    }

    def typeCaseClauses(): List[TypeCase] = {
      def cases() = listBy[TypeCase] { allCases =>
        while (token.is[KwCase]) {
          allCases += typeCaseClause()
          newLinesOpt()
        }
      }
      if (acceptOpt[LeftBrace])
        inBracesAfterOpen(cases())
      else if (acceptOpt[Indentation.Indent])
        indentedAfterOpen(cases())
      else {
        syntaxError("Expected braces or indentation", at = token.pos)
      }

    }

    def typeCaseClause(): TypeCase = autoPos {
      accept[KwCase]
      val pat = infixTypeOrTuple(inMatchType = true)
      accept[RightArrow]
      val tpe = typeIndentedOpt()
      TypeCase(
        pat,
        tpe
      )
    }

    def quasiquoteType(): Type = entrypointType()

    def entrypointType(): Type = paramType()

    def typeArgs(): Type.ArgClause = autoPos(inBrackets(types()).reduceWith(Type.ArgClause.apply))

    def infixTypeOrTuple(inMatchType: Boolean = false): Type = {
      if (token.is[LeftParen]) tupleInfixType(allowFunctionType = !inMatchType)
      else infixType(inMatchType = inMatchType)
    }

    @inline def infixType(inMatchType: Boolean = false): Type = maybeAnonymousLambda(
      infixTypeRest(compoundType(inMatchType = inMatchType), inMatchType = inMatchType)
    )

    @inline
    private def infixTypeRest(t: Type, inMatchType: Boolean = false): Type =
      if (dialect.useInfixTypePrecedence)
        infixTypeRestWithPrecedence(t, inMatchType = inMatchType)
      else
        infixTypeRestWithMode(
          t,
          InfixMode.FirstOp,
          t.startTokenPos,
          identity,
          inMatchType = inMatchType
        )

    @tailrec
    private final def infixTypeRestWithMode(
        t: Type,
        mode: InfixMode.Value,
        startPos: Int,
        f: Type => Type,
        inMatchType: Boolean = false
    ): Type = {
      @inline def verifyLeftAssoc(at: Tree, leftAssoc: Boolean = true) =
        if (mode != InfixMode.FirstOp) checkAssoc(at, leftAssoc, mode == InfixMode.LeftOp)
      token match {
        case Ident("*") if ahead(token match {
              case _: RightParen | _: Comma | _: Equals | _: RightBrace | _: EOF => true
              case _ => false
            }) => // we assume that this is a type specification for a vararg parameter
          f(t)
        case _: Ident | _: Unquote =>
          val op = typeName()
          val leftAssoc = op.isLeftAssoc
          verifyLeftAssoc(op, leftAssoc)
          newLineOptWhenFollowedBy[TypeIntro]
          val typ = compoundType(inMatchType = inMatchType)
          def mkOp(t1: Type) = atPos(startPos, t1)(Type.ApplyInfix(t, op, t1))
          if (leftAssoc)
            infixTypeRestWithMode(
              mkOp(typ),
              InfixMode.LeftOp,
              startPos,
              f,
              inMatchType = inMatchType
            )
          else
            infixTypeRestWithMode(
              typ,
              InfixMode.RightOp,
              typ.startTokenPos,
              f.compose(mkOp),
              inMatchType = inMatchType
            )
        case _ =>
          f(t)
      }
    }

    private final def infixTypeRestWithPrecedence(t: Type, inMatchType: Boolean = false): Type = {
      val ctx = TypeInfixContext
      val base = ctx.stack
      @inline def reduce(rhs: ctx.Typ, op: Option[ctx.Op]): ctx.Typ =
        ctx.reduceStack(base, rhs, rhs, op)
      @tailrec
      def loop(rhs: ctx.Typ): ctx.Typ = token match {
        case Ident("*") if ahead(token match {
              case _: RightParen | _: Comma | _: Equals | _: RightBrace | _: EOF => true
              case _ => false
            }) => // we assume that this is a type specification for a vararg parameter
          reduce(rhs, None)
        case _: Ident | _: Unquote =>
          val op = typeName()
          newLineOptWhenFollowedBy[TypeIntro]
          ctx.push(ctx.UnfinishedInfix(reduce(rhs, Some(op)), op))
          loop(compoundType(inMatchType = inMatchType))
        case _ =>
          reduce(rhs, None)
      }
      loop(t)
    }

    def compoundType(inMatchType: Boolean = false): Type = {
      if (token.is[LeftBrace])
        refinement(innerType = None)
      else {
        val startPos = auto.startTokenPos
        compoundTypeRest(annotType(startPos, inMatchType = inMatchType), startPos)
      }
    }

    def compoundTypeRest(typ: Type, startPos: Int): Type = {
      @tailrec
      def gatherWithTypes(previousType: Type): Type = {
        if (acceptOpt[KwWith]) {
          /* Indentation means a refinement and we cannot join
           * refinements this way so stop looping.
           */
          if (token.is[Indentation.Indent]) {
            autoEndPos(startPos)(Type.Refine(Some(previousType), indented(refineStatSeq())))
          } else {
            val rhs = annotType()
            val t = autoEndPos(startPos)(Type.With(previousType, rhs))
            gatherWithTypes(t)
          }
        } else {
          previousType
        }
      }

      val t = gatherWithTypes(typ)
      val wasLF = token.is[LF]

      // the indentation needs to be higher than the containing one
      def canAddBracesRefinement =
        !dialect.allowSignificantIndentation || !wasLF || currentIndentation > previousIndentation

      if (isAfterOptNewLine[LeftBrace] && canAddBracesRefinement) {
        refinement(innerType = Some(t))
      } else t
    }

    def annotType(inMatchType: Boolean = false): Type =
      annotType(auto.startTokenPos, inMatchType = inMatchType)

    private def annotType(startPos: Int, inMatchType: Boolean): Type =
      annotTypeRest(simpleType(inMatchType = inMatchType), startPos)

    def annotTypeRest(t: Type, startPos: Int): Type = {
      val annots = ScalametaParser.this.annots(skipNewLines = false)
      if (annots.isEmpty) t
      else autoEndPos(startPos)(Type.Annotate(t, annots))
    }

    def simpleType(inMatchType: Boolean = false): Type = {
      val startPos = auto.startTokenPos
      def wildcardType(): Type = {
        next()
        Type.Wildcard(typeBounds())
      }
      def anonymousParamWithVariant(value: String): Type = {
        val variant = if (value(0) == '+') Mod.Covariant() else Mod.Contravariant()
        Type.AnonymousParam(Some(atPos(startPos)(variant)))
      }
      val res = token match {
        case LeftParen() => makeTupleType(startPos, inParensOnOpen(types()))
        case MacroSplicedIdent(ident) =>
          Type.Macro(macroSplicedIdent(ident))
        case MacroSplice() =>
          Type.Macro(macroSplice())
        case _: Underscore if inMatchType =>
          next(); Type.PatWildcard()
        case _: Underscore if dialect.allowUnderscoreAsTypePlaceholder =>
          next(); Type.AnonymousParam(None)
        case _: Underscore =>
          wildcardType()
        case Ident("?") if !inMatchType && dialect.allowQuestionMarkAsTypeWildcard =>
          wildcardType()
        case Ident("*") if !inMatchType && dialect.allowStarAsTypePlaceholder =>
          next(); Type.AnonymousParam(None)
        case Ident(value @ ("-*" | "+*")) if !inMatchType && dialect.allowStarAsTypePlaceholder =>
          next(); anonymousParamWithVariant(value)
        case Ident(value @ ("+" | "-"))
            if (dialect.allowPlusMinusUnderscoreAsIdent || dialect.allowUnderscoreAsTypePlaceholder) &&
              !inMatchType && tryAhead[Underscore] =>
          next() // Ident and Underscore
          if (dialect.allowUnderscoreAsTypePlaceholder)
            anonymousParamWithVariant(value)
          else
            Type.Name(s"${value}_")
        case _: Literal =>
          if (dialect.allowLiteralTypes) literal()
          else syntaxError(s"$dialect doesn't support literal types", at = path())
        case Ident("-") if dialect.allowLiteralTypes && tryAhead[NumericConstant[_]] =>
          literal(isNegated = true)
        case _ =>
          val ref = path().become[Term.Ref]
          if (token.isNot[Dot]) {
            ref match {
              case q: Quasi =>
                q.become[Type]
              case Term.Select(qual: Term.Quasi, name: Term.Name.Quasi) =>
                val newQual = qual.become[Term.Ref]
                val newName = name.become[Type.Name]
                Type.Select(newQual, newName)
              case Term.Select(qual: Term.Ref, name) =>
                val newName = name.becomeOr(x => copyPos(x)(Type.Name(x.value)))
                Type.Select(qual, newName)
              case name: Term.Name =>
                Type.Name(name.value)
              case _ =>
                syntaxError("identifier expected", at = ref)
            }
          } else {
            next()
            accept[KwType]
            Type.Singleton(ref)
          }
      }
      simpleTypeRest(autoEndPosOpt(startPos)(res), startPos)
    }

    @tailrec
    private final def simpleTypeRest(t: Type, startPos: Int): Type = token match {
      case Hash() =>
        next()
        simpleTypeRest(autoEndPos(startPos)(Type.Project(t, typeName())), startPos)
      case LeftBracket() =>
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
        case q: Quasi =>
          q
        case tpe @ Type.Name(value) if convertTypevars && value(0).isLower =>
          Type.Var(tpe)
        case tpe: Type.Name =>
          tpe
        case tpe: Type.Select =>
          tpe
        case Type.Project(qual, name) =>
          val qual1 = loop(qual, convertTypevars = false)
          val name1 = name
          Type.Project(qual1, name1)
        case tpe: Type.Singleton =>
          tpe
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
        case t: Type.Wildcard =>
          Type.Wildcard(t.bounds)
        case t: Type.AnonymousParam =>
          Type.AnonymousParam(t.variant)
        case t: Type.Placeholder =>
          val bounds1 = t.bounds
          Type.Placeholder(bounds1)
        case tpe: Lit =>
          tpe
      })
      val t: Type = {
        if (allowInfix) {
          val t = if (token.is[LeftParen]) tupleInfixType() else compoundType()
          token match {
            case KwForsome() => next(); copyPos(t)(Type.Existential(t, existentialStats()))
            case Ident("|") => t
            case _: Unquote | _: Ident => infixTypeRest(t)
            case _ => t
          }
        } else {
          compoundType()
        }
      }
      loop(t, convertTypevars = allowImmediateTypevars)
    }

    def patternTypeArgs() = autoPos(inBrackets(commaSeparated {
      patternTyp(allowInfix = true, allowImmediateTypevars = true)
    }).reduceWith(Type.ArgClause.apply))
  }

  private trait AllowedName[T]
  private object AllowedName {
    implicit object AllowedTermName extends AllowedName[Term.Name]
    implicit object AllowedTypeName extends AllowedName[Type.Name]
  }

  private def identName[T <: Tree](ident: Ident, pos: Int, ctor: String => T): T =
    atPos(pos)(ctor(ident.value))
  private def name[T <: Tree: AllowedName: AstInfo](ctor: String => T): T =
    token match {
      case t: Ident =>
        val res = identName(t, in.tokenPos, ctor)
        next()
        res
      case t: Unquote =>
        unquote[T](t)
      case _ =>
        syntaxErrorExpected[Ident]
    }

  def termName(): Term.Name = name(Term.Name(_))
  def typeName(): Type.Name = name(Type.Name(_))

  def path(thisOK: Boolean = true): Term.Ref = {
    val startsAtBofIfUnquote = dialect.allowUnquotes && token.prev.is[BOF]
    def stop = token.isNot[Dot] || !tryAhead(token match {
      case _: KwThis | _: KwSuper | _: Ident | _: Unquote => true
      case _ => false
    })
    @inline def maybeSelectors(ref: Term.Ref): Term.Ref =
      if (stop) ref else selectors(ref)
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
      if (startsAtBofIfUnquote && token.is[EOF]) superp
      else {
        accept[Dot]
        maybeSelectors(autoEndPos(name)(Term.Select(superp, termName())))
      }
    }
    def getAnonQual(): Name =
      try autoPos(Name.Anonymous())
      finally next()
    def getQual(name: Term.Name): Name = {
      next()
      name.becomeOr[Name](x => copyPos(x)(Name.Indeterminate(x.value)))
    }
    token match {
      case _: KwThis => getThis(getAnonQual())
      case _: KwSuper => getSuper(getAnonQual())
      case _ =>
        val name = termName()
        if (stop) name
        else
          token match {
            case _: KwThis => getThis(getQual(name))
            case _: KwSuper => getSuper(getQual(name))
            case _ =>
              selectors(name.become[Term])
          }
    }
  }

  def selector(t: Term, startPos: Int): Term.Select =
    autoEndPos(startPos)(Term.Select(t, termName()))
  @inline
  def selector(t: Term): Term.Select = selector(t, t.startTokenPos)
  @tailrec
  private final def selectors(t: Term): Term.Ref = {
    val t1 = selector(t)
    if (token.is[Dot] && tryAhead[Ident])
      selectors(t1)
    else t1
  }

  def mixinQualifier(): Name = {
    if (acceptOpt[LeftBracket]) {
      inBracketsAfterOpen {
        typeName().becomeOr[Name](x => copyPos(x)(Name.Indeterminate(x.value)))
      }
    } else {
      autoPos(Name.Anonymous())
    }
  }

  def stableId(): Term.Ref =
    path(thisOK = false)

  def qualId(): Term.Ref = {
    val name = termName().become[Term.Ref]
    if (acceptOpt[Dot]) selectors(name) else name
  }

  def literal(isNegated: Boolean = false): Lit = {
    val startPos = if (isNegated) auto.endTokenPos else auto.startTokenPos
    def isHex = {
      val syntax = token.syntax
      syntax.startsWith("0x") || syntax.startsWith("0X")
    }
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
    autoEndPos(startPos)(res)
  }

  private def interpolateWith[Ctx, Ret <: Tree](
      arg: => Ctx,
      result: (Term.Name, List[Lit], List[Ctx]) => Ret
  ): Ret = autoPos {
    val partsBuf = new ListBuffer[Lit]
    val argsBuf = new ListBuffer[Ctx]
    @tailrec
    def loop(): Unit = token match {
      case Interpolation.Part(value) =>
        partsBuf += atCurPos(Lit.String(value))
        next()
        loop()
      case Interpolation.SpliceStart() =>
        next()
        argsBuf += arg
        accept[Interpolation.SpliceEnd]
        loop()
      case _ =>
    }
    val interpolator = atCurPos(token match {
      case Interpolation.Id(value) => next(); Term.Name(value)
      case _ => syntaxErrorExpected[Interpolation.Id]
    })
    accept[Interpolation.Start]
    loop()
    accept[Interpolation.End]
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  private def xmlWith[Ctx, Ret <: Tree](
      arg: => Ctx,
      result: (List[Lit], List[Ctx]) => Ret
  ): Ret = autoPos {
    val partsBuf = new ListBuffer[Lit]
    val argsBuf = new ListBuffer[Ctx]
    @tailrec
    def loop(): Unit = token match {
      case Xml.Part(value) =>
        partsBuf += atCurPos(Lit.String(value))
        next()
        loop()
      case Xml.SpliceStart() =>
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

  def interpolateTerm(): Term.Interpolate =
    interpolateWith(unquoteExpr(), Term.Interpolate.apply)

  def xmlTerm(): Term.Xml =
    xmlWith(unquoteExpr(), Term.Xml.apply)

  def interpolatePat(): Pat.Interpolate =
    interpolateWith(unquotePattern(), Pat.Interpolate.apply)

  def xmlPat(): Pat.Xml =
    xmlWith(unquoteSeqPattern(), Pat.Xml.apply)

  /* ------------- NEW LINES ------------------------------------------------- */

  @inline
  def newLineOpt(): Unit = {
    if (token.is[LF]) next()
  }

  @inline
  def newLinesOpt(): Unit = {
    if (token.is[LF] || token.is[LFLF]) next()
  }

  def newLineOptWhenFollowedBy[T: TokenClassifier]: Unit = {
    token.is[LF] && tryAhead[T]
  }

  def newLineOptWhenFollowedBySignificantIndentationAnd(cond: Token => Boolean): Boolean = {
    token.is[LF] && {
      val prev = currentIndentation
      prev >= 0 && tryAhead(cond(token) && currentIndentation > prev)
    }
  }

  def isAfterOptNewLine[T: TokenClassifier]: Boolean = {
    if (token.is[LF]) tryAhead[T] else token.is[T]
  }

  def isAfterOptNewLine(cond: => Boolean): Boolean = {
    if (token.is[LF]) tryAhead(cond) else cond
  }

  def getAfterOptNewLine[A](body: => Option[A]): Option[A] = {
    if (token.is[LF]) tryAhead(body) else body
  }

  /* ------------- TYPES ---------------------------------------------------- */

  def typedOpt(): Option[Type] =
    if (acceptOpt[Colon]) {
      if (token.is[At] && ahead(token.is[Ident])) {
        val startPos = auto.startTokenPos
        Some(outPattern.annotTypeRest(autoEndPos(startPos)(Type.AnonymousName()), startPos))
      } else {
        Some(typ())
      }
    } else None

  def typeOrInfixType(location: Location): Type =
    if (location == NoStat || location == PostfixStat) typ()
    else startInfixType()

  /* ----------- EXPRESSIONS ------------------------------------------------ */

  def condExpr(): Term = inParens(expr())

  def expr(): Term = expr(location = NoStat, allowRepeated = false)

  def quasiquoteExpr(): Term = expr(location = NoStat, allowRepeated = true)

  def entrypointExpr(): Term = expr(location = NoStat, allowRepeated = false)

  def unquoteExpr(): Term =
    token match {
      case Ident(_) => termName()
      case LeftBrace() => dropTrivialBlock(expr(location = NoStat, allowRepeated = true))
      case KwThis() =>
        val qual = autoPos { next(); Name.Anonymous() }
        copyPos(qual)(Term.This(qual))
      case _ =>
        syntaxError(
          "error in interpolated string: identifier, `this' or block expected",
          at = token
        )
    }

  private def exprMaybeIndented(): Term = {
    if (token.is[Indentation.Indent]) {
      blockExpr()
    } else {
      expr()
    }
  }

  private def tryAcceptWithOptLF[T: TokenClassifier]: Boolean = {
    acceptOpt[T] || {
      val ok = token.is[LF] && tryAhead[T]
      if (ok) next()
      ok
    }
  }

  /**
   * Deals with Scala 3 concept of {{{inline x match { ...}}}. Since matches can also be chained in
   * Scala 3 we need to create the Match first and only then add the the inline modifier.
   */
  def inlineMatchClause(inlineMods: List[Mod]) = {
    autoEndPos(inlineMods)(postfixExpr(allowRepeated = false)) match {
      case t: Term.Match =>
        copyPos(t)(t.fullCopy(mods = inlineMods))
      case other =>
        syntaxError("`inline` must be followed by an `if` or a `match`", at = other.pos)
    }
  }

  private def matchClause(t: Term, startPos: Int) = {
    val cases =
      if (acceptOpt[Indentation.Indent])
        indentedAfterOpen(caseClauses())
      else {
        accept[LeftBrace]
        inBracesAfterOpen(caseClauses())
      }
    autoEndPos(startPos)(Term.Match(t, cases))
  }

  def ifClause(mods: List[Mod] = Nil) = autoEndPos(mods) {
    accept[KwIf]
    val (cond, thenp) = if (token.isNot[LeftParen] && dialect.allowSignificantIndentation) {
      val cond = exprMaybeIndented()
      newLineOpt()
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
    } else if (token.is[Semicolon] && tryAhead[KwElse]) {
      next(); Term.If(cond, thenp, expr(), mods)
    } else {
      Term.If(cond, thenp, autoPos(Lit.Unit()), mods)
    }
  }

  def condExprInParens[T <: Token: TokenInfo]: Term = {
    def isFollowedBy[U: TokenClassifier] = {
      if (token.is[LF] || token.is[LFLF] || token.is[EOF]) {
        false
      } else { followedByToken[U] }
    }
    if (dialect.allowSignificantIndentation) {
      val forked = in.fork
      val simpleExpr = condExpr()
      if ((token.is[Ident] && token.isLeadingInfixOperator) || token.is[Dot] || isFollowedBy[T]) {
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
    maybeAnonymousFunctionUnlessPostfix(autoPosOpt(token match {
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
          case LeftParen() => inParensOnOpen(expr())
          case LeftBrace() | Indentation.Indent() => block()
          case _ => expr()
        }
        def caseClausesOrExpr = caseClausesIfAny().getOrElse(expr())
        val catchopt =
          if (tryAcceptWithOptLF[KwCatch]) Some {
            if (acceptOpt[CaseIntro]) caseClause(true)
            else if (acceptOpt[Indentation.Indent]) indentedAfterOpen(caseClausesOrExpr)
            else if (acceptOpt[LeftBrace]) inBracesAfterOpen(caseClausesOrExpr)
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
          if (!acceptOpt[KwDo]) {
            in.observeIndented()
          }
          Term.While(cond, exprMaybeIndented())
        } else if (token.is[Indentation.Indent]) {
          val cond = block()
          newLineOpt()
          accept[KwDo]
          Term.While(cond, exprMaybeIndented())
        } else {
          val cond = expr()
          newLineOpt()
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
          if (acceptOpt[LeftBrace]) inBracesAfterOpen(enumerators())
          else if (token.is[LeftParen]) {
            val forked = in.fork
            Try(inParensOnOpen(enumerators())) match {
              // Dotty retry in case of `for (a,b) <- list1.zip(list2) yield (a, b)`
              case Failure(_) if dialect.allowSignificantIndentation =>
                in = forked
                enumerators()
              case Failure(exception) => throw exception
              case Success(value) =>
                value
            }
          } else if (acceptOpt[Indentation.Indent]) {
            indentedAfterOpen(enumerators())
          } else {
            enumerators()
          }

        newLinesOpt()
        if (acceptOpt[KwDo]) {
          Term.For(enums, exprMaybeIndented())
        } else if (acceptOpt[KwYield]) {
          Term.ForYield(enums, exprMaybeIndented())
        } else {
          in.observeIndented()
          Term.For(enums, exprMaybeIndented())
        }
      case KwReturn() =>
        next()
        if (token.is[ExprIntro]) Term.Return(expr())
        else Term.Return(autoPos(Lit.Unit()))
      case KwThrow() =>
        next()
        Term.Throw(expr())
      case KwImplicit() =>
        next()
        implicitClosure(location)
      case _ =>
        val startPos = auto.startTokenPos
        @inline def addPos[T <: Tree](body: T) = autoEndPos(startPos)(body)
        var t: Term = postfixExpr(allowRepeated, PostfixStat)
        def repeatedTerm(nextTokens: () => Unit) = {
          if (allowRepeated) t = addPos { nextTokens(); Term.Repeated(t) }
          else syntaxError("repeated argument not allowed here", at = token)
        }
        if (token.is[Equals]) {
          t match {
            case _: Term.Ref | _: Term.Apply | _: Quasi =>
              next()
              t = addPos(Term.Assign(t, expr(location = NoStat, allowRepeated = true)))
            case _ =>
          }
        } else if (token.is[Colon] && allowFewerBraces) {
          val colonPos = auto.startTokenPos
          next()
          in.observeIndented()
          val args = blockExpr(allowRepeated = false)
          val argClause = autoEndPos(colonPos)(Term.ArgClause(args :: Nil))
          val arguments = addPos { Term.Apply(t, argClause) }
          t = simpleExprRest(arguments, canApply = true, startPos = startPos)
        } else if (acceptOpt[Colon]) {
          if (token.is[At] || (token.is[Ellipsis] && ahead(token.is[At]))) {
            t = addPos(Term.Annotate(t, annots(skipNewLines = false)))
          } else if (token.is[Underscore] && ahead(isStar)) {
            repeatedTerm(nextTwice)
          } else {
            // this does not necessarily correspond to syntax, but is necessary to accept lambdas
            // check out the `if (token.is[RightArrow]) { ... }` block below
            t = addPos(Term.Ascribe(t, typeOrInfixType(location)))
          }
        } else if (allowRepeated && isVarargStarParam()) {
          repeatedTerm(next)
        } else if (acceptOpt[KwMatch]) {
          t = matchClause(t, startPos)
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
              @tailrec
              def unapply(tree: Tree): Boolean = tree match {
                case Term.Quasi(0, _) => true
                case Term.Quasi(1, t) => unapply(t)
                case NameLike() => true
                case Term.Ascribe(Term.Quasi(0, _), _) => true
                case Term.Ascribe(NameLike(), _) => true
                case _ => false
              }
            }
            t match {
              case Lit.Unit() => true // 1
              case NameLike() => location != TemplateStat // 2-3
              case ParamLike() => // 4-5
                location == BlockStat ||
                scannerTokens(startPos).is[LeftParen] &&
                scannerTokens(in.prevTokenPos).is[RightParen]
              case Term.Tuple(xs) => xs.forall(ParamLike.unapply) // 6
              case _ => false
            }
          }
          if (looksLikeLambda) {
            val params = addPos(convertToParamClause(t))
            val contextFunction = token.is[ContextArrow]
            next()
            val trm = termFunctionBody(location)
            t = addPos {
              if (contextFunction)
                Term.ContextFunction(params, trm)
              else
                Term.Function(params, trm)
            }
          } else {
            // do nothing, which will either allow self-type annotation parsing to kick in
            // or will trigger an unexpected token error down the line
          }
        }
        t
    }))(location)
  }

  private def termFunctionBody(location: Location): Term =
    if (location != BlockStat) expr()
    else {
      blockExpr(isBlockOptional = true) match {
        case partial: Term.PartialFunction if acceptOpt[Colon] =>
          autoEndPos(partial)(Term.Ascribe(partial, startInfixType()))
        case t => t
      }
    }

  private def convertToParam(tree: Term): Option[Term.Param] = {
    def getType: Option[Type] = tree match {
      case t: Term.Ascribe => Some(t.tpe)
      case _ => None
    }
    @tailrec def getName(t: Tree, nest: Int = 0): Option[Name] = t match {
      case t: Name => Some(t)
      case t: Quasi => Some(t.become[Term.Name])
      case t: Term.Select => Some(t.name)
      case t: Term.Placeholder => Some(copyPos(t)(Name.Placeholder()))
      case t: Term.Eta => Some(atPos(t.endTokenPos)(Name.Placeholder()))
      case t: Term.Ascribe if nest == 0 => getName(t.expr, 1)
      case _ => None
    }
    @tailrec def getMod(t: Tree, nest: Int = 0): List[Mod.ParamsType] = t match {
      case _: Quasi => Nil
      case t: Term.Ascribe if nest == 0 => getMod(t.expr, 1)
      case t: Term.Select if nest <= 1 => getMod(t.qual, 2)
      case t: Term.Eta => getMod(t.expr, 2)
      case t @ Term.Name(soft.KwUsing.name) => copyPos(t)(Mod.Using()) :: Nil
      case _ => Nil
    }
    tree match {
      case q: Quasi => Some(q.become[Term.Param])
      case _: Lit.Unit => None
      case t =>
        val name = getName(t).getOrElse(syntaxError("not a legal formal parameter", at = t))
        Some(copyPos(t)(Term.Param(getMod(t), name, getType, None)))
    }
  }

  private def convertToParamClause(tree: Term): Term.ParamClause = (tree match {
    case Term.Tuple(ts) => ts.flatMap(convertToParam)
    case _ => convertToParam(tree).toList
  }).reduceWith(toParamClause(None))

  def implicitClosure(location: Location): Term.Function = {
    require(token.isNot[KwImplicit] && debug(token))
    val implicitPos = in.prevTokenPos
    val paramName = termName()
    val paramTpt = if (acceptOpt[Colon]) Some(typeOrInfixType(location)) else None
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
    def head = stack.head
    def push(unfinishedInfix: UnfinishedInfix): Unit = stack ::= unfinishedInfix
    def pop(): UnfinishedInfix =
      try head
      finally stack = stack.tail

    def reduceStack(
        stack: List[UnfinishedInfix],
        curr: Typ,
        currEnd: EndPos,
        op: Option[Op]
    ): Typ = {
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
      def loop(rhs: Typ): Typ = {
        if (!canReduce) {
          rhs
        } else {
          val lhs = pop()
          val fin = finishInfixExpr(lhs, rhs, currEnd)
          loop(fin)
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
  object termInfixContext extends InfixContext {
    type Typ = Term
    type Op = Term.Name

    // We need to carry lhsStart/lhsEnd separately from lhs.pos
    // because their extent may be bigger than lhs because of parentheses or whatnot.
    case class UnfinishedInfix(
        lhs: Typ,
        op: Op,
        targs: Type.ArgClause
    ) extends Unfinished {
      override def toString = {
        s"[$lhs $op$targs]"
      }
    }

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhsExt, op, targs) = unf
      val lhs = lhsExt match {
        case Term.Tuple(arg :: Nil) => arg
        case x => x
      }

      if (lhs.is[Term.Repeated])
        syntaxError("repeated argument not allowed here", at = lhs.tokens.last.prev)

      def infixExpression() = {
        val args = copyPos(rhs)((rhs match {
          case _: Lit.Unit => Nil
          case t: Term.Tuple => t.args
          case _ => rhs :: Nil
        }).reduceWith(Term.ArgClause(_)))
        atPos(lhsExt, rhsEnd)(Term.ApplyInfix(lhs, op, targs, args))
      }

      op match {
        case _: Quasi =>
          infixExpression()
        case Term.Name("match") =>
          if (targs.nonEmpty) {
            syntaxError("no type parameters can be added for match", at = lhs.tokens.last.prev)
          }

          rhs match {
            case Term.PartialFunction(cases) =>
              atPos(lhsExt, rhsEnd)(Term.Match(lhs, cases))
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
    type Typ = Pat
    type Op = Term.Name

    case class UnfinishedInfix(lhs: Typ, op: Op) extends Unfinished

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Typ, rhsEnd: EndPos): Typ = {
      val UnfinishedInfix(lhsExt, op) = unf
      val lhs = lhsExt match {
        case Pat.Tuple(arg :: Nil) => arg
        case x => x
      }
      val args = copyPos(rhs)((rhs match {
        case _: Lit.Unit => Nil
        case t: Pat.Tuple => t.args
        case _ => rhs :: Nil
      }).reduceWith(Pat.ArgClause.apply))
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

  @inline def checkAssoc(op: Name, leftAssoc: Boolean): Unit =
    checkAssoc(op, op.isLeftAssoc, leftAssoc)

  @inline private def checkAssoc(at: Tree, opLeftAssoc: Boolean, leftAssoc: Boolean): Unit =
    if (opLeftAssoc != leftAssoc)
      syntaxError(
        "left- and right-associative operators with same precedence may not be mixed",
        at = at
      )

  def postfixExpr(allowRepeated: Boolean, location: Location = NoStat): Term = {
    val ctx = termInfixContext
    val base = ctx.stack

    // Skip to later in the `postfixExpr` method to start mental debugging.
    // rhsStartK/rhsEndK may be bigger than then extent of rhsK,
    // so we really have to track them separately.
    @tailrec
    def loop(rhsK: ctx.Typ): ctx.Typ = {
      if (!token.is[Ident] && !token.is[Unquote] &&
        !(token.is[KwMatch] && dialect.allowMatchAsOperator)) {
        // Infix chain has ended.
        // In the running example, we're at `a + b[]`
        // with base = List([a +]), rhsK = List([b]).
        rhsK
      } else if (allowRepeated && isVarargStarParam()) {
        rhsK
      } else {
        val rhsEndK: EndPos = in.prevTokenPos
        // Infix chain continues.
        // In the running example, we're at `a [+] b`.
        val op = if (token.is[KwMatch]) {
          atCurPosNext(Term.Name("match"))
        } else {
          termName() // op = [+]
        }
        val targs = if (token.is[LeftBracket]) exprTypeArgs() else autoPos(Type.ArgClause(Nil))

        // Check whether we're still infix or already postfix by testing the current token.
        // In the running example, we're at `a + [b]` (infix).
        // If we were parsing `val c = a b`, then we'd be at `val c = a b[]` (postfix).
        if (isAfterOptNewLine[ExprIntro]) {
          // Infix chain continues, so we need to reduce the stack.
          // In the running example, base = List(), rhsK = [a].
          val lhsK = ctx.reduceStack(base, rhsK, rhsEndK, Some(op)) // lhsK = [a]
          // afterwards, ctx.stack = List([a +])
          ctx.push(ctx.UnfinishedInfix(lhsK, op, targs))
          val rhsKplus1 = argumentExprsOrPrefixExpr(PostfixStat)
          // Try to continue the infix chain.
          loop(rhsKplus1) // base = List([a +]), rhsKplus1 = List([b])
        } else {
          // Infix chain has ended with a postfix expression.
          // This never happens in the running example.
          val finQual = ctx.reduceStack(base, rhsK, rhsEndK, Some(op))
          if (targs.nonEmpty)
            syntaxError("type application is not allowed for postfix operators", at = token)
          atPos(finQual, op)(Term.Select(finQual, op))
        }
      }
    }

    val startPos = auto.startTokenPos

    // Start the infix chain.
    // We'll use `a + b` as our running example.
    val rhs0 = prefixExpr(allowRepeated)

    // Iteratively read the infix chain via `loop`.
    // rhs0 is now [a]
    // If the next token is not an ident or an unquote, the infix chain ends immediately,
    // and `postfixExpr` becomes a fallthrough.
    val rhsN = loop(rhs0)

    // Infix chain has ended.
    // base contains pending UnfinishedInfix parts and rhsN is the final rhs.
    // For our running example, this'll be List([a +]) and [b].
    // Afterwards, `lhsResult` will be List([a + b]).
    val endPos = auto.endTokenPos
    val lhsResult = ctx.reduceStack(base, rhsN, endPos, None) match {
      case `rhs0` => rhs0
      case res => atPos(startPos, endPos)(res)
    }

    maybeAnonymousFunctionUnlessPostfix(lhsResult)(location)
  }

  def prefixExpr(allowRepeated: Boolean): Term =
    if (!isUnaryOp) simpleExpr(allowRepeated)
    else {
      val startPos = auto.startTokenPos
      val op = termName()
      if (op.value == "-" && token.is[NumericConstant[_]])
        simpleExprRest(literal(isNegated = true), canApply = true, startPos = startPos)
      else {
        simpleExpr0(allowRepeated = true) match {
          case Success(result) => autoEndPos(startPos)(Term.ApplyUnary(op, result))
          case Failure(_) =>
            // maybe it is not unary operator but simply an ident `trait - {...}`
            // we would fail here anyway, let's try to treat it as ident
            simpleExprRest(op, canApply = true, startPos = startPos)
        }
      }
    }

  def simpleExpr(allowRepeated: Boolean): Term = simpleExpr0(allowRepeated).get

  private def simpleExpr0(allowRepeated: Boolean): Try[Term] = {
    var canApply = true
    val startPos = auto.startTokenPos
    val t: Try[Term] = {
      token match {
        case MacroQuote() =>
          Success(macroQuote())
        case MacroSplice() =>
          Success(macroSplice())
        case MacroQuotedIdent(ident) =>
          Success(macroQuotedIdent(ident))
        case MacroSplicedIdent(ident) =>
          Success(macroSplicedIdent(ident))
        case _: Literal =>
          Success(literal())
        case _: Interpolation.Id =>
          Success(interpolateTerm())
        case _: Xml.Start =>
          Success(xmlTerm())
        case Ident(_) | KwThis() | KwSuper() | Unquote() =>
          Success(path().become[Term])
        case Underscore() =>
          Success(atCurPosNext(Term.Placeholder()))
        case LeftParen() =>
          Success(inParensOrTupleOrUnitExpr(allowRepeated = allowRepeated))
        case LeftBrace() =>
          canApply = false
          Success(blockExpr())
        case KwNew() =>
          canApply = false
          Success(autoPos {
            next()
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
    t.map(term => simpleExprRest(term, canApply = canApply, startPos = startPos))
  }

  def polyFunction() = autoPos {
    val quants = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    accept[RightArrow]
    val term = expr()
    Term.PolyFunction(quants, term)
  }

  def macroSplice(): Term = autoPos {
    accept[MacroSplice]
    QuotedSpliceContext.within {
      if (QuotedPatternContext.isInside())
        Term.SplicedMacroPat(autoPos(inBraces(pattern())))
      else
        Term.SplicedMacroExpr(autoPos(Term.Block(inBraces(blockStatSeq()))))
    }
  }

  def macroQuote(): Term = autoPos {
    accept[MacroQuote]
    QuotedSpliceContext.within {
      if (acceptOpt[LeftBrace]) {
        val block = autoEndPos(StartPosPrev)(Term.Block(inBracesAfterOpen(blockStatSeq())))
        Term.QuotedMacroExpr(block)
      } else if (acceptOpt[LeftBracket]) {
        Term.QuotedMacroType(inBracketsAfterOpen(typ()))
      } else {
        syntaxError("Quotation only works for expressions and types", at = token)
      }
    }
  }

  @inline private def macroQuotedIdent(ident: String): Term =
    macroIdent(ident, Term.QuotedMacroExpr.apply)

  @inline private def macroSplicedIdent(ident: String): Term =
    macroIdent(ident, Term.SplicedMacroExpr.apply)

  private def macroIdent(ident: String, f: Term.Name => Term): Term = {
    val curpos = auto.startTokenPos
    next()
    autoEndPos(curpos)(f(atPos(curpos)(Term.Name(ident))))
  }

  @tailrec
  private def simpleExprRest(t: Term, canApply: Boolean, startPos: Int): Term = {
    @inline def addPos(body: Term): Term = autoEndPos(startPos)(body)
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
        if (dialect.allowMatchAsOperator && acceptOpt[KwMatch]) {
          val clause = matchClause(t, startPos)
          // needed if match uses significant identation
          newLineOptWhenFollowedBy[Dot]
          simpleExprRest(clause, canApply = false, startPos = startPos)
        } else {
          simpleExprRest(selector(t, startPos), canApply = true, startPos = startPos)
        }
      case LeftBracket() =>
        t match {
          case _: Quasi | _: Term.Name | _: Term.Select | _: Term.Apply | _: Term.ApplyInfix |
              _: Term.ApplyUnary | _: Term.New | _: Term.Placeholder | _: Term.ApplyUsing |
              _: Term.Interpolate =>
            var app: Term = t
            while (token.is[LeftBracket]) app = addPos(Term.ApplyType(app, exprTypeArgs()))

            simpleExprRest(app, canApply = true, startPos = startPos)
          case _ =>
            addPos(t)
        }
      case LeftParen() | LeftBrace() if canApply =>
        val arguments = addPos(Term.Apply(t, getArgClause()))
        simpleExprRest(arguments, canApply = true, startPos = startPos)
      case _: Colon if canApply && allowFewerBraces =>
        val colonPos = auto.startTokenPos
        val isEol = isEolAfterColon(token)
        next()
        // map:
        val args = if (isEol) {
          in.observeIndented()
          blockExpr(allowRepeated = false)
        } else {
          val paramPos = auto.startTokenPos

          /**
           * We need to handle param and then open indented region, otherwise only the block will be
           * handles and any `.` will be accepted into the block:
           * ```
           * .map: a =>
           *   a+1
           * .filter: x =>
           *   x > 2
           * ```
           * Without manual handling here, filter would be included for `(a+1).filter`
           */
          val param = simpleExpr(allowRepeated = false)
          val params = autoEndPos(paramPos)(convertToParamClause(param))
          val contextFunction = token.is[ContextArrow]
          next()
          val trm = blockExpr(allowRepeated = false)
          autoEndPos(paramPos) {
            if (contextFunction)
              Term.ContextFunction(params, trm)
            else
              Term.Function(params, trm)
          }
        }
        val argClause = autoEndPos(colonPos)(Term.ArgClause(args :: Nil))
        val arguments = addPos { Term.Apply(t, argClause) }
        simpleExprRest(arguments, canApply = true, startPos = startPos)
      case Underscore() =>
        next()
        addPos(Term.Eta(t))
      case _ =>
        t
    }
  }

  private def allowFewerBraces: Boolean =
    dialect.allowFewerBraces && (isEolAfterColon(token) || followingIsLambdaAfterColon())

  private def followingIsLambdaAfterColon(): Boolean = {

    /**
     * Skip matching pairs of `(...)` or `[...]` parentheses.
     * @pre
     *   The current token is `(` or `[`
     */
    def skipParens[T1: TokenClassifier, T2: TokenClassifier]: Boolean = {
      // starts on left delimiter
      @tailrec
      def iter(nest: Int): Boolean = {
        next()
        if (token.is[EOF]) false
        else if (token.is[T2]) nest == 0 || iter(nest - 1)
        else if (token.is[T1]) iter(nest + 1)
        else iter(nest)
      }
      // stops on right delimiter (true), or end (false)
      iter(0)
    }

    ahead {
      val okParams = token match {
        case _: Ident | _: Underscore => true
        case _: LeftParen => skipParens[LeftParen, RightParen]
        case _: LeftBracket => skipParens[LeftBracket, RightBracket]
        case _ => false
      }
      okParams && {
        next()
        token.is[RightArrow] || token.is[ContextArrow]
      } && {
        next()
        token.is[Indentation.Indent]
      }
    }
  }

  private def argumentExprsOrPrefixExpr(location: Location): termInfixContext.Typ = {
    val isBrace = token.is[LeftBrace]
    if (!isBrace && token.isNot[LeftParen]) prefixExpr(allowRepeated = false)
    else {
      def findRep(args: List[Term]): Option[Term.Repeated] = args.collectFirst {
        case Term.Assign(_, rep: Term.Repeated) => rep
        case rep: Term.Repeated => rep
      }
      val lpPos = auto.startTokenPos
      val args =
        if (isBrace) checkNoTripleDot(blockExpr(allowRepeated = true)) :: Nil
        else inParensOnOpenOr(argumentExprsInParens(location))(Nil)
      token match {
        case Dot() | LeftBracket() | LeftParen() | LeftBrace() | Underscore() =>
          findRep(args).foreach(x => syntaxError("repeated argument not allowed here", at = x))
          simpleExprRest(makeTupleTerm(lpPos, args), canApply = true, startPos = lpPos)
        case _ =>
          makeTupleTerm { arg =>
            val res = maybeAnonymousFunction(arg)
            if (isBrace) Right(res) else Left(res :: Nil)
          }(lpPos, args)
      }
    }
  }

  private def argumentExpr(location: Location): Term = token match {
    case t @ Ellipsis(2) => syntaxError(Messages.QuasiquoteRankMismatch(2, 1), at = t)
    case _ => expr(location = location, allowRepeated = true)
  }

  private def getArgClause(location: Location = NoStat): Term.ArgClause = autoPos(token match {
    case _: LeftBrace =>
      Term.ArgClause(blockExpr(allowRepeated = true) :: Nil)
    case _: LeftParen =>
      inParensOnOpenOr(token match {
        case t @ Ellipsis(2) =>
          (ellipsis[Term](t) :: Nil).reduceWith(Term.ArgClause(_))
        case x =>
          val using = x.toString == soft.KwUsing.name && ahead(token.isNot[CantStartStat])
          val mod = if (using) Some(atCurPosNext(Mod.Using())) else None
          argumentExprsInParens(location).reduceWith(Term.ArgClause(_, mod))
      })(Term.ArgClause(Nil))
    case _ =>
      Term.ArgClause(Nil)
  })

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

  def blockExpr(isBlockOptional: Boolean = false, allowRepeated: Boolean = false): Term = {
    if (ahead(token.is[CaseIntro] || (token.is[Ellipsis] && next(token.is[KwCase]))))
      autoPos(Term.PartialFunction {
        if (acceptOpt[LeftBrace]) inBracesAfterOpen(caseClauses()) else indented(caseClauses())
      })
    else block(isBlockOptional, allowRepeated)
  }

  def block(isBlockOptional: Boolean = false, allowRepeated: Boolean = false): Term = autoPos {
    def blockWithStats = Term.Block(blockStatSeq(allowRepeated = allowRepeated))
    if (!isBlockOptional && acceptOpt[LeftBrace]) {
      inBracesAfterOpen(blockWithStats)
    } else {
      val possibleBlock =
        if (acceptOpt[Indentation.Indent]) indentedAfterOpen(blockWithStats) else blockWithStats
      dropOuterBlock(possibleBlock)
    }
  }

  def caseClause(forceSingleExpr: Boolean = false): Case = atPos(StartPosPrev, EndPosPreOutdent) {
    if (token.isNot[KwCase]) {
      def caseBody() = {
        accept[RightArrow]
        val start = auto.startTokenPos
        def parseStatSeq() = blockStatSeq() match {
          case List(q: Quasi) => q.become[Term]
          case List(term: Term) => term
          case other => autoEndPos(start)(Term.Block(other))
        }
        if (acceptOpt[Indentation.Indent]) indentedAfterOpen(parseStatSeq())
        else if (forceSingleExpr) expr(location = BlockStat, allowRepeated = false)
        else parseStatSeq()
      }
      @inline def guard(): Option[Term] = if (token.is[KwIf]) Some(guardOnIf()) else None
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
    @tailrec
    def iter(): Unit = token match {
      case t: Ellipsis =>
        cases += ellipsis[Case](t, 1, accept[KwCase])
        while (token.is[StatSep]) next()
        iter
      case CaseIntro() =>
        next()
        token match {
          case t: Unquote =>
            cases += unquote[Case](t)
            while (token.is[StatSep]) next()
          case _ =>
            cases += caseClause()
            if (token.is[StatSep] && ahead(token.is[CaseIntro])) acceptStatSep()
        }
        iter
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
    enumeratorBuf(enums, isFirst = true)
    while (token.is[StatSep] && tryAhead(!token.is[Indentation.Outdent] && !token.is[KwDo])) {
      enumeratorBuf(enums, isFirst = false)
    }
  }

  private def enumeratorBuf(
      buf: ListBuffer[Enumerator],
      isFirst: Boolean,
      allowNestedIf: Boolean = true
  ): Unit = token match {
    case _: KwIf if !isFirst => buf += enumeratorGuardOnIf()
    case t: Ellipsis => buf += ellipsis[Enumerator](t, 1)
    case t: Unquote if ahead(!token.is[Equals] && !token.is[LeftArrow]) =>
      buf += unquote[Enumerator](t) // support for q"for ($enum1; ..$enums; $enum2)"
    case _ => generatorBuf(buf, !isFirst, allowNestedIf)
  }

  def quasiquoteEnumerator(): Enumerator = entrypointEnumerator()

  def entrypointEnumerator(): Enumerator = {
    listBy[Enumerator](enumeratorBuf(_, isFirst = false, allowNestedIf = false)) match {
      case enumerator :: Nil => enumerator
      case other => unreachable(debug(other))
    }
  }

  private def generatorBuf(
      buf: ListBuffer[Enumerator],
      eqOK: Boolean,
      allowNestedIf: Boolean = true
  ): Unit = {
    val startPos = in.tokenPos
    val hasVal = acceptOpt[KwVal]
    val isCase = acceptOpt[KwCase]

    val pat = noSeq.pattern1(isForComprehension = true)
    val hasEq = token.is[Equals]

    if (hasVal) {
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated", at = token)
      else syntaxError("val in for comprehension must be followed by assignment", at = token)
    }

    if (hasEq && eqOK) next()
    else accept[LeftArrow]
    val rhs = expr()

    buf += autoEndPos(startPos) {
      if (hasEq) Enumerator.Val(pat, rhs)
      else if (isCase) Enumerator.CaseGenerator(pat, rhs)
      else Enumerator.Generator(pat, rhs)
    }
    if (allowNestedIf) {
      while (token.is[KwIf]) {
        buf += enumeratorGuardOnIf()
      }
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

    def patterns(): List[Pat] = commaSeparated(pattern())

    def pattern(): Pat = patternAlternatives(Nil)

    @tailrec
    private def patternAlternatives(pats: List[Pat]): Pat = {
      val pat = pattern1()
      checkNoTripleDot(pat)
      if (isBar) {
        next()
        patternAlternatives(pat :: pats)
      } else if (pats.isEmpty) {
        pat
      } else {
        val endPos = pat.endTokenPos
        pats.foldLeft(pat) { case (rtAll, ltOne) =>
          atPos(ltOne.startTokenPos, endPos)(Pat.Alternative(ltOne, rtAll))
        }
      }
    }

    def quasiquotePattern(): Pat = {
      // NOTE: As per quasiquotes.md
      // * p"x" => Pat.Var (ok)
      // * p"X" => Pat.Var (needs postprocessing, parsed as Term.Name)
      // * p"`x`" => Term.Name (ok)
      // * p"`X`" => Term.Name (ok)
      val nonbqIdent = isIdent && !isBackquoted
      argumentPattern() match {
        case pat: Term.Name if nonbqIdent => copyPos(pat)(Pat.Var(pat))
        case pat => pat
      }
    }

    def entrypointPattern(): Pat = {
      pattern()
    }

    def unquotePattern(): Pat = {
      dropAnyBraces(pattern())
    }

    private def isArglistEnd = token.is[RightParen] || token.is[RightBrace] || token.is[EOF]

    private def getSeqWildcard(isEnabled: Boolean, elseF: => Pat, mapF: Pat => Pat = identity) = {
      if (isEnabled && isSequenceOK && token.is[Underscore]) {
        val startPos = in.tokenPos
        if (tryAhead(isStar && next(isArglistEnd)))
          mapF(autoEndPos(startPos)(Pat.SeqWildcard()))
        else elseF
      } else elseF
    }

    def pattern1(isForComprehension: Boolean = false): Pat = {
      val p = pattern2(isForComprehension)
      @inline def typed() =
        Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
      val pat = p match {
        case _ if token.isNot[Colon] => p
        case _: Quasi =>
          next()
          typed()
        case _: Pat.Var =>
          next()
          if (!dialect.allowColonForExtractorVarargs && token.is[Underscore] && ahead(isStar))
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
        case _ if token.isNot[At] => p
        case _: Quasi =>
          next()
          Pat.Bind(p, pattern3())
        case _: Term.Name =>
          syntaxError(
            "Pattern variables must start with a lower-case letter. (SLS 8.1.1.)",
            at = p
          )
        case p: Pat.Var =>
          next()
          Pat.Bind(p, getSeqWildcard(dialect.allowAtForExtractorVarargs, pattern3()))
        case _: Pat.Wildcard =>
          next()
          getSeqWildcard(dialect.allowAtForExtractorVarargs, pattern3())
        case p =>
          p
      }
      if (pat eq p) p else autoEndPos(p)(pat)
    }

    def pattern3(isForComprehension: Boolean = false): Pat = {
      val ctx = patInfixContext
      val lhs = simplePattern(badPattern3, isForComprehension = isForComprehension)
      val base = ctx.stack
      @tailrec
      def loop(rhs: ctx.Typ): ctx.Typ = {
        val op =
          if (isIdentExcept("|") || token.is[Unquote]) Some(termName())
          else None
        val lhs1 = ctx.reduceStack(base, rhs, rhs, op)
        op match {
          case Some(op) =>
            if (token.is[LeftBracket])
              syntaxError("infix patterns cannot have type arguments", at = token)
            ctx.push(ctx.UnfinishedInfix(lhs1, op))
            val rhs1 = simplePattern(badPattern3, isRhs = true)
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
      syntaxError(msg, at = token)
    }

    def simplePattern(): Pat =
      // simple diagnostics for this entry point
      simplePattern(token => syntaxError("illegal start of simple pattern", at = token))
    def simplePattern(
        onError: Token => Nothing,
        isRhs: Boolean = false,
        isForComprehension: Boolean = false
    ): Pat =
      autoPos(token match {
        case _: Ident | _: KwThis | _: Unquote =>
          val isBackquoted = parser.isBackquoted
          val sid = stableId()
          if (token.is[NumericConstant[_]]) {
            sid match {
              case Term.Name("-") => return literal(isNegated = true)
              case _ =>
            }
          }
          val targs = if (token.is[LeftBracket]) Some(patternTypeArgs()) else None
          if (token.is[LeftParen]) {
            val ref = sid.become[Term]
            Pat.Extract(
              targs.fold(ref)(x => autoEndPos(sid)(Term.ApplyType(ref, x))),
              autoPos(argumentPatterns().reduceWith(Pat.ArgClause.apply))
            )
          } else if (targs.nonEmpty)
            syntaxError("pattern must be a value", at = token)
          else
            sid match {
              case name: Term.Name.Quasi => name.become[Pat]
              case name: Term.Name =>
                if (dialect.allowPostfixStarVarargSplices && isStar && tryAhead(!token.is[Ident]))
                  Pat.Repeated(name)
                else if ((!isBackquoted || isForComprehension) && {
                    val first = name.value.head
                    first == '_' || Character.getType(first) == Character.LOWERCASE_LETTER ||
                    dialect.allowUpperCasePatternVarBinding && token.is[At]
                  })
                  Pat.Var(name)
                else name
              case select: Term.Select => select
              case _ => unreachable(debug(token, token.structure, sid, sid.structure))
            }
        case _: Underscore =>
          next()
          if (isSequenceOK && isStar && tryAhead(isArglistEnd))
            Pat.SeqWildcard()
          else
            Pat.Wildcard()
        case _: Literal =>
          literal()
        case _: Interpolation.Id =>
          interpolatePat()
        case _: Xml.Start =>
          xmlPat()
        case _: LeftParen =>
          val lpPos = auto.startTokenPos
          val patterns = inParensOnOpenOr(noSeq.patterns())(Nil)
          makeTuple(lpPos, patterns, Lit.Unit(), Pat.Tuple.apply) {
            case t if !isRhs => Right(t)
            case t @ Pat.Tuple(_ :: Nil) => Right(t)
            case t => Left(t :: Nil)
          }
        case _: MacroQuote =>
          QuotedPatternContext.within {
            Pat.Macro(macroQuote())
          }
        case MacroQuotedIdent(ident) =>
          Pat.Macro(macroQuotedIdent(ident))
        case _: KwGiven =>
          next()
          Pat.Given(patternTyp(allowInfix = false, allowImmediateTypevars = false))
        case t =>
          onError(t)
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

  /**
   * These are default entry points into the pattern context sensitive methods: they are all
   * initiated from non-pattern context.
   */
  def typ() = outPattern.typ()
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
  def argumentPatterns(): List[Pat] = inParens {
    if (token.is[RightParen]) Nil
    else seqPatterns()
  }
  def xmlLiteralPattern(): Pat = syntaxError("XML literals are not supported", at = in.token)
  def patternTyp() = noSeq.patternTyp(allowInfix = true, allowImmediateTypevars = false)
  def patternTypeArgs() = noSeq.patternTypeArgs()

  /* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  private def privateModifier(): Mod = {
    accessModifier(Mod.Private(_))
  }

  private def protectedModifier(): Mod = {
    accessModifier(Mod.Protected(_))
  }

  private def accessModifier(mod: Ref => Mod): Mod = autoPos {
    next()
    if (!acceptOpt[LeftBracket]) mod(autoPos(Name.Anonymous()))
    else {
      val result = mod {
        if (token.is[KwThis]) {
          val name = autoPos { next(); Name.Anonymous() }
          copyPos(name)(Term.This(name))
        } else {
          termName().becomeOr[Ref](x => copyPos(x)(Name.Indeterminate(x.value)))
        }
      }
      accept[RightBracket]
      result
    }
  }

  private def modifier(isLocal: Boolean): Mod = {
    val mod = token match {
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
      case _ =>
        token.toString match {
          case soft.KwInline() => atCurPosNext(Mod.Inline())
          case soft.KwInfix() => atCurPosNext(Mod.Infix())
          case soft.KwOpen() if !isLocal => atCurPosNext(Mod.Open())
          case soft.KwOpaque() => atCurPosNext(Mod.Opaque())
          case soft.KwTransparent() => atCurPosNext(Mod.Transparent())
          case n =>
            val local = if (isLocal) "local " else ""
            syntaxError(s"${local}modifier expected but $n found", at = token)
        }
    }
    newLinesOpt()
    mod
  }

  def quasiquoteModifier(): Mod = entrypointModifier()

  def entrypointModifier(): Mod = {
    val mod = token match {
      case t: Unquote => unquote[Mod](t)
      case At() =>
        annots(skipNewLines = true) match {
          case annot :: Nil => annot
          case _ :: other :: _ =>
            syntaxError(s"end of file expected but ${token.name} found", at = other)
          case Nil => unreachable
        }
      case KwPrivate() => privateModifier()
      case KwProtected() => protectedModifier()
      case KwImplicit() => atCurPosNext(Mod.Implicit())
      case KwFinal() => atCurPosNext(Mod.Final())
      case KwSealed() => atCurPosNext(Mod.Sealed())
      case KwOverride() => atCurPosNext(Mod.Override())
      case KwCase() => atCurPosNext(Mod.Case())
      case KwAbstract() => atCurPosNext(Mod.Abstract())
      case Ident("+") => atCurPosNext(Mod.Covariant())
      case Ident("-") => atCurPosNext(Mod.Contravariant())
      case KwLazy() => atCurPosNext(Mod.Lazy())
      case KwVal() if !dialect.allowUnquotes => atCurPosNext(Mod.ValParam())
      case KwVar() if !dialect.allowUnquotes => atCurPosNext(Mod.VarParam())
      case Ident("valparam") if dialect.allowUnquotes => atCurPosNext(Mod.ValParam())
      case Ident("varparam") if dialect.allowUnquotes => atCurPosNext(Mod.VarParam())
      case _ =>
        token.toString match {
          case soft.KwOpen() => atCurPosNext(Mod.Open())
          case soft.KwTransparent() => atCurPosNext(Mod.Transparent())
          case soft.KwInline() => atCurPosNext(Mod.Inline())
          case soft.KwInfix() => atCurPosNext(Mod.Infix())
          case _ =>
            syntaxError(s"modifier expected but ${parser.token.name} found", at = parser.token)
        }
    }
    newLinesOpt()
    mod
  }

  def ctorModifiers(): Option[Mod] = token match {
    case t: Unquote if ahead(token.is[LeftParen]) => Some(unquote[Mod](t))
    case t: Ellipsis => Some(ellipsis[Mod](t, 1))
    case KwPrivate() => Some(privateModifier())
    case KwProtected() => Some(protectedModifier())
    case _ => None
  }

  def tparamModifiers(): Option[Mod] = {
    token match {
      case t: Unquote =>
        if (ahead(token.is[Ident] || token.is[Unquote])) Some(unquote[Mod](t)) else None
      case t: Ellipsis => Some(ellipsis[Mod](t, 1))
      case Ident("+") => Some(atCurPosNext(Mod.Covariant()))
      case Ident("-") => Some(atCurPosNext(Mod.Contravariant()))
      case _ => None
    }
  }

  def modifiersBuf(
      buf: ListBuffer[Mod],
      isLocal: Boolean = false,
      isParams: Boolean = false
  ): Unit = {
    val mods = buf.view.drop(buf.length)
    def appendMod(mod: Mod): Unit = {
      if (!mod.is[Mod.Quasi]) {
        if (mods.exists(_.productPrefix == mod.productPrefix)) {
          syntaxError("repeated modifier", at = mod)
        }
        if (mods.exists(_.isNakedAccessMod) && mod.isNakedAccessMod) {
          if (mod.is[Mod.Protected])
            rejectModWith[Mod.Private](mod, mods)
          else if (mod.is[Mod.Private])
            rejectModWith[Mod.Protected](mod, mods)
        }
        if (mods.exists(_.isQualifiedAccessMod) && mod.isQualifiedAccessMod) {
          syntaxError("duplicate private/protected qualifier", at = mod)
        }
      }
      buf += mod
    }
    // the only things that can come after $mod or $mods are either keywords or names; the former is easy,
    // but in the case of the latter, we need to take care to not hastily parse those names as modifiers
    def continueLoop = ahead(token match {
      case _: Colon | _: Equals | _: EOF | _: LeftBracket | _: Subtype | _: Supertype |
          _: Viewbound =>
        true
      case _ => false
    })
    @tailrec
    def loop: Unit = token match {
      case NonParamsModifier() if isParams =>
      case _: Unquote if continueLoop =>
      case _: Unquote | _: Ellipsis | Modifier() => appendMod(modifier(isLocal)); loop
      case _: LF if !isLocal => next(); loop
      case _ =>
    }
    loop
  }

  def annots(skipNewLines: Boolean, allowArgss: Boolean = true): List[Mod.Annot] =
    listBy[Mod.Annot](annotsBuf(_, skipNewLines, allowArgss))

  def annotsBuf[T >: Mod.Annot](
      annots: ListBuffer[T],
      skipNewLines: Boolean,
      allowArgss: Boolean = true
  ): Unit = {
    while (token match {
        case _: At =>
          next()
          annots += (token match {
            case t: Unquote => unquote[Mod.Annot](t)
            case _ => autoEndPos(StartPosPrev)(Mod.Annot(initInsideAnnotation(allowArgss)))
          })
          true
        case t: Ellipsis if ahead(token.is[At]) =>
          annots += ellipsis[Mod.Annot](t, 1, next())
          true
        case _ => false
      }) {
      if (skipNewLines) newLineOpt()
    }
  }

  /* -------- PARAMETERS ------------------------------------------- */

  @tailrec
  private def onlyLastParameterCanBeRepeated(params: List[Term.Param]): Unit = params match {
    case p :: tail if tail.nonEmpty =>
      if (!p.is[Term.Param.Quasi] && p.decltpe.exists(_.is[Type.Repeated]))
        syntaxError("*-parameter must come last", p)
      onlyLastParameterCanBeRepeated(tail)
    case _ =>
  }

  def termParamClauses(
      ownerIsType: Boolean,
      ownerIsCase: Boolean = false
  ): List[Term.ParamClause] = {
    if (!isAfterOptNewLine[LeftParen]) Nil
    else termParamClausesOnParen(ownerIsType, ownerIsCase)
  }

  private def termParamClausesOnParen(
      ownerIsType: Boolean,
      ownerIsCase: Boolean = false
  ): List[Term.ParamClause] = {
    var hadModImplicit = false
    def paramClause(first: Boolean) = autoPos(inParensOnOpenOr {
      def reduceParams(params: List[Term.Param], mod: Option[Mod.ParamsType] = None) =
        params.reduceWith { x =>
          onlyLastParameterCanBeRepeated(x)
          toParamClause(mod)(x)
        }
      def parseParams(mod: Option[Mod.ParamsType] = None) =
        reduceParams(commaSeparated(termParam(ownerIsCase && first, ownerIsType, mod = mod)), mod)
      token match {
        case t @ Ellipsis(2) =>
          reduceParams(List(ellipsis[Term.Param](t)))
        case _: KwImplicit =>
          hadModImplicit = true
          parseParams(Some(atCurPosNext(Mod.Implicit())))
        case soft.KwUsing() =>
          parseParams(Some(atCurPosNext(Mod.Using())))
        case _ => parseParams()
      }
    }(Term.ParamClause(Nil)))

    listBy[Term.ParamClause] { paramss =>
      paramss += paramClause(true)
      while (isAfterOptNewLine[LeftParen] && !hadModImplicit) {
        paramss += paramClause(false)
      }
    }
  }

  def paramType(): Type =
    autoPosOpt(token match {
      case RightArrow() =>
        val t = autoPos {
          next()
          Type.ByName(typ())
        }
        if (isStar && dialect.allowByNameRepeatedParameters) {
          next()
          Type.Repeated(t)
        } else {
          t
        }
      case _ =>
        val t = typ()
        if (!isStar) t
        else {
          next()
          Type.Repeated(t)
        }
    })

  def termParam(
      ownerIsCase: Boolean,
      ownerIsType: Boolean,
      mod: Option[Mod.ParamsType] = None
  ): Term.Param = autoPos {
    val mods = new ListBuffer[Mod]
    annotsBuf(mods, skipNewLines = false)
    rejectMod[Mod.Open](mods, "Open modifier only applied to classes")
    if (ownerIsType) {
      modifiersBuf(mods, isParams = true)
      rejectMod[Mod.Lazy](
        mods,
        "lazy modifier not allowed here. Use call-by-name parameters instead."
      )
      rejectMod[Mod.Sealed](mods, "`sealed' modifier can be used only for classes")
      if (!mods.has[Mod.Override])
        rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    } else {
      if (token.is[soft.KwInline] && tryAhead[Ident]) {
        mods += autoEndPos(StartPosPrev)(Mod.Inline())
      }
    }
    mod.foreach { mod =>
      val clazz = mod.getClass
      if (!mods.exists(_.getClass eq clazz)) mods += mod
    }

    val (isValParam, isVarParam) = (ownerIsType && token.is[KwVal], ownerIsType && token.is[KwVar])
    if (isValParam) {
      mods += atCurPosNext(Mod.ValParam())
    }
    if (isVarParam) {
      mods += atCurPosNext(Mod.VarParam())
    }
    def endParamQuasi = token.is[RightParen] || token.is[Comma]
    mods.headOption
      .collect {
        case q: Mod.Quasi if endParamQuasi => q.become[Term.Param]
      }
      .orElse(token match {
        case t: Ellipsis => Some(ellipsis[Term.Param](t, 1))
        case _ => None
      })
      .getOrElse {
        var anonymousUsing = false
        val name = if (mod.exists(_.is[Mod.Using]) && ahead(!token.is[Colon])) { // anonymous using
          anonymousUsing = true
          autoPos(Name.Anonymous())
        } else {
          termName().become[Name]
        }
        name match {
          case q: Quasi if endParamQuasi =>
            q.become[Term.Param]
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
                  } else if (!dialect.allowImplicitByNameParameters &&
                    mod.exists(_.is[Mod.Implicit]))
                    mayNotBeByName("implicit")
                }
                Some(tpt)
              }
            val default = if (acceptOpt[Equals]) Some(expr()) else None
            Term.Param(mods.toList, name, tpt, default)
        }
      }
  }

  def quasiquoteTermParam(): Term.Param = entrypointTermParam()

  def entrypointTermParam(): Term.Param =
    termParam(ownerIsCase = false, ownerIsType = true)

  private def emptyTypeParams: Type.ParamClause = autoPos(Type.ParamClause(Nil))

  private def typeParamClauseOpt(
      ownerIsType: Boolean,
      ctxBoundsAllowed: Boolean,
      allowUnderscore: Boolean = true
  ): Type.ParamClause =
    if (!isAfterOptNewLine[LeftBracket]) emptyTypeParams
    else typeParamClauseOnBracket(ownerIsType, ctxBoundsAllowed, allowUnderscore)

  private def typeParamClauseOnBracket(
      ownerIsType: Boolean,
      ctxBoundsAllowed: Boolean,
      allowUnderscore: Boolean = true
  ): Type.ParamClause = {
    autoPos(inBrackets(commaSeparated {
      typeParam(ownerIsType, ctxBoundsAllowed, allowUnderscore)
    }.reduceWith(Type.ParamClause.apply)))
  }

  def typeParam(
      ownerIsType: Boolean,
      ctxBoundsAllowed: Boolean,
      allowUnderscore: Boolean = true
  ): Type.Param = autoPos {
    val mods: List[Mod] = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = false)
      if (ownerIsType) tparamModifiers().foreach(buf += _)
    }
    def endTparamQuasi = token.is[RightBracket] || token.is[Comma]
    mods.headOption match {
      case Some(q: Mod.Quasi) if endTparamQuasi =>
        q.become[Type.Param]
      case _ =>
        val name = token match {
          case _: Ident => typeName()
          case t: Unquote => unquote[Name](t)
          case _: Underscore if allowUnderscore =>
            autoPos { next(); Name.Placeholder() }
          case _ =>
            if (allowUnderscore) syntaxError("identifier or `_' expected", at = token)
            else syntaxError("identifier expected", at = token)
        }
        name match {
          case q: Quasi if endTparamQuasi =>
            q.become[Type.Param]
          case _ =>
            val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
            val tbounds = typeBounds()
            val vbounds = new ListBuffer[Type]
            val cbounds = new ListBuffer[Type]
            if (ctxBoundsAllowed) {
              @inline def getBound(): Type = {
                next()
                token match {
                  case t: Ellipsis => ellipsis[Type](t, 1)
                  case _ => typ()
                }
              }
              if (token.is[Viewbound]) {
                if (!dialect.allowViewBounds) {
                  val msg = "Use an implicit parameter instead.\n" +
                    "Example: Instead of `def f[A <% Int](a: A)` " +
                    "use `def f[A](a: A)(implicit ev: A => Int)`."
                  syntaxError(s"View bounds are not supported. $msg", at = token)
                }
                do vbounds += getBound() while (token.is[Viewbound])
              }
              while (token.is[Colon]) cbounds += getBound()
            }
            Type.Param(mods, name, tparams, tbounds, vbounds.toList, cbounds.toList)
        }
    }
  }

  def quasiquoteTypeParam(): Type.Param = entrypointTypeParam()

  def entrypointTypeParam(): Type.Param = typeParam(ownerIsType = true, ctxBoundsAllowed = true)

  def typeBounds() =
    autoPos(Type.Bounds(bound[Supertype], bound[Subtype]))

  def bound[T: TokenClassifier]: Option[Type] =
    if (acceptOpt[T]) Some(typ()) else None

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
      case sid @ Term.Select(q: Quasi, name) =>
        copyPos(sid)(Term.Select(q.become[Term.Ref], name))
      case path => path
    }
    def dotselectors = Importer(sid, importees())
    def name(tn: Term.Name) = copyPos(tn)(Name.Indeterminate(tn.value))
    sid match {
      case Term.Select(sid: Term.Ref, tn: Term.Name) if sid.isStableId =>
        if (acceptOpt[Dot]) dotselectors
        else if (acceptOpt[soft.KwAs]) {
          Importer(sid, importeeRename(name(tn)) :: Nil)
        } else if (Wildcard.isStar(tn.tokens.head)) {
          Importer(sid, copyPos(tn)(Importee.Wildcard()) :: Nil)
        } else {
          Importer(sid, copyPos(tn)(Importee.Name(name(tn))) :: Nil)
        }
      case tn: Term.Name if acceptOpt[soft.KwAs] =>
        Importer(autoPos(Term.Anonymous()), importeeRename(name(tn)) :: Nil)
      case _ =>
        accept[Dot]
        dotselectors
    }
  }

  def quasiquoteImporter(): Importer = entrypointImporter()

  def entrypointImporter(): Importer = importer()

  def importees(): List[Importee] =
    if (!acceptOpt[LeftBrace]) {
      List(importWildcardOrName())
    } else {
      val importees = inBracesAfterOpen(commaSeparated(importee()))

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
            copyPos(importee.name)(Importee.GivenAll())
          case i => i
        }
      else importees
    }

  def importWildcardOrName(): Importee = autoPos {
    token match {
      case Wildcard() => next(); Importee.Wildcard()
      case _: KwGiven => next(); if (token.is[Ident]) Importee.Given(typ()) else Importee.GivenAll()
      case t: Unquote => Importee.Name(unquote[Name.Quasi](t))
      case _ => val name = termName(); Importee.Name(copyPos(name)(Name.Indeterminate(name.value)))
    }
  }

  def importeeRename(from: Name) = autoEndPos(from) {
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
          if (token.is[RightArrow] || token.is[soft.KwAs]) && tryAhead(Wildcard.unapply(token)) =>
        next()
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
        mods.find(mod => !mod.isAccessMod && !mod.is[Mod.Annot]) match {
          case Some(mod) =>
            syntaxError("Only access modifiers allowed on enum case", at = mod.pos)
          case None =>
            enumCaseDef(mods)
        }
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
      Term.EndMarker(termName())
    } else {
      val r = Term.EndMarker(atPos(token)(Term.Name(token.text)))
      next()
      r
    }
  }

  def patDefOrDcl(mods: List[Mod]): Stat = autoEndPos(mods) {
    val isMutable = token.is[KwVar]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    if (!mods.has[Mod.Override])
      rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    next()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2()).map {
      case name: Term.Name => copyPos(name)(Pat.Var(name))
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
  private def givenDecl(mods: List[Mod]): Stat = autoEndPos(mods) {
    accept[KwGiven]
    val anonymousName = autoPos(Name.Anonymous())

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
            termParamClauses(ownerIsType = false)
          else Nil
        if (acceptOpt[Colon]) {
          (name, tparams, uparamss)
        } else {
          in = forked
          (anonymousName, emptyTypeParams, Nil)
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
          (anonymousName, emptyTypeParams, Nil)
      }

    val decltype = if (token.is[LeftBrace]) refinement(None) else startModType()

    def parents() = {
      val parents = ListBuffer[Init](
        autoEndPos(decltype)(
          initRest(decltype, allowArgss = true, allowBraces = false, allowTypeSingleton = false)
        )
      )
      while (token.is[KwWith] && tryAhead[Ident]) parents += init()
      parents.toList
    }

    if (acceptOpt[Equals]) {
      Defn.GivenAlias(mods, sigName, sigTparams, sigUparamss, decltype, exprMaybeIndented())
    } else if (token.is[KwWith] || token.is[LeftParen]) {
      val inits = parents()
      val (slf, stats) = {
        if (inits.size > 1 && token.isNot[KwWith])
          syntaxError("expected 'with' <body>", at = token.pos)

        val needsBody = acceptOpt[KwWith]

        if (isAfterOptNewLine[LeftBrace]) {
          inBracesOnOpen(templateStatSeq())
        } else if (acceptOpt[Indentation.Indent]) {
          indentedAfterOpen(templateStatSeq())
        } else if (needsBody) {
          syntaxError("expected '{' or indentation", at = token.pos)
        } else (selfEmpty(), Nil)
      }
      val rhs = if (slf.decltpe.nonEmpty) {
        syntaxError("given cannot have a self type", at = slf.pos)
      } else {
        autoEndPos(decltype)(Template(List.empty, inits, slf, stats))
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
  def extensionGroupDecl(mods: List[Mod]): Defn.ExtensionGroup = autoEndPos(mods) {
    next() // 'extension'

    val tparams = typeParamClauseOpt(
      ownerIsType = false,
      ctxBoundsAllowed = true,
      allowUnderscore = dialect.allowTypeParamUnderscore
    )

    newLineOptWhenFollowedBy[LeftParen]

    val paramss = ListBuffer[Term.ParamClause]()

    def collectUparams(): Unit = {
      while (isAfterOptNewLine[LeftParen] && tryAhead[soft.KwUsing]) paramss += autoPrevPos {
        val mod = Some(atCurPos(Mod.Using()))
        inParensOnOpen(commaSeparated {
          termParam(ownerIsCase = false, ownerIsType = true, mod = mod)
        }).reduceWith(toParamClause(mod))
      }
    }

    collectUparams()

    paramss += autoPos(inParens(List(token match {
      case t @ Ellipsis(2) => ellipsis[Term.Param](t)
      case _ => termParam(ownerIsCase = false, ownerIsType = false)
    })).reduceWith(toParamClause(None)))

    collectUparams()
    newLinesOpt()

    val body: Stat =
      if (acceptOpt[LeftBrace]) {
        autoEndPos(StartPosPrev)(Term.Block(inBracesAfterOpen(statSeq(templateStat()))))
      } else if (in.observeIndented()) {
        val block = autoPos(Term.Block(indentedOnOpen(statSeq(templateStat()))))
        block.stats match {
          case stat :: Nil => stat
          case _ => block
        }
      } else if (token.is[DefIntro]) {
        nonLocalDefOrDcl()
      } else {
        syntaxError("Extension without extension method", token)
      }
    Defn.ExtensionGroup(tparams, paramss.toList, body)
  }

  def funDefRest(mods: List[Mod]): Stat = autoEndPos(mods) {
    accept[KwDef]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    rejectMod[Mod.Open](mods, Messages.InvalidOpen)
    if (!mods.has[Mod.Override])
      rejectMod[Mod.Abstract](mods, Messages.InvalidAbstract)
    val name = termName()
    def warnProcedureDeprecation = {
      val hint = s"Convert procedure `$name` to method by adding `: Unit =`."
      if (dialect.allowProcedureSyntax)
        deprecationWarning(s"Procedure syntax is deprecated. $hint", at = name)
      else
        syntaxError(s"Procedure syntax is not supported. $hint", at = name)
    }
    val tparams = typeParamClauseOpt(ownerIsType = false, ctxBoundsAllowed = true)
    val paramss = termParamClauses(ownerIsType = false)

    val restype = ReturnTypeContext.within(typedOpt())
    if (restype.isEmpty && isAfterOptNewLine[LeftBrace]) {
      warnProcedureDeprecation
      Defn.Def(
        mods,
        name,
        tparams,
        paramss,
        Some(autoPos(Type.Name("Unit"))),
        expr()
      )
    } else if (token.is[StatSep] || token.is[RightBrace] || token.is[Indentation.Outdent]) {
      val decltype = restype.getOrElse {
        warnProcedureDeprecation
        autoPos(Type.Name("Unit"))
      }
      Decl.Def(mods, name, tparams, paramss, decltype)
    } else {
      accept[Equals]
      val isMacro = acceptOpt[KwMacro]
      val rhs = exprMaybeIndented()
      if (isMacro) Defn.Macro(mods, name, tparams, paramss, restype, rhs)
      else Defn.Def(mods, name, tparams, paramss, restype, rhs)
    }
  }

  def typeDefOrDcl(mods: List[Mod]): Member.Type with Stat = autoEndPos(mods) {
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
      if (acceptOpt[Equals]) {
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
  def topLevelTmplDef: Stat = tmplDef(listBy[Mod] { buf =>
    annotsBuf(buf, skipNewLines = true)
    modifiersBuf(buf)
  })

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
      case KwCase() if tryAhead[KwClass] =>
        classDef(mods :+ atPos(in.prevTokenPos)(Mod.Case()))
      case KwObject() =>
        objectDef(mods)
      case KwCase() if tryAhead[KwObject] =>
        objectDef(mods :+ atPos(in.prevTokenPos)(Mod.Case()))
      case Token.At() =>
        syntaxError("Annotations must precede keyword modifiers", at = token)
      case DefIntro() if dialect.allowToplevelStatements =>
        defOrDclOrSecondaryCtor(mods)
      case _ =>
        syntaxError(s"expected start of definition", at = token)
    }
  }

  def traitDef(mods: List[Mod]): Defn.Trait = autoEndPos(mods) {
    val assumedAbstract = atCurPos(Mod.Abstract())
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

  def classDef(mods: List[Mod]): Defn.Class = autoEndPos(mods) {
    accept[KwClass]
    rejectMod[Mod.Override](mods, Messages.InvalidOverrideClass)

    val className = typeName()
    def culprit = Some(s"class $className")
    rejectModCombination[Mod.Final, Mod.Sealed](mods, culprit)
    rejectModCombination[Mod.Open, Mod.Final](mods, culprit)
    rejectModCombination[Mod.Open, Mod.Sealed](mods, culprit)
    rejectModCombination[Mod.Case, Mod.Implicit](mods, culprit)
    val typeParams = typeParamClauseOpt(
      ownerIsType = true,
      ctxBoundsAllowed = true,
      allowUnderscore = dialect.allowTypeParamUnderscore
    )
    val ctor = primaryCtor(if (mods.has[Mod.Case]) OwnedByCaseClass else OwnedByClass)

    if (!dialect.allowCaseClassWithoutParameterList &&
      mods.has[Mod.Case] && ctor.paramClauses.isEmpty) {
      syntaxError(
        s"case classes must have a parameter list; try 'case class $className()' or 'case object $className'",
        at = token
      )
    }

    val tmpl = templateOpt(OwnedByClass)
    Defn.Class(mods, className, typeParams, ctor, tmpl)
  }

  // EnumDef           ::=  id ClassConstr InheritClauses EnumBody
  def enumDef(mods: List[Mod]): Defn.Enum = autoEndPos(mods) {
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
  def enumCaseDef(mods: List[Mod]): Stat = autoEndPos(mods) {
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
    val inits = if (acceptOpt[KwExtends]) {
      templateParents(afterExtend = true)
    } else { List() }
    Defn.EnumCase(mods, name, tparams, ctor, inits)
  }

  def objectDef(mods: List[Mod]): Defn.Object = autoEndPos(mods) {
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
      val mods = listBy[Mod] { buf =>
        annotsBuf(buf, skipNewLines = false, allowArgss = false)
        ctorModifiers().foreach(buf += _)
      }
      val name = autoPos(Name.Anonymous())
      val paramss = termParamClauses(ownerIsType = true, owner == OwnedByCaseClass)
      Ctor.Primary(mods, name, paramss)
    } else if (owner.isTrait) {
      Ctor.Primary(Nil, autoPos(Name.Anonymous()), Seq.empty)
    } else {
      unreachable(debug(owner))
    }
  }

  def secondaryCtor(mods: List[Mod]): Ctor.Secondary = autoEndPos(mods) {
    accept[KwDef]
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    val name = atCurPos(Name.Anonymous())
    accept[KwThis]
    if (token.isNot[LeftParen]) {
      syntaxError("auxiliary constructor needs non-implicit parameter list", at = token.pos)
    } else {
      val paramss = termParamClauses(ownerIsType = true)
      secondaryCtorRest(mods, name, paramss)
    }
  }

  def quasiquoteCtor(): Ctor = autoPos {
    val mods = listBy[Mod] { buf =>
      annotsBuf(buf, skipNewLines = true)
      modifiersBuf(buf)
    }
    rejectMod[Mod.Sealed](mods, Messages.InvalidSealed)
    accept[KwDef]
    val name = atCurPos(Name.Anonymous())
    accept[KwThis]
    val paramss = termParamClauses(ownerIsType = true)
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
      paramss: Seq[Term.ParamClause]
  ): Ctor.Secondary = {
    val hasLeftBrace = isAfterOptNewLine[LeftBrace] || { accept[Equals]; token.is[LeftBrace] }
    val (init, stats) =
      if (hasLeftBrace) inBracesOnOpen(constrInternal())
      else if (acceptOpt[Indentation.Indent]) indentedAfterOpen(constrInternal())
      else (initInsideConstructor(), Nil)
    Ctor.Secondary(mods, name, paramss, init, stats)
  }

  def constrInternal(): (Init, List[Stat]) = {
    val init = initInsideConstructor()
    val stats = if (acceptOpt[StatSep]) blockStatSeq() else Nil
    (init, stats)
  }

  def initInsideConstructor(): Init = {
    def tpe = {
      val t = autoPos(Term.This(autoPos { accept[KwThis]; Name.Anonymous() }))
      copyPos(t)(Type.Singleton(t))
    }
    initRest(tpe, allowArgss = true, allowBraces = true, allowTypeSingleton = true)
  }

  def initInsideAnnotation(allowArgss: Boolean): Init = {
    initRest(
      exprSimpleType(),
      allowArgss = allowArgss,
      allowBraces = false,
      allowTypeSingleton = true
    )
  }

  def initInsideTemplate(): Init = {
    initRest(startModType(), allowArgss = true, allowBraces = false, allowTypeSingleton = false)
  }

  def quasiquoteInit(): Init = entrypointInit()

  def entrypointInit(): Init = {
    token match {
      case KwThis() => initInsideConstructor()
      case _ => initInsideTemplate()
    }
  }

  def initRest(
      typeParser: => Type,
      allowArgss: Boolean,
      allowBraces: Boolean,
      allowTypeSingleton: Boolean
  ): Init = autoPosOpt {
    def isPendingArglist = token.is[LeftParen] || (token.is[LeftBrace] && allowBraces)
    def newlineOpt() = {
      if (dialect.allowSignificantIndentation)
        newLineOptWhenFollowedBySignificantIndentationAnd(x => x.is[LeftBrace] || x.is[LeftParen])
      else if (allowBraces) newLineOptWhenFollowedBy[LeftBrace]
    }
    token match {
      case t: Unquote if !ahead(isPendingArglist) =>
        unquote[Init](t)
      case _ =>
        val tpe = typeParser
        if (!allowTypeSingleton && tpe.is[Type.Singleton])
          syntaxError(s"class type required but $tpe found", at = tpe.pos)
        val name = autoPos(Name.Anonymous())
        val argss = listBy[Term.ArgClause] { argss =>
          @inline def body() = {
            argss += getArgClause()
            newlineOpt()
          }
          newlineOpt()
          if (allowArgss)
            while (isPendingArglist) body()
          else if (isPendingArglist) body()
        }
        Init(tpe, name, argss)
    }
  }

  /* ---------- SELFS --------------------------------------------- */

  def quasiquoteSelf(): Self = entrypointSelf()

  def entrypointSelf(): Self = self()

  private def selfEmpty(): Self = {
    val name = autoPos(Name.Anonymous())
    copyPos(name)(Self(name, None))
  }

  private def self(): Self =
    selfOpt().getOrElse(syntaxError("expected identifier, `this' or unquote", at = token))

  private def selfOpt(): Option[Self] = {
    val name = token match {
      case Ident(_) =>
        termName()
      case Underscore() | KwThis() =>
        autoPos { next(); Name.Anonymous() }
      case t: Unquote =>
        if (ahead(token.is[Colon])) unquote[Name.Quasi](t)
        else return Some(unquote[Self.Quasi](t))
      case _ =>
        return None
    }
    val decltpe = token match {
      case Colon() =>
        next()
        Some(startInfixType())
      case _ =>
        None
    }
    Some(autoEndPos(name)(Self(name, decltpe)))
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
      case t: Ellipsis => ellipsis[Init](t, 1)
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
    if (isAfterOptNewLine[soft.KwDerives]) {
      next()
      val deriving = ListBuffer[Type]()
      do token match {
        case t: Ellipsis => deriving += ellipsis[Type](t, 1)
        case _ => deriving += startModType()
      } while (acceptOpt[Comma])
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
    if (isAfterOptNewLine[LeftBrace]) {
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
    if (token.is[Unquote] && ahead(token match {
        case _: Dot | _: Hash | _: At | _: Ellipsis | _: LeftParen | _: LeftBracket | _: LeftBrace |
            _: KwWith =>
          false
        case _ => true
      })) {
      unquote[Template](token.asInstanceOf[Unquote])
    } else if (acceptOpt[KwExtends] || (owner.isTrait && acceptOpt[Subtype])) {
      template(
        afterExtend = true,
        enumCaseAllowed = owner.isEnum,
        secondaryConstructorAllowed = owner.isClass || owner.isEnum
      )
    } else {
      if (isAfterOptNewLine[soft.KwDerives]) {
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
    if (isAfterOptNewLine[LeftBrace]) {
      templateBody(enumCaseAllowed, secondaryConstructorAllowed)
    } else if (isColonEol(token)) {
      accept[Colon]

      val nextIndented =
        if (enumCaseAllowed)
          in.observeIndentedEnum()
        else
          in.observeIndented()

      if (nextIndented)
        indentedOnOpen(templateStatSeq(enumCaseAllowed, secondaryConstructorAllowed))
      else if (token.is[EndMarkerIntro] && !enumCaseAllowed)
        (selfEmpty(), Nil)
      else
        syntaxError("expected template body", token)
    } else {
      if (token.is[LeftParen]) {
        if (parenMeansSyntaxError) {
          val what = if (dialect.allowTraitParameters) "objects" else "traits or objects"
          syntaxError(s"$what may not have parameters", at = token)
        } else syntaxError("unexpected opening parenthesis", at = token)
      }
      (selfEmpty(), Nil)
    }
  }

  @tailrec
  private def refinement(innerType: Option[Type]): Type.Refine = {
    val refineType = autoEndPos(innerType)(Type.Refine(innerType, inBraces(refineStatSeq())))
    if (isAfterOptNewLine[LeftBrace]) refinement(innerType = Some(refineType))
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
    case t: Ellipsis => ellipsis[Stat](t, 1)
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
      if (!acceptOpt[StatSep]) syntaxErrorExpected[EOF]
      skipStatementSeparators()
    }
    val maybeStat = consumeStat.lift(token)
    val stat = maybeStat.getOrElse(syntaxError("unexpected start of statement", at = token))
    skipStatementSeparators()
    stat
  }

  def stat(body: => Stat): Stat = {
    body.become[Stat]
  }

  def statSeq[T <: Tree: AstInfo](
      statpf: PartialFunction[Token, T],
      errorMsg: String = "illegal start of definition"
  ): List[T] = {
    listBy[T](statSeqBuf(_, statpf, errorMsg))
  }

  def statSeqBuf[T <: Tree: AstInfo](
      stats: ListBuffer[T],
      statpf: PartialFunction[Token, T],
      errorMsg: String = "illegal start of definition"
  ): Unit = {
    val isIndented = acceptOpt[Indentation.Indent]
    val statpfAdd = statpf.runWith(stats += _)

    while (!token.is[StatSeqEnd]) {
      if (statpfAdd(token)) acceptStatSepOpt()
      else if (token.is[StatSep]) acceptStatSep()
      else syntaxError(errorMsg + s" ${token.name}", at = token)
    }
    if (isIndented) accept[Indentation.Outdent]
  }

  val topStat: PartialFunction[Token, Stat] = {
    case t: Ellipsis =>
      ellipsis[Stat](t, 1)
    case t: Unquote =>
      unquote[Stat](t)
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
    val forked = in.fork
    val selfTree: Self = selfOpt() match {
      case Some(selfTree) if acceptOpt[RightArrow] =>
        in.undoIndent()
        selfTree
      case _ =>
        in = forked
        selfEmpty()
    }
    val stats = listBy[Stat] { buf =>
      statSeqBuf(buf, templateStat(enumCaseAllowed, secondaryConstructorAllowed))
    }
    (selfTree, stats)
  }

  def templateStat(
      enumCaseAllowed: Boolean = false,
      secondaryConstructorAllowed: Boolean = false
  ): PartialFunction[Token, Stat] = {
    case KwImport() =>
      importStmt()
    case KwExport() =>
      exportStmt()
    case DefIntro() =>
      nonLocalDefOrDcl(enumCaseAllowed, secondaryConstructorAllowed)
    case t: Unquote =>
      unquote[Stat](t)
    case t: Ellipsis =>
      ellipsis[Stat](t, 1)
    case EndMarkerIntro() =>
      endMarker()
    case ExprIntro() =>
      expr(location = TemplateStat, allowRepeated = false)
  }

  def refineStatSeq(): List[Stat] = listBy[Stat] { stats =>
    while (!token.is[StatSeqEnd]) {
      refineStat().foreach(stats += _)
      if (token.isNot[RightBrace] && token.isNot[Indentation.Outdent]) acceptStatSep()
    }
  }

  def refineStat(): Option[Stat] = token match {
    case t: Ellipsis => Some(ellipsis[Stat](t, 1))
    case DclIntro() =>
      val stat = defOrDclOrSecondaryCtor(Nil)
      if (stat.isRefineStat) Some(stat)
      else syntaxError("is not a valid refinement declaration", at = stat)
    case StatSep() => None
    case _ if ReturnTypeContext.isInside() =>
      syntaxError(
        "illegal start of declaration (possible cause: missing `=' in front of current method body)",
        at = token
      )
    case _ => syntaxError("illegal start of declaration", at = token)
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
      })
      defOrDclOrSecondaryCtor(mods)
    else
      tmplDef(mods) match {
        case stat: Decl.Type if dialect.allowTypeInBlock => stat
        case stat: Decl.Type => syntaxError("is not a valid block statement", at = stat)
        case stat if stat.isBlockStat => stat
        case other => syntaxError("is not a valid block statement", at = other)
      }
  }

  def blockStatSeq(allowRepeated: Boolean = false): List[Stat] = listBy[Stat] { stats =>
    while (!token.is[CaseDefEnd] && !in.observeOutdented()) token match {
      case _: KwExport =>
        stats += exportStmt()
        acceptStatSepOpt()
      case _: KwImport =>
        stats += importStmt()
        acceptStatSepOpt()
      case _: KwImplicit =>
        val implicitPos = in.tokenPos
        next()
        if (token.is[Ident] && token.isNot[SoftModifier]) stats += implicitClosure(BlockStat)
        else stats += localDef(Some(atPos(implicitPos)(Mod.Implicit())))
        if (!token.is[CaseDefEnd]) acceptStatSepOpt()
      case t @ DefIntro() if !t.is[NonlocalModifier] =>
        stats += localDef(None)
        if (!token.is[CaseDefEnd]) acceptStatSepOpt()
      case ExprIntro() =>
        stats += stat(expr(location = BlockStat, allowRepeated = allowRepeated))
        if (!token.is[CaseDefEnd]) acceptStatSep()
      case StatSep() =>
        next()
      case t: Ellipsis =>
        stats += ellipsis[Stat](t, 1)
      case EndMarkerIntro() =>
        stats += endMarker()
      case _ =>
        syntaxError("illegal start of statement", at = token)
    }
    if (allowRepeated && stats.length > 1)
      stats.foreach {
        case t: Term.Repeated => syntaxError("repeated argument not allowed here", at = t)
        case _ =>
      }
  }

  def packageOrPackageObjectDef(statpf: PartialFunction[Token, Stat]): Stat = autoPos {
    accept[KwPackage]
    if (acceptOpt[KwObject])
      Pkg.Object(Nil, termName(), templateOpt(OwnedByObject))
    else {
      def packageBody =
        if (isColonEol(token)) {
          next()
          in.observeIndented()
          indented(statSeq(statpf))
        } else {
          inBracesOrNil(statSeq(statpf))
        }
      Pkg(qualId(), packageBody)
    }
  }

  def source(): Source = autoPos {
    batchSource(if (dialect.allowToplevelTerms) consumeStat else topStat)
  }

  def quasiquoteSource(): Source = entrypointSource()

  def entrypointSource(): Source = source()

  def batchSource(statpf: PartialFunction[Token, Stat] = topStat): Source = autoPos {
    val buf = new ListBuffer[Stat]
    @tailrec
    def bracelessPackageStats(f: List[Stat] => List[Stat]): List[Stat] = token match {
      case _: EOF => f(buf.toList)
      case StatSep() =>
        next()
        bracelessPackageStats(f)
      case _: KwPackage if tryAhead(!token.is[KwObject]) =>
        val startPos = in.prevTokenPos
        val qid = qualId()
        def inPackage(stats: => List[Stat]) = {
          buf += autoEndPos(startPos)(Pkg(qid, stats))
          acceptStatSepOpt()
        }
        token match {
          case _: LeftBrace =>
            inPackage(inBracesOnOpen(statSeq(statpf)))
            bracelessPackageStats(f)
          case t if isColonEol(t) =>
            next()
            in.observeIndented()
            inPackage(indented(statSeq(statpf)))
            bracelessPackageStats(f)
          case _ => bracelessPackageStats(x => f(List(autoEndPos(startPos)(Pkg(qid, x)))))
        }
      case _: LeftBrace =>
        inBracesOnOpen(statSeqBuf(buf, statpf))
        f(buf.toList)
      case _ =>
        statSeqBuf(buf, statpf)
        f(buf.toList)
    }
    val bracelessPackageStatsOpt: Option[List[Stat]] = if (token.is[KwPackage]) {
      val forked = in.fork
      val startPos = in.tokenPos
      next()
      val refOpt = if (!token.is[KwObject]) {
        val ref = qualId()
        newLineOpt()
        if (token.is[LeftBrace] || isColonEol(token)) None else Some(ref)
      } else None
      if (refOpt.isEmpty) in = forked
      refOpt.map(ref => List(autoEndPos(startPos)(Pkg(ref, bracelessPackageStats(identity)))))
    } else None
    Source(bracelessPackageStatsOpt.getOrElse { statSeqBuf(buf, statpf); buf.toList })
  }
}

object ScalametaParser {

  private def dropTrivialBlock(term: Term): Term =
    term match {
      case b: Term.Block => dropOuterBlock(b)
      case _ => term
    }

  private def dropOuterBlock(term: Term.Block): Term =
    term.stats match {
      case (stat: Term) :: Nil => stat
      case _ => term
    }

  @inline
  private def maybeAnonymousFunctionUnlessPostfix(expr: Term)(location: Location): Term =
    if (location == Location.PostfixStat) expr else maybeAnonymousFunction(expr)

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

    def become[A >: T <: Tree: AstInfo]: A =
      tree match {
        case q: Quasi => q.become[A]
        case _ => tree
      }

    def becomeOr[A <: Tree: AstInfo](f: T => A): A = {
      tree match {
        case q: Quasi => q.become[A]
        case _ => f(tree)
      }
    }

  }

  private def copyPos[T <: Tree](tree: Tree)(body: => T): T = {
    body.withOrigin(tree.origin)
  }

  @inline
  private def quasi[T <: Tree](rank: Int, tree: Tree)(implicit astInfo: AstInfo[T]): T with Quasi =
    astInfo.quasi(rank, tree)

  private def reellipsis[T <: Tree: AstInfo](q: Quasi, rank: Int): T = {
    val became = q.become[T]
    if (became.rank != rank) copyPos(became)(quasi[T](rank, became.tree)) else became
  }

  private implicit class XtensionList[A <: Tree](private val list: List[A]) extends AnyVal {
    def reduceWith[B <: Tree: AstInfo](f: List[A] => B, reduceRank: Int = 1): B =
      ScalametaParser.reduceAs[A, B, B](list, f, reduceRank)
  }

  private def reduceAs[A <: Tree, B <: Tree, C <: B: AstInfo](
      list: List[A],
      f: List[A] => B,
      reduceRank: Int = 1
  ): B = list match {
    case (t: Quasi) :: Nil if t.rank >= reduceRank => reellipsis[C](t, t.rank - reduceRank)
    case v => f(v)
  }

}

class Location private (val value: Int) extends AnyVal
object Location {
  val NoStat = new Location(0)
  val BlockStat = new Location(1)
  val TemplateStat = new Location(2)
  val PostfixStat = new Location(3)
}

object InfixMode extends Enumeration {
  val FirstOp, LeftOp, RightOp = Value
}
