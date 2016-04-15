package scala.meta
package internal
package parsers

import scala.language.implicitConversions
import scala.compat.Platform.EOL
import scala.reflect.{ClassTag, classTag}
import scala.runtime.ScalaRunTime
import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import scala.annotation.tailrec
import scala.{Seq => _}
import scala.collection.immutable._
import scala.meta.internal.parsers.Location._
import scala.meta.internal.ast._
import scala.meta.internal.ast.Helpers._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokens.Token._
import scala.meta.internal.tokens._
import scala.meta.internal.ast.AstInfo
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.prettyprinters._
import org.scalameta._
import org.scalameta.invariants._

private[meta] class ScalametaParser(val input: Input)(implicit val dialect: Dialect) { parser =>
  // implementation restrictions wrt various dialect properties
  require(Set("@", ":").contains(dialect.bindToSeqWildcardDesignator))
  def inQuasiquote = dialect.isInstanceOf[scala.meta.dialects.Quasiquote]

/* ------------- PARSER ENTRY POINTS -------------------------------------------- */

  def parseRule[T <: Tree](rule: this.type => T): T = {
    // NOTE: can't require in.tokenPos to be at -1, because TokIterator auto-rewinds when created
    // require(in.tokenPos == -1 && debug(in.tokenPos))
    val start = 0
    accept[BOF]
    val t = rule(this)
    // NOTE: can't have in.prevTokenPos here
    // because we need to subsume all the trailing trivia
    val end = in.tokenPos
    accept[EOF]
    atPos(start, end)(t)
  }

  // Entry points for Parse[T]
  // Used for quasiquotes as well as for ad-hoc parsing
  // parseStat is used to implement q"..." and surprisingly can't be reduced to anything that's already in the parser,
  // because it needs to support a wide range of constructs, from expressions to top-level definitions.
  // Note the expr(Local) part, which means that we're going to parse lambda expressions in the mode that
  // precludes ambiguities with self-type annotations.
  def parseStat(): Stat = parseRule(parser => parser.statSeq[Stat] {
    case token if token.is[`import`] => importStmt()
    case token if token.is[`package `] => packageOrPackageObjectDef()
    case token if token.is[TokenClass.DefIntro] || token.is[Ellipsis] => nonLocalDefOrDcl()
    case token if token.is[TokenClass.ExprIntro] => expr(Local)
  } match {
    case Nil => reporter.syntaxError("unexpected end of input", at = token)
    case stat :: Nil => stat
    case stats if stats.forall(_.isBlockStat) => Term.Block(stats)
    case stats if stats.forall(_.isTopLevelStat) => Source(stats)
    case other => reporter.syntaxError("these statements can't be mixed together", at = parserTokens.head)
  })
  def parseQuasiquoteStat(): Stat = parseStat() // NOTE: special treatment for q"super" and likes is implemented directly in `path`
  def parseQuasiquoteCtor(): Ctor = parseRule(_.quasiquoteCtor())
  def parseTerm(): Term = parseRule(_.expr())
  def parseTermArg(): Term.Arg = parseRule(_.argumentExpr())
  def parseTermParam(): Term.Param = parseRule(_.param(ownerIsCase = false, ownerIsType = true, isImplicit = false))
  def parseType(): Type = parseRule(_.typ())
  def parseTypeArg(): Type.Arg = parseRule(_.paramType())
  def parseTypeParam(): Type.Param = parseRule(_.typeParam(ownerIsType = true, ctxBoundsAllowed = true))
  def parsePat(): Pat = parseRule(_.pattern()).require[Pat]
  def parseQuasiquotePat(): Pat = parseRule(_.quasiquotePattern()).require[Pat]
  def parsePatArg(): Pat.Arg = parseRule(_.argumentPattern())
  def parseQuasiquotePatArg(): Pat.Arg = parseRule(_.quasiquotePatternArg())
  def parsePatType(): Pat.Type = parseRule(_.patternTyp())
  def parseQuasiquotePatType(): Pat.Type = parseRule(_.quasiquotePatternTyp())
  def parseCase(): Case = parseRule{parser => parser.accept[`case`]; parser.caseClause()}
  def parseCtorCall(): Ctor.Call = parseRule(_.constructorCall(typ(), allowArgss = true))
  def parseTemplate(): Template = parseRule(_.template())
  def parseMod(): Mod = {
    implicit class XtensionParser(parser: this.type) {
      def annot() = parser.annots(skipNewLines = false) match {
        case annot :: Nil => annot
        case annot :: other :: _ => parser.reporter.syntaxError(s"end of file expected but ${token.name} found", at = other)
        case Nil => unreachable(debug(input, dialect))
      }
    }
    def fail() = parser.reporter.syntaxError(s"modifier expected but ${parser.token.name} found", at = parser.token)
    parseRule(parser => parser.autoPos(parser.token match {
      case _: Unquote                                 => unquote[Mod.Quasi]
      case _: `@`                                     => parser.annot()
      case _: `private`                               => parser.accessModifier()
      case _: `protected`                             => parser.accessModifier()
      case _: `implicit`                              => parser.next(); Mod.Implicit()
      case _: `final`                                 => parser.next(); Mod.Final()
      case _: `sealed`                                => parser.next(); Mod.Sealed()
      case _: `override`                              => parser.next(); Mod.Override()
      case _: `case`                                  => parser.next(); Mod.Case()
      case _: `abstract`                              => parser.next(); Mod.Abstract()
      case _ if isIdentOf("+")                        => parser.next(); Mod.Covariant()
      case _ if isIdentOf("-")                        => parser.next(); Mod.Contravariant()
      case _: `lazy`                                  => parser.next(); Mod.Lazy()
      case _: `val` if !inQuasiquote                  => parser.next(); Mod.ValParam()
      case _: `var` if !inQuasiquote                  => parser.next(); Mod.VarParam()
      case _ if isIdentOf("valparam") && inQuasiquote => parser.next(); Mod.ValParam()
      case _ if isIdentOf("varparam") && inQuasiquote => parser.next(); Mod.VarParam()
      case _                                          => fail()
    }))
  }
  def parseQuasiquoteMod(): Mod = parseMod() // NOTE: special treatment for mod"valparam" and likes is implemented directly in `parseMod`
  def parseEnumerator(): Enumerator = {
    parseRule(_.enumerator(isFirst = false, allowNestedIf = false) match {
      case enumerator :: Nil => enumerator
      case other => unreachable(debug(input, dialect, other))
    })
  }
  def parseImporter(): Importer = parseRule(_.importer())
  def parseImportee(): Importee = parseRule(_.importee())
  def parseSource(): Source = parseRule(_.compilationUnit())

/* ------------- PARSER-SPECIFIC TOKEN CLASSIFIERS -------------------------------------------- */

  abstract class TokenClass(fn: Token => Boolean) { def contains(token: Token): Boolean = fn(token) }
  object TokenClass {
    private implicit class XtensionTokenClass(token: Token) {
      def isCaseClassOrObject = token.isInstanceOf[`case`] && (token.next.is[`class `] || token.next.is[`object`])
    }
    class TypeIntro extends TokenClass(token => {
      token.isInstanceOf[Ident] || token.isInstanceOf[`super`] || token.isInstanceOf[`this`] ||
      token.isInstanceOf[`(`] || token.isInstanceOf[`@`] || token.isInstanceOf[`_ `] ||
      token.isInstanceOf[Unquote]
    })
    class ExprIntro extends TokenClass(token => {
      token.isInstanceOf[Ident] || token.isInstanceOf[Literal] ||
      token.isInstanceOf[Interpolation.Id] || token.isInstanceOf[Xml.Start] ||
      token.isInstanceOf[`do`] || token.isInstanceOf[`for`] || token.isInstanceOf[`if`] ||
      token.isInstanceOf[`new`] || token.isInstanceOf[`return`] || token.isInstanceOf[`super`] ||
      token.isInstanceOf[`this`] || token.isInstanceOf[`throw`] || token.isInstanceOf[`try`] || token.isInstanceOf[`while`] ||
      token.isInstanceOf[`(`] || token.isInstanceOf[`{`] || token.isInstanceOf[`_ `] ||
      token.isInstanceOf[`Unquote`]
    })
    class CaseIntro extends TokenClass(token => {
      token.isInstanceOf[`case`] && !token.isCaseClassOrObject
    })
    class DefIntro extends TokenClass(token => {
      token.isInstanceOf[Modifier] || token.isInstanceOf[`@`] ||
      token.is[TemplateIntro] || token.is[DclIntro] ||
      (token.isInstanceOf[Unquote] && token.next.is[DefIntro]) ||
      (token.isInstanceOf[`case`] && token.isCaseClassOrObject)
    })
    class TemplateIntro extends TokenClass(token => {
      token.isInstanceOf[Modifier] || token.isInstanceOf[`@`] ||
      token.isInstanceOf[`class `] || token.isInstanceOf[`object`] || token.isInstanceOf[`trait`] ||
      (token.isInstanceOf[Unquote] && token.next.is[TemplateIntro]) ||
      (token.isInstanceOf[`case`] && token.isCaseClassOrObject)
    })
    class DclIntro extends TokenClass(token => {
      token.isInstanceOf[`def`] || token.isInstanceOf[`type`] ||
      token.isInstanceOf[`val`] || token.isInstanceOf[`var`] ||
      (token.isInstanceOf[Unquote] && token.next.is[DclIntro])
    })
    class NonlocalModifier extends TokenClass(token => {
      token.isInstanceOf[`private`] || token.isInstanceOf[`protected`] || token.isInstanceOf[`override`]
    })
    class LocalModifier extends TokenClass(token => {
      token.is[Modifier] && !token.is[NonlocalModifier]
    })
    class StatSeqEnd extends TokenClass(token => {
      token.isInstanceOf[`}`] || token.isInstanceOf[EOF]
    })
    class CaseDefEnd extends TokenClass(token => {
      token.isInstanceOf[`}`] || token.isInstanceOf[EOF] ||
      (token.isInstanceOf[`case`] && !token.isCaseClassOrObject) ||
      (token.is[Ellipsis] && token.next.is[`case`])
    })
    class CantStartStat extends TokenClass(token => {
      token.isInstanceOf[`catch`] || token.isInstanceOf[`else`] || token.isInstanceOf[`extends`] ||
      token.isInstanceOf[`finally`] || token.isInstanceOf[`forSome`] || token.isInstanceOf[`match`] ||
      token.isInstanceOf[`with`] || token.isInstanceOf[`yield`] ||
      token.isInstanceOf[`)`] || token.isInstanceOf[`[`] || token.isInstanceOf[`]`] || token.isInstanceOf[`}`] ||
      token.isInstanceOf[`,`] || token.isInstanceOf[`:`] || token.isInstanceOf[`.`] || token.isInstanceOf[`=`] ||
      token.isInstanceOf[`;`] || token.isInstanceOf[`#`] || token.isInstanceOf[`=>`] || token.isInstanceOf[`<-`] ||
      token.isInstanceOf[`<:`] || token.isInstanceOf[`>:`] || token.isInstanceOf[`<%`] ||
      token.isInstanceOf[`\n`] || token.isInstanceOf[`\n\n`] || token.isInstanceOf[EOF]
    })
    class CanEndStat extends TokenClass(token => {
      token.isInstanceOf[Ident] || token.isInstanceOf[Literal] ||
      token.isInstanceOf[Interpolation.End] || token.isInstanceOf[Xml.End] ||
      token.isInstanceOf[`return`] || token.isInstanceOf[`this`] || token.isInstanceOf[`type`] ||
      token.isInstanceOf[`)`] || token.isInstanceOf[`]`] || token.isInstanceOf[`}`] || token.isInstanceOf[`_ `] ||
      token.isInstanceOf[Ellipsis] || token.isInstanceOf[Unquote]
    })
    class StatSep extends TokenClass(token => {
      token.isInstanceOf[`;`] || token.isInstanceOf[`\n`] || token.isInstanceOf[`\n\n`] || token.isInstanceOf[EOF]
    })
    class NumericLiteral extends TokenClass(token => {
      token.isInstanceOf[Literal.Int] || token.isInstanceOf[Literal.Long] ||
      token.isInstanceOf[Literal.Float] || token.isInstanceOf[Literal.Double]
    })
  }
  import TokenClass._

  trait TokenClassifier[T] { def contains(token: Token): Boolean }
  object TokenClassifier {
    private def apply[T: ClassTag](fn: Token => Boolean): TokenClassifier[T] = {
      new TokenClassifier[T] { def contains(token: Token) = fn(token) }
    }
    implicit def inheritanceBased[T <: Token : ClassTag]: TokenClassifier[T] = apply(token => {
      token != null && classTag[T].runtimeClass.isAssignableFrom(token.getClass)
    })
    implicit def typeclassBased[T <: TokenClass : InstanceTag]: TokenClassifier[T] = apply(token => {
      instanceOf[T].contains(token)
    })
  }

  implicit class XtensionTokenClassification(val token: Token) {
    def isNot[T: TokenClassifier]: Boolean = !is[T]
    def is[T: TokenClassifier]: Boolean = implicitly[TokenClassifier[T]].contains(token)
  }

  // NOTE: This is a cache that's necessary for reasonable performance of prev/next for tokens.
  // It maps character positions in input's content into indices in the scannerTokens vector.
  // One complication here is that there can be multiple tokens that sort of occupy a given position,
  // so the clients of this cache need to be wary of that!
  //   17:09 ~/Projects/alt/core/sandbox (master)$ tokenize 'q"$x"'
  //   // Scala source: /var/folders/n4/39sn1y_d1hn3qjx6h1z3jk8m0000gn/T/tmpeIwaZ8
  //   BOF (0..-1)
  //   q (0..0)
  //   " (1..1)
  //    (2..1)
  //   splice start (2..2)
  //   x (3..3)
  //   splice end (3..2)
  //    (4..3)
  //   " (4..4)
  //   EOF (5..4)
  private val scannerTokenCache: Array[Int] = input match {
    case content: Content =>
      val result = new Array[Int](content.chars.length)
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
    case tokens: Tokens =>
      new Array[Int](0)
  }
  implicit class XtensionTokenIndex(token: Token) {
    def index: Int = {
      def fast() = {
        def lurk(roughIndex: Int): Int = {
          require(roughIndex >= 0 && debug(token))
          if (scannerTokens(roughIndex) eq token) roughIndex
          else lurk(roughIndex - 1)
        }
        lurk(scannerTokenCache(token.start))
      }
      def slow() = {
        val result = scannerTokens.indexWhere(_ eq token)
        require(result != -1 && debug(token))
        result
      }
      if (scannerTokenCache.length != 0) fast()
      else slow()
    }
    def prev: Token = {
      val prev = scannerTokens.apply(Math.max(token.index - 1, 0))
      if (prev.isInstanceOf[Token.Whitespace] || prev.isInstanceOf[Token.Comment]) prev.prev
      else prev
    }
    def next: Token = {
      val next = scannerTokens.apply(Math.min(token.index + 1, scannerTokens.length - 1))
      if (next.isInstanceOf[Token.Whitespace] || next.isInstanceOf[Token.Comment]) next.next
      else next
    }
  }

/* ------------- PARSER-SPECIFIC TOKENS -------------------------------------------- */

  // TODO: Scala's parser isn't ready to accept whitespace and comment tokens,
  // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
  // However, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted.
  // This leads to extremely dirty and seriously crazy code, which I'd like to replace in the future.
  lazy val scannerTokens = input.tokenize match {
    case Tokenized.Success(tokens) => tokens
    case Tokenized.Error(pos, message, _) => reporter.syntaxError(message, at = pos)
  }
  lazy val (parserTokens, parserTokenPositions) = {
    val parserTokens = new immutable.VectorBuilder[Token]()
    val parserTokenPositions = mutable.ArrayBuilder.make[Int]()
    @tailrec def loop(prevPos: Int, currPos: Int, sepRegions: List[Char]): Unit = {
      if (currPos >= scannerTokens.length) return
      val prev = if (prevPos >= 0) scannerTokens(prevPos) else null
      val curr = scannerTokens(currPos)
      val nextPos = {
        var i = currPos + 1
        while (i < scannerTokens.length && scannerTokens(i).is[Trivia]) i += 1
        if (i == scannerTokens.length) i = -1
        i
      }
      val next = if (nextPos != -1) scannerTokens(nextPos) else null
      if (curr.isNot[Trivia]) {
        parserTokens += curr
        parserTokenPositions += currPos
        val sepRegions1 = {
          if (curr.is[`(`]) ')' :: sepRegions
          else if (curr.is[`[`]) ']' :: sepRegions
          else if (curr.is[`{`]) '}' :: sepRegions
          else if (curr.is[CaseIntro]) '\u21d2' :: sepRegions
          else if (curr.is[`}`]) {
            var sepRegions1 = sepRegions
            while (!sepRegions1.isEmpty && sepRegions1.head != '}') sepRegions1 = sepRegions1.tail
            if (!sepRegions1.isEmpty) sepRegions1 = sepRegions1.tail
            sepRegions1
          } else if (curr.is[`]`]) { if (!sepRegions.isEmpty && sepRegions.head == ']') sepRegions.tail else sepRegions }
          else if (curr.is[`)`]) { if (!sepRegions.isEmpty && sepRegions.head == ')') sepRegions.tail else sepRegions }
          else if (curr.is[`=>`]) { if (!sepRegions.isEmpty && sepRegions.head == '\u21d2') sepRegions.tail else sepRegions }
          else sepRegions // do nothing for other tokens
        }
        loop(currPos, currPos + 1, sepRegions1)
      } else {
        var i = prevPos + 1
        var lastNewlinePos = -1
        var newlineStreak = false
        var newlines = false
        while (i < nextPos) {
          if (scannerTokens(i).is[`\n`] || scannerTokens(i).is[`\f`]) {
            lastNewlinePos = i
            if (newlineStreak) newlines = true
            newlineStreak = true
          }
          newlineStreak &= scannerTokens(i).is[Whitespace]
          i += 1
        }
        if (lastNewlinePos != -1 &&
            prev != null && prev.is[CanEndStat] &&
            next != null && next.isNot[CantStartStat] &&
            (sepRegions.isEmpty || sepRegions.head == '}')) {
          var token = scannerTokens(lastNewlinePos)
          if (newlines) token = `\n\n`(token.content, token.dialect, token.start)
          parserTokens += token
          parserTokenPositions += lastNewlinePos
          loop(lastNewlinePos, currPos + 1, sepRegions)
        } else {
          loop(prevPos, nextPos, sepRegions)
        }
      }
    }
    loop(-1, 0, Nil)
    (Tokens(parserTokens.result: _*), parserTokenPositions.result)
  }

  // NOTE: public methods of TokenIterator return scannerTokens-based positions
  trait TokenIterator extends Iterator[Token] { def prevTokenPos: Int; def tokenPos: Int; def token: Token; def fork: TokenIterator }
  var in: TokenIterator = new SimpleTokenIterator()
  private class SimpleTokenIterator(var i: Int = -1) extends TokenIterator {
    require(parserTokens.nonEmpty)
    if (i == -1) next() // NOTE: only do next() if we've been just created. forks can't go for next()
    def hasNext: Boolean = i < parserTokens.length - 1
    def next(): Token = { if (!hasNext) throw new NoSuchElementException(); i += 1; parserTokens(i) }
    def prevTokenPos: Int = if (i > 0) parserTokenPositions(Math.min(i, parserTokens.length - 1) - 1) else -1
    def tokenPos: Int = if (i > -1) parserTokenPositions(Math.min(i, parserTokens.length - 1)) else -1
    def token: Token = parserTokens(i)
    def fork: TokenIterator = new SimpleTokenIterator(i)
  }
  def token = in.token
  def next() = in.next()
  def nextOnce() = next()
  def nextTwice() = { next(); next() }
  def nextThrice() = { next(); next(); next() }

/* ------------- PARSER COMMON -------------------------------------------- */

  /** Scoping operator used to temporarily look into the future.
   *  Backs up token iterator before evaluating a block and restores it after.
   */
  @inline final def ahead[T](body: => T): T = {
    val forked = in.fork
    next()
    try body finally in = forked
  }

  /** Perform an operation while peeking ahead.
   *  Recover to inputal state in case of exception.
   */
  @inline def peekingAhead[T](tree: => T): T = {
    val forked = in.fork
    next()
    // try it, in case it is recoverable
    try tree catch { case e: Exception => in = forked ; throw e }
  }

  /** Methods inParensOrError and similar take a second argument which, should
   *  the next token not be the expected opener (e.g. token.`(`) will be returned
   *  instead of the contents of the groupers.  However in all cases accept[`(`]
   *  will be called, so a parse error will still result.  If the grouping is
   *  optional, token should be tested before calling these methods.
   */
  @inline final def inParens[T](body: => T): T = {
    accept[`(`]
    val ret = body
    accept[`)`]
    ret
  }
  @inline final def inParensOrError[T](body: => T, alt: T): T =
    if (token.is[`(`]) inParens(body)
    else { accept[`(`]; alt }

  @inline final def inParensOrUnit[T, Ret >: Lit](body: => Ret): Ret = inParensOrError(body, Lit(()))
  @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

  @inline final def inBraces[T](body: => T): T = {
    accept[`{`]
    val ret = body
    accept[`}`]
    ret
  }
  @inline final def inBracesOrError[T](body: => T, alt: T): T =
    if (token.is[`{`]) inBraces(body)
    else { accept[`{`]; alt }

  @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
  @inline final def inBracesOrUnit[T](body: => Term): Term = inBracesOrError(body, Lit(()))
  @inline final def dropAnyBraces[T](body: => T): T =
    if (token.is[`{`]) inBraces(body)
    else body

  @inline final def inBrackets[T](body: => T): T = {
    accept[`[`]
    val ret = body
    accept[`]`]
    ret
  }

/* ------------- POSITION HANDLING ------------------------------------------- */

  sealed trait Pos {
    def startTokenPos: Int
    def endTokenPos: Int
  }
  case class IndexPos(index: Int) extends Pos {
    def startTokenPos = index
    def endTokenPos = startTokenPos
  }
  case class TokenPos(token: Token) extends Pos {
    def startTokenPos = token.index
    def endTokenPos = startTokenPos
  }
  case class TreePos(tree: Tree) extends Pos {
    // NOTE: if a tree has synthetic tokens produced by inferTokens,
    // then their input will be synthetic as well, and here we verify that it's not the case
    private def requirePositioned() = {
      def fail() = throw new Exception(
        s"internal error: unpositioned prototype tree " +
        s"${tree.show[Syntax]}: ${tree.show[Structure]}")
      if (!tree.tokens.isAuthentic) fail()
    }
    def startTokenPos = { requirePositioned(); tree.tokens.require[Tokens.Slice].from }
    def endTokenPos = { requirePositioned(); tree.tokens.require[Tokens.Slice].until - 1 }
  }
  case object AutoPos extends Pos {
    def startTokenPos = in.tokenPos
    def endTokenPos = in.prevTokenPos
  }
  implicit def intToIndexPos(index: Int): IndexPos = IndexPos(index)
  implicit def tokenToTokenPos(token: Token): TokenPos = TokenPos(token)
  implicit def treeToTreePos(tree: Tree): TreePos = TreePos(tree)
  implicit def optionTreeToPos(tree: Option[Tree]): Pos = tree.map(TreePos).getOrElse(AutoPos)
  implicit def modsToPos(mods: List[Mod]): Pos = mods.headOption.map(TreePos).getOrElse(AutoPos)
  def auto = AutoPos

  def atPos[T <: Tree](start: Pos, end: Pos)(body: => T): T = {
    val startTokenPos = start.startTokenPos
    val result = body
    var endTokenPos = end.endTokenPos
    if (endTokenPos < startTokenPos) endTokenPos = startTokenPos - 1
    result.withTokens(scannerTokens.slice(startTokenPos, endTokenPos + 1)).asInstanceOf[T]
  }
  def autoPos[T <: Tree](body: => T): T = atPos(start = auto, end = auto)(body)

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

  def syntaxErrorExpected[T <: Token : TokenInfo]: Nothing =
    syntaxError(s"${implicitly[TokenInfo[T]].name} expected but ${token.name} found", at = token)

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept[T <: Token : TokenInfo]: Unit =
    if (token.is[T]) {
      if (token.isNot[EOF]) next()
    } else syntaxErrorExpected[T]

  /** If current token is T consume it. */
  def acceptOpt[T <: Token : TokenInfo]: Unit =
    if (token.is[T]) next()

  /** {{{
   *  semi = nl {nl} | `;`
   *  nl  = `\n' // where allowed
   *  }}}
   */
  def acceptStatSep(): Unit = token match {
    case _: `\n` | _: `\n\n` => next()
    case _                   => accept[`;`]
  }
  def acceptStatSepOpt() =
    if (!token.is[StatSeqEnd])
      acceptStatSep()

/* -------------- TOKEN CLASSES ------------------------------------------- */

  def isIdentAnd(pred: String => Boolean) = token match {
    case id: Ident if pred(id.code.stripPrefix("`").stripSuffix("`")) => true
    case _                                                            => false
  }
  def isUnaryOp: Boolean            = isIdentAnd(Helpers.isUnaryOp)
  def isIdentExcept(except: String) = isIdentAnd(_ != except)
  def isIdentOf(name: String)       = isIdentAnd(_ == name)
  def isIdent: Boolean              = isIdentAnd(_ => true)
  def isRawStar: Boolean            = isIdentOf("*")
  def isRawBar: Boolean             = isIdentOf("|")
  def isColonWildcardStar: Boolean  = token.is[`:`] && ahead(token.is[`_ `] && ahead(isIdentOf("*")))
  def isSpliceFollowedBy(check: => Boolean): Boolean
                                    = token.is[Ellipsis] && ahead(token.is[Unquote] && ahead(token.is[Ident] || check))
  def isBackquoted: Boolean         = token.code.startsWith("`") && token.code.endsWith("`")

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

  def ellipsis[T <: Tree : AstInfo](rank: Int, body: => T, extraSkip: => Unit = {}): T = autoPos {
    token match {
      case ellipsis: Ellipsis =>
        if (inQuasiquote) {
          if (ellipsis.rank != rank) {
            val errorMessage = s"rank mismatch when unquoting;$EOL found   : ${"." * (ellipsis.rank + 1)}$EOL required: ${"." * (rank + 1)}"
            syntaxError(errorMessage, at = ellipsis)
          } else {
            next()
            extraSkip
            val tree = {
              val result = token match {
                case _: `(` => inParens(body)
                case _: `{` => inBraces(body)
                case _ => body
              }
              result match {
                case quasi: Quasi =>
                  // NOTE: In the case of an unquote nested directly under ellipsis, we get a bit of a mixup.
                  // Unquote's pt may not be directly equal unwrapped ellipsis's pt, but be its refinement instead.
                  // For example, in `new { ..$stats }`, ellipsis's pt is Seq[Stat], but quasi's pt is Term.
                  // This is an artifact of the current implementation, so we just need to keep it mind and work around it.
                  require(classTag[T].runtimeClass.isAssignableFrom(quasi.pt) && debug(ellipsis, result, result.show[Structure]))
                  atPos(quasi, quasi)(implicitly[AstInfo[T]].quasi(quasi.rank, quasi.tree))
                case other =>
                  other
              }
            }
            implicitly[AstInfo[T]].quasi(rank, tree)
          }
        } else {
          syntaxError(s"unexpected token for $dialect", at = ellipsis)
        }
      case _ =>
        unreachable(debug(token))
    }
  }

  def unquote[T <: Tree : AstInfo](advance: Boolean = true): T = autoPos {
    token match {
      case unquote: Unquote =>
        if (inQuasiquote) {
          if (advance) {
            next()
            implicitly[AstInfo[T]].quasi(0, unquote.tree)
          } else ahead {
            implicitly[AstInfo[T]].quasi(0, unquote.tree)
          }
        } else {
          syntaxError(s"unexpected token for $dialect", at = unquote)
        }
      case _ =>
        unreachable(debug(token))
    }
  }

  def unquote[T <: Tree : AstInfo]: T = unquote[T](advance = true) // to write `unquote[T]` without round braces

  /** Convert tree to formal parameter list. */
  def convertToParams(tree: Term): List[Term.Param] = tree match {
    case Term.Tuple(ts) => ts.toList flatMap convertToParam
    case _              => List(convertToParam(tree)).flatten
  }

  /** Convert tree to formal parameter. */
  def convertToParam(tree: Term): Option[Term.Param] = tree match {
    case q: Term.Quasi =>
      Some(q.become[Term.Param.Quasi])
    case name: Term.Name =>
      Some(atPos(tree, tree)(Term.Param(Nil, name, None, None)))
    case name: Term.Placeholder =>
      Some(atPos(tree, tree)(Term.Param(Nil, atPos(name, name)(Name.Anonymous()), None, None)))
    case Term.Ascribe(name: Term.Name, tpt) =>
      Some(atPos(tree, tree)(Term.Param(Nil, name, Some(tpt), None)))
    case Term.Ascribe(name: Term.Placeholder, tpt) =>
      Some(atPos(tree, tree)(Term.Param(Nil, atPos(name, name)(Name.Anonymous()), Some(tpt), None)))
    case Lit(()) =>
      None
    case other =>
      syntaxError(s"not a legal formal parameter", at = other)
  }

  def convertToTypeId(ref: Term.Ref): Option[Type] = ref match {
    case ref: Term.Ref.Quasi =>
      Some(ref.become[Type.Quasi])
    case Term.Select(qual: Term.Quasi, name: Term.Name.Quasi) =>
      val newQual = qual.become[Term.Ref.Quasi]
      val newName = name.become[Type.Name.Quasi]
      Some(atPos(ref, ref)(Type.Select(newQual, newName)))
    case Term.Select(qual: Term.Ref, name) =>
      val newName = atPos(name, name)(Type.Name(name.value))
      Some(atPos(ref, ref)(Type.Select(qual, newName)))
    case name: Term.Name =>
      Some(atPos(name, name)(Type.Name(name.value)))
    case _ =>
      None
  }

  /** {{{ part { `sep` part } }}}, or if sepFirst is true, {{{ { `sep` part } }}}.
    * if useUnquoteForEllipsis = true, uses direct unquote instead of `part`
    * */
  final def tokenSeparated[Sep <: Token : TokenInfo, T <: Tree : AstInfo](sepFirst: Boolean, part: => T): List[T] = {
    def partOrEllipsis =
      if (token.is[Ellipsis]) ellipsis(1, unquote[T])
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

  @inline final def commaSeparated[T <: Tree : AstInfo](part: => T): List[T] =
    tokenSeparated[`,`, T](sepFirst = false, part)

  def makeTuple[T <: Tree](body: List[T], zero: () => T, tuple: List[T] => T): T = body match {
    case Nil => zero()
    case only :: Nil => only match {
      case q: Quasi if q.rank == 1 => tuple(body)
      case _ => only
    }
    case _ => tuple(body)
  }

  def makeTupleTerm(body: List[Term]): Term = {
    // NOTE: we can't make this autoPos, unlike makeTupleTermParens
    // see comments to makeTupleType for discussion
    body match {
      case Seq(q @ Term.Quasi(1, _)) => atPos(q, q)(Term.Tuple(body))
      case _ => makeTuple[Term](body, () => Lit(()), Term.Tuple(_))
    }
  }

  def makeTupleTermParens(bodyf: => List[Term]) = autoPos {
    makeTupleTerm(inParens(if (token.is[`)`]) Nil else bodyf))
  }

  // TODO: make zero tuple for types Lit.Unit() too?
  def makeTupleType(body: List[Type]): Type = {
    // NOTE: we can't make this autoPos, unlike makeTuplePatParens
    // because, by the time control reaches this method, we're already past the closing parenthesis
    // therefore, we'll rely on our callers to assign positions to the tuple we return
    // we can't do atPos(body.first, body.last) either, because that wouldn't account for parentheses
    body match {
      case Seq(q @ Type.Quasi(1, _)) => atPos(q, q)(Type.Tuple(body))
      case _ => makeTuple[Type](body, () => unreachable, Type.Tuple(_))
    }
  }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

  /** Methods which implicitly propagate the context in which they were
   *  called: either in a pattern context or not.  Formerly, this was
   *  threaded through numerous methods as boolean isPattern.
   */
  trait PatternContextSensitive {
    /** {{{
     *  ArgType       ::=  Type
     *  }}}
     */
    def argType(): Type
    def functionArgType(): Type.Arg

    private def tupleInfixType(): Type = autoPos {
      require(token.is[`(`] && debug(token))
      val openParenPos = in.tokenPos

      // NOTE: This is a really hardcore disambiguation caused by introduction of Type.Method.
      // We need to accept `(T, U) => W`, `(x: T): x.U` and also support unquoting.
      // Maybe it was not the best idea to introduce method types or to have this syntax for them...
      var hasParams = false
      var hasImplicits = false
      var hasTypes = false
      var secondOpenParenPos = -1
      var closeParenPos = -1
      val rawtss: List[List[Tree]] = {
        def paramOrType() = {
          def looksLikeParam = token.is[`implicit`] || (token.isInstanceOf[Ident] && ahead(token.is[`:`]))
          if (token.is[Ellipsis]) {
            ellipsis(1, unquote[Tree])
          } else if (token.is[Unquote]) {
            unquote[Tree]
          } else if (looksLikeParam) {
            if (hasTypes) syntaxError("can't mix function type and method type syntaxes", at = token)
            if (hasImplicits) accept[Ident]
            hasParams = true
            hasImplicits |= token.is[`implicit`]
            param(ownerIsCase = false, ownerIsType = false, isImplicit = hasImplicits)
          } else {
            if (hasParams) syntaxError("can't mix function type and method type syntaxes", at = token)
            hasTypes = true
            functionArgType()
          }
        }

        val rawtss = new ListBuffer[List[Tree]]
        while (!hasTypes && !hasImplicits && token.is[`(`]) {
          if (openParenPos != in.tokenPos && secondOpenParenPos == 0) secondOpenParenPos = in.tokenPos
          accept[`(`]
          val rawts = new ListBuffer[Tree]
          if (!token.is[`)`]) {
            rawts += paramOrType()
            while (token.is[`,`] || token.is[Ellipsis]) {
              if (token.is[`,`]) next()
              rawts += paramOrType()
            }
          }
          closeParenPos = in.tokenPos
          accept[`)`]
          // NOTE: can't have this, because otherwise we run into #312
          // newLineOptWhenFollowedBy[`(`]
          rawtss += rawts.toList
        }
        rawtss.toList
      }
      def ts: List[Type.Arg] = {
        if (hasParams) require(false && debug(hasParams, hasImplicits, hasTypes))
        if (rawtss.length != 1) {
          val message = "can't have multiple parameter lists in function types"
          syntaxError(message, at = scannerTokens(secondOpenParenPos))
        }
        rawtss.head.map({
          case q: Tree.Quasi => q.become[Type.Arg.Quasi]
          case t: Type.Arg => t
          case other => unreachable(debug(other.show[Syntax], other.show[Structure]))
        })
      }
      def pss: List[List[Term.Param]] = {
        if (hasTypes) require(false && debug(hasParams, hasImplicits, hasTypes))
        rawtss.map(_.map({
          case q: Tree.Quasi => q.become[Term.Param.Quasi]
          case t: Term.Param => t
          case other => unreachable(debug(other.show[Syntax], other.show[Structure]))
        }))
      }

      if (hasParams && !token.is[`:`]) syntaxError("can't mix function type and method type syntaxes", at = token)
      if (hasTypes && token.is[`:`]) accept[`=>`]

      if (token.is[`:`]) {
        next()
        Type.Method(pss, typ())
      } else if (token.is[`=>`]) {
        next()
        Type.Function(ts, typ())
      } else {
        val tuple = atPos(openParenPos, closeParenPos)(makeTupleType(ts map {
          case q: Type.Arg.Quasi    => q.become[Type.Quasi]
          case t: Type              => t
          case p: Type.Arg.ByName   => syntaxError("by name type not allowed here", at = p)
          case p: Type.Arg.Repeated => syntaxError("repeated type not allowed here", at = p)
        }))
        infixTypeRest(
          compoundTypeRest(Some(
            annotTypeRest(
              simpleTypeRest(
                tuple)))),
          InfixMode.FirstOp
        )
      }
    }

    private def typeLambda(): Type =
      if (dialect.allowTypeLambdas) {
        val quants = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
        val tpe = typ()
        Type.Lambda(quants, tpe)
      } else {
        syntaxError(s"type lambdas are not allowed for the $dialect dialect", at = token)
      }

    /** {{{
     *  Type ::= InfixType `=>' Type
     *         | `(' [`=>' Type] `)' `=>' Type
     *         | `[' [`=>' Type] `]' `=>' Type
     *         | InfixType [ExistentialClause]
     *  ExistentialClause ::= forSome `{' ExistentialDcl {semi ExistentialDcl}} `}'
     *  ExistentialDcl    ::= type TypeDcl | val ValDcl
     *  }}}
     */
    def typ(): Type = autoPos {
      if (token.is[`:`]) {
        next()
        Type.Method(Nil, typ())
      } else {
        val t: Type =
          if (token.is[`(`]) tupleInfixType()
          else if (token.is[`[`]) typeLambda()
          else infixType(InfixMode.FirstOp)

        token match {
          case _: `=>`      => next(); Type.Function(List(t), typ())
          case _: `forSome` => next(); Type.Existential(t, existentialStats())
          case _            => t
        }
      }
    }

    /** {{{
     *  TypeArgs    ::= `[' ArgType {`,' ArgType} `]'
     *  }}}
     */
    def typeArgs(): List[Type] = inBrackets(types())

    /** {{{
     *  ModType        ::=  SimpleType {Annotation}
     *  }}}
     */
    def annotType(): Type = annotTypeRest(simpleType())

    /** {{{
     *  SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' Id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' Types `)'
     *                     |  WildcardType
     *  }}}
     */
    def simpleType(): Type = {
      simpleTypeRest(autoPos(token match {
        case _: `(`  => autoPos(makeTupleType(inParens(types())))
        case _: `_ ` => next(); atPos(in.prevTokenPos, auto)(Type.Placeholder(typeBounds()))
        case _       =>
          val ref: Term.Ref = path()
          if (token.isNot[`.`])
            convertToTypeId(ref) getOrElse { syntaxError("identifier expected", at = ref) }
          else {
            next()
            accept[`type`]
            Type.Singleton(ref)
          }
      }))
    }

    def simpleTypeRest(t: Type): Type = token match {
      case _: `#` => next(); simpleTypeRest(atPos(t, auto)(Type.Project(t, typeName())))
      case _: `[` => simpleTypeRest(atPos(t, auto)(Type.Apply(t, typeArgs())))
      case _      => t
    }


    /** {{{
     *  CompoundType ::= ModType {with ModType} [Refinement]
     *                |  Refinement
     *  }}}
     */
    def compoundType(): Type = compoundTypeRest {
      if (token.is[`{`])
        None
      else if (isSpliceFollowedBy(token.is[`with`] || token.is[`{`] || (token.is[`\n`] && ahead (token.is[`{`]))))
        Some(ellipsis(1, unquote[Type]))
      else
        Some(annotType())
    }

    // TODO: warn about def f: Unit { } case?
    def compoundTypeRest(t: Option[Type]): Type = atPos(t, auto) {
      val ts = new ListBuffer[Type] ++ t
      while (token.is[`with`]) {
        next()
        if (token.is[Ellipsis]) ts += ellipsis(1, unquote[Type])
        else ts += annotType()
      }
      newLineOptWhenFollowedBy[`{`]
      val types = ts.toList
      if (token.is[`{`]) {
        val refinements = refinement()
        val hasQuasi = t match {
          case q @ Some(Type.Quasi(1, _)) => true
          case _ => false
        }
        (types, refinements) match {
          case (typ :: Nil, Nil) if !hasQuasi => typ
          case _  => Type.Compound(types, refinements)
        }
      } else {
        if (types.length == 1) types.head
        else Type.Compound(types, Nil)
      }
    }

    def infixTypeRest(t: Type, mode: InfixMode.Value): Type = atPos(t, auto) {
      if (isIdent || token.is[Unquote]) {
        if (isRawStar && ahead(token.is[`)`] || token.is[`,`] || token.is[`=`] || token.is[`}`] || token.is[EOF])) {
          // we assume that this is a type specification for a vararg parameter
          t
        } else {
          val name = termName(advance = false)
          val leftAssoc = name.isLeftAssoc
          if (mode != InfixMode.FirstOp) checkAssoc(name, leftAssoc = mode == InfixMode.LeftOp)
          val op = typeName()
          newLineOptWhenFollowing(_.is[TypeIntro])
          def mkOp(t1: Type) = atPos(t, t1)(Type.ApplyInfix(t, op, t1))
          if (leftAssoc)
            infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
          else
            mkOp(infixType(InfixMode.RightOp))
        }
      } else {
        t
      }
    }

    /** {{{
     *  InfixType ::= CompoundType {id [nl] CompoundType}
     *  }}}
     */
    def infixType(mode: InfixMode.Value): Type =
      infixTypeRest(compoundType(), mode)

    /** {{{
     *  Types ::= Type {`,' Type}
     *  }}}
     */
    def types(): List[Type] = commaSeparated(argType())

    // TODO: I have a feeling that we no longer need PatternContextSensitive
    // now that we have Pat.Type separate from Type
    def patternTyp(allowInfix: Boolean, allowImmediateTypevars: Boolean): Pat.Type = {
      def loop(tpe: Type, convertTypevars: Boolean): Pat.Type = tpe match {
        case q: Type.Quasi =>
          q.become[Pat.Type.Quasi]
        case tpe @ Type.Name(value) if convertTypevars && value(0).isLower =>
          atPos(tpe, tpe)(Pat.Var.Type(tpe))
        case tpe: Type.Name =>
          tpe
        case tpe: Type.Select =>
          tpe
        case Type.Project(qual, name) =>
          val qual1 = loop(qual, convertTypevars = false)
          val name1 = name
          atPos(tpe, tpe)(Pat.Type.Project(qual1, name1))
        case tpe: Type.Singleton =>
          tpe
        case Type.Apply(underlying, args) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val args1 = args.map(arg => loop(arg, convertTypevars = true))
          atPos(tpe, tpe)(Pat.Type.Apply(underlying1, args1))
        case Type.ApplyInfix(lhs, op, rhs) =>
          val lhs1 = loop(lhs, convertTypevars = false)
          val op1 = op
          val rhs1 = loop(rhs, convertTypevars = false)
          atPos(tpe, tpe)(Pat.Type.ApplyInfix(lhs1, op1, rhs1))
        case Type.Function(params, res) =>
          val params1 = params.map {
            case q @ Type.Arg.Quasi(1, Type.Arg.Quasi(0, _)) => q.become[Pat.Type.Quasi]
            case p => loop(p.require[Type], convertTypevars = true)
          }
          val res1 = loop(res, convertTypevars = false)
          atPos(tpe, tpe)(Pat.Type.Function(params1, res1))
        case Type.Tuple(elements) =>
          val elements1 = elements.map(el => loop(el, convertTypevars = true))
          atPos(tpe, tpe)(Pat.Type.Tuple(elements1))
        case Type.Compound(tpes, refinement) =>
          val tpes1 = tpes.map(tpe => loop(tpe, convertTypevars = false))
          val refinement1 = refinement
          atPos(tpe, tpe)(Pat.Type.Compound(tpes1, refinement1))
        case Type.Existential(underlying, quants) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val quants1 = quants
          atPos(tpe, tpe)(Pat.Type.Existential(underlying1, quants1))
        case Type.Annotate(underlying, annots) =>
          val underlying1 = loop(underlying, convertTypevars = false)
          val annots1 = annots
          atPos(tpe, tpe)(Pat.Type.Annotate(underlying1, annots1))
        case Type.Placeholder(Type.Bounds(None, None)) if convertTypevars =>
          atPos(tpe, tpe)(Pat.Type.Wildcard())
        case Type.Placeholder(bounds) =>
          val bounds1 = bounds
          atPos(tpe, tpe)(Pat.Type.Placeholder(bounds1))
        case tpe: Lit =>
          tpe
      }
      val t: Type = {
        if (allowInfix) {
          val t = if (token.is[`(`]) tupleInfixType() else compoundType()
          def mkOp(t1: Type) = atPos(t, t1)(Type.ApplyInfix(t, typeName(), t1))
          token match {
            case _: `forSome` => next(); atPos(t, t)(Type.Existential(t, existentialStats()))
            case _: Unquote | _: Ident if !isRawBar => infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
            case _ => t
          }
        } else {
          compoundType()
        }
      }
      loop(t, convertTypevars = allowImmediateTypevars)
    }

    def quasiquotePatternTyp(allowInfix: Boolean, allowImmediateTypevars: Boolean): Pat.Type = autoPos {
      // NOTE: As per quasiquotes.md
      // * pt"_" => Pat.Type.Wildcard (doesn't parse)
      // * pt"x" => Pat.Var.Type (needs postprocessing, parsed as Type.Name)
      // * pt"X" => error
      // * pt"`x`" => Type.Name (ok)
      // * pt"`X`" => Type.Name (ok)
      token match {
        case _: `_ ` if ahead(token.is[EOF]) =>
          next()
          Pat.Type.Wildcard()
        case _ =>
          val bqIdent = isIdent && isBackquoted
          val nonbqIdent = isIdent && !isBackquoted
          val ptpe = patternTyp(allowInfix = true, allowImmediateTypevars = false)
          ptpe match {
            case ptpe: Type.Name if nonbqIdent =>
              if (ptpe.value.head.isLower) atPos(ptpe, ptpe)(Pat.Var.Type(ptpe))
              else syntaxError("Pattern type variables must start with a lower-case letter", at = ptpe)
            case ptpe: Type.Name if bqIdent =>
              syntaxError("Pattern type variables must not be enclosed in backquotes", at = ptpe)
            case _ =>
              ptpe
          }
      }
    }

    def patternTypeArgs() = inBrackets(commaSeparated(patternTyp(allowInfix = true, allowImmediateTypevars = true)))
  }

  private trait AllowedName[T]
  private object AllowedName {
    implicit object AllowedTermName extends AllowedName[Term.Name]
    implicit object AllowedTypeName extends AllowedName[Type.Name]
  }
  private def name[T <: Tree : AllowedName : AstInfo](ctor: String => T, advance: Boolean): T = token match {
    case token: Ident =>
      val name = token.code.stripPrefix("`").stripSuffix("`")
      val res = atPos(in.tokenPos, in.tokenPos)(ctor(name))
      if (advance) next()
      res
    case token: Unquote =>
      unquote[T](advance)
    case _ =>
      syntaxErrorExpected[Ident]
  }
  def termName(advance: Boolean = true): Term.Name = name(Term.Name(_), advance)
  def typeName(advance: Boolean = true): Type.Name = name(Type.Name(_), advance)

  /** {{{
   *  Path       ::= StableId
   *              |  [Ident `.'] this
   *  ModType ::= Path [`.' type]
   *  }}}
   */
  // TODO: this has to be rewritten
  def path(thisOK: Boolean = true): Term.Ref = {
    val startsAtBof = token.prev.isInstanceOf[BOF]
    def endsAtEof = token.isInstanceOf[EOF]
    def stop = token.isNot[`.`] || ahead { token.isNot[`this`] && token.isNot[`super`] && token.isNot[Ident] && token.isNot[Unquote] }
    if (token.is[`this`]) {
      val anonqual = atPos(in.tokenPos, in.prevTokenPos)(Name.Anonymous())
      next()
      val thisp = atPos(in.prevTokenPos, auto)(Term.This(anonqual))
      if (stop && thisOK) thisp
      else {
        accept[`.`]
        selectors(thisp)
      }
    } else if (token.is[`super`]) {
      val anonqual = atPos(in.tokenPos, in.prevTokenPos)(Name.Anonymous())
      next()
      val superp = atPos(in.prevTokenPos, auto)(Term.Super(anonqual, mixinQualifier()))
      if (startsAtBof && endsAtEof && inQuasiquote) return superp
      accept[`.`]
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
        if (token.is[`this`]) {
          next()
          val qual = name match {
            case q: Term.Name.Quasi => q.become[Name.Qualifier.Quasi]
            case name => atPos(name, name)(Name.Indeterminate(name.value))
          }
          val thisp = atPos(name, auto)(Term.This(qual))
          if (stop && thisOK) thisp
          else {
            accept[`.`]
            selectors(thisp)
          }
        } else if (token.is[`super`]) {
          next()
          val qual = name match {
            case q: Term.Name.Quasi => q.become[Name.Qualifier.Quasi]
            case name => atPos(name, name)(Name.Indeterminate(name.value))
          }
          val superp = atPos(name, auto)(Term.Super(qual, mixinQualifier()))
          if (startsAtBof && endsAtEof && inQuasiquote) return superp
          accept[`.`]
          val supersel = atPos(superp, auto)(Term.Select(superp, termName()))
          if (stop) supersel
          else {
            next()
            selectors(supersel)
          }
        } else {
          selectors(name match {
            case q: Term.Name.Quasi => q.become[Term.Quasi]
            case name => name
          })
        }
      }
    }
  }

  def selector(t: Term): Term.Select = atPos(t, auto)(Term.Select(t, termName()))
  def selectors(t: Term): Term.Ref = {
    val t1 = selector(t)
    if (token.is[`.`] && ahead { token.is[Ident] }) {
      next()
      selectors(t1)
    }
    else t1
  }

  /** {{{
  *   MixinQualifier ::= `[' Id `]'
  *   }}}
  */
  def mixinQualifier(): Name.Qualifier = {
    if (token.is[`[`]) {
      inBrackets {
        typeName() match {
          case q: Type.Name.Quasi => q.become[Name.Qualifier.Quasi]
          case name => atPos(name, name)(Name.Indeterminate(name.value))
        }

      }
    } else {
      atPos(in.tokenPos, in.prevTokenPos)(Name.Anonymous())
    }
  }

  /** {{{
   *  StableId ::= Id
   *            |  Path `.' Id
   *            |  [id `.'] super [`[' id `]']`.' id
   *  }}}
   */
  def stableId(): Term.Ref =
    path(thisOK = false)

  /** {{{
  *   QualId ::= Id {`.' Id}
  *   }}}
  */
  def qualId(): Term.Ref = {
    val name = termName()
    if (token.isNot[`.`]) name
    else {
      next()
      selectors(name)
    }
  }

  /** {{{
   *  SimpleExpr    ::= literal
   *                  | symbol
   *                  | null
   *  }}}
   */
  def literal(isNegated: Boolean = false): Lit = autoPos {
    def isHex = token.code.startsWith("0x") || token.code.startsWith("0X")
    val res = token match {
      case token: Literal.Char =>
        Lit(token.value)
      case token: Literal.Int =>
        val value = if (isNegated) -token.value else token.value
        val min = if (isHex) BigInt(Int.MinValue) * 2 + 1 else BigInt(Int.MinValue)
        val max = if (isHex) BigInt(Int.MaxValue) * 2 + 1 else BigInt(Int.MaxValue)
        if (value > max) syntaxError("integer number too large", at = token)
        else if (value < min) syntaxError("integer number too small", at = token)
        Lit(value.toInt)
      case token: Literal.Long =>
        val value = if (isNegated) -token.value else token.value
        val min = if (isHex) BigInt(Long.MinValue) * 2 + 1 else BigInt(Long.MinValue)
        val max = if (isHex) BigInt(Long.MaxValue) * 2 + 1 else BigInt(Long.MaxValue)
        if (value > max) syntaxError("integer number too large", at = token)
        else if (value < min) syntaxError("integer number too small", at = token)
        Lit(value.toLong)
      case token: Literal.Float =>
        val value = if (isNegated) -token.value else token.value
        if (value > Float.MaxValue) syntaxError("floating point number too large", at = token)
        else if (value < Float.MinValue) syntaxError("floating point number too small", at = token)
        Lit(value.toFloat)
      case token: Literal.Double  =>
        val value = if (isNegated) -token.value else token.value
        if (value > Double.MaxValue) syntaxError("floating point number too large", at = token)
        else if (value < Double.MinValue) syntaxError("floating point number too small", at = token)
        Lit(value.toDouble)
      case token: Literal.String =>
        Lit(token.value)
      case token: Literal.Symbol =>
        Lit(token.value)
      case token: Literal.`true` =>
        Lit(true)
      case token: Literal.`false` =>
        Lit(false)
      case token: Literal.`null` =>
        Lit(null)
      case _ =>
        unreachable(debug(token))
    }
    next()
    res
  }

  def interpolate[Ctx <: Tree, Ret <: Tree](arg: () => Ctx, result: (Term.Name, List[Lit], List[Ctx]) => Ret): Ret = autoPos {
    val interpolator = {
      if (token.is[Xml.Start]) atPos(in.tokenPos, in.tokenPos)(Term.Name("xml"))
      else atPos(in.tokenPos, in.tokenPos)(Term.Name(token.require[Interpolation.Id].code)) // termName() for INTERPOLATIONID
    }
    if (token.is[Interpolation.Id]) next()
    val partsBuf = new ListBuffer[Lit]
    val argsBuf = new ListBuffer[Ctx]
    def loop(): Unit = token match {
      case _: Interpolation.Start | _: Xml.Start =>
        next()
        loop()
      case _: Interpolation.Part | _: Xml.Part =>
        partsBuf += atPos(in.tokenPos, in.tokenPos)(Lit(token.code))
        next()
        loop()
      case _: Interpolation.SpliceStart | _: Xml.SpliceStart =>
        next()
        argsBuf += arg()
        loop()
      case _: Interpolation.SpliceEnd | _: Xml.SpliceEnd =>
        next()
        loop()
      case _: Interpolation.End | _: Xml.End =>
        next(); // simply return
      case _ =>
        unreachable(debug(token, token.show[Structure]))
    }
    loop()
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  def interpolateTerm(): Term.Interpolate = {
    def dropTrivialBlock(term: Term): Term = term match {
      case Term.Block((stat: Term) :: Nil) => stat
      case _ => term
    }
    interpolate[Term, Term.Interpolate](arg = { () =>
      token match {
        case _: Ident   => termName()
        // case _: `_ ` => freshPlaceholder()       // ifonly etapolation
        case _: `{`     => dropTrivialBlock(expr()) // dropAnyBraces(expr0(Local))
        case _: `this`  => val qual = atPos(in.tokenPos, in.prevTokenPos)(Name.Anonymous()); next(); atPos(in.prevTokenPos, auto)(Term.This(qual))
        case _          => syntaxError("error in interpolated string: identifier or block expected", at = token)
      }
    }, result = Term.Interpolate(_, _, _))
  }

  def xmlTerm(): Term.Interpolate =
    interpolateTerm()

  def interpolatePat(): Pat.Interpolate =
    interpolate[Pat, Pat.Interpolate](arg = () => dropAnyBraces(pattern().require[Pat]), result = Pat.Interpolate(_, _, _))

  def xmlPat(): Pat.Interpolate =
    // TODO: probably should switch into XML pattern mode here
    interpolatePat()

/* ------------- NEW LINES ------------------------------------------------- */

  def newLineOpt(): Unit = {
    if (token.is[`\n`]) next()
  }

  def newLinesOpt(): Unit = {
    if (token.is[`\n`] || token.is[`\n\n`])
      next()
  }

  def newLineOptWhenFollowedBy[T <: Token : TokenInfo]: Unit = {
    // note: next is defined here because current is token.`\n`
    if (token.is[`\n`] && ahead { token.is[T] }) newLineOpt()
  }

  def newLineOptWhenFollowing(p: Token => Boolean): Unit = {
    // note: next is defined here because current is token.`\n`
    if (token.is[`\n`] && ahead { p(token) }) newLineOpt()
  }

/* ------------- TYPES ---------------------------------------------------- */

  /** {{{
   *  TypedOpt ::= [`:' Type]
   *  }}}
   */
  def typedOpt(): Option[Type] =
    if (token.is[`:`]) { next(); Some(typ()) }
    else None

  def typeOrInfixType(location: Location): Type =
    if (location == Local) typ()
    else startInfixType()

  def annotTypeRest(t: Type): Type = atPos(t, auto) {
    val annots = this.annots(skipNewLines = false)
    if (annots.isEmpty) t
    else Type.Annotate(t, annots)
  }

/* ----------- EXPRESSIONS ------------------------------------------------ */

  def condExpr(): Term = {
    accept[`(`]
    val r = expr()
    accept[`)`]
    r
  }

  /** {{{
   *  Expr       ::= (Bindings | [`implicit'] Id | `_')  `=>' Expr
   *               | Expr1
   *  ResultExpr ::= (Bindings | Id `:' CompoundType) `=>' Block
   *               | Expr1
   *  Expr1      ::= if `(' Expr `)' {nl} Expr [[semi] else Expr]
   *               | try (`{' Block `}' | Expr) [catch `{' CaseClauses `}'] [finally Expr]
   *               | while `(' Expr `)' {nl} Expr
   *               | do Expr [semi] while `(' Expr `)'
   *               | for (`(' Enumerators `)' | `{' Enumerators `}') {nl} [yield] Expr
   *               | throw Expr
   *               | return [Expr]
   *               | [SimpleExpr `.'] Id `=' Expr
   *               | SimpleExpr1 ArgumentExprs `=' Expr
   *               | PostfixExpr Ascription
   *               | PostfixExpr match `{' CaseClauses `}'
   *  Bindings   ::= `(' [Binding {`,' Binding}] `)'
   *  Binding    ::= (Id | `_') [`:' Type]
   *  Ascription ::= `:' CompoundType
   *               | `:' Annotation {Annotation}
   *               | `:' `_' `*'
   *  }}}
   */
  def expr(): Term = expr(Local)

  // TODO: when parsing `(2 + 3)`, do we want the ApplyInfix's position to include parentheses?
  // if yes, then nothing has to change here
  // if no, we need eschew autoPos here, because it forces those parentheses on the result of calling prefixExpr
  def expr(location: Location): Term = autoPos(token match {
    case _: `if` =>
      next()
      val cond = condExpr()
      newLinesOpt()
      val thenp = expr()
      if (token.is[`else`]) { next(); Term.If(cond, thenp, expr()) }
      else if (token.is[`;`] && ahead { token.is[`else`] }) { next(); next(); Term.If(cond, thenp, expr()) }
      else { Term.If(cond, thenp, atPos(in.tokenPos, in.prevTokenPos)(Lit(()))) }
    case _: `try` =>
      next()
      val body: Term = token match {
        case _: `{` => autoPos(inBracesOrUnit(block()))
        case _: `(` => inParensOrUnit(expr())
        case _      => expr()
      }
      val catchopt =
        if (token.isNot[`catch`]) None
        else {
          next()
          if (token.isNot[`{`]) Some(expr())
          else inBraces {
            if (token.is[CaseIntro] || token.is[Ellipsis]) Some(caseClauses())
            else Some(expr())
          }
        }
      val finallyopt = token match {
        case _: `finally` => next(); Some(expr())
        case _            => None
      }
      catchopt match {
        case None => Term.TryWithCases(body, Nil, finallyopt)
        case Some(cases: List[_]) => Term.TryWithCases(body, cases.require[List[Case]], finallyopt)
        case Some(term: Term) => Term.TryWithTerm(body, term, finallyopt)
        case _ => unreachable(debug(catchopt))
      }
    case _: `while` =>
      next()
      val cond = condExpr()
      newLinesOpt()
      val body = expr()
      Term.While(cond, body)
    case _: `do` =>
      next()
      val body = expr()
      while (token.is[StatSep]) next()
      accept[`while`]
      val cond = condExpr()
      Term.Do(body, cond)
    case _: `for` =>
      next()
      val enums =
        if (token.is[`{`]) inBracesOrNil(enumerators())
        else inParensOrNil(enumerators())
      newLinesOpt()
      if (token.is[`yield`]) {
        next()
        Term.ForYield(enums, expr())
      } else {
        Term.For(enums, expr())
      }
    case _: `return` =>
      next()
      if (token.is[ExprIntro]) Term.Return(expr())
      else Term.Return(atPos(in.tokenPos, auto)(Lit(())))
    case _: `throw` =>
      next()
      Term.Throw(expr())
    case _: `implicit` =>
      next()
      implicitClosure(location)
    case _ =>
      var t: Term = autoPos(postfixExpr())
      if (token.is[`=`]) {
        t match {
          case ref: Term.Ref =>
            next()
            t = atPos(ref, auto)(Term.Assign(ref, expr()))
          case app: Term.Apply =>
            def decompose(term: Term): (Term, List[List[Term.Arg]]) = term match {
              case Term.Apply(fun, args) =>
                val (core, otherArgss) = decompose(fun)
                (core, args.toList +: otherArgss)
              case term =>
                (term, Nil)
            }
            val (core, argss) = decompose(app)
            next()
            t = atPos(core, auto)(Term.Update(core, argss, expr()))
          case q: Term.Quasi =>
            next()
            t = atPos(q, auto)(Term.Assign(q.become[Term.Ref.Quasi], expr()))
          case _ =>
        }
      } else if (token.is[`:`]) {
        next()
        if (token.is[`@`] || (token.is[Ellipsis] && ahead(token.is[`@`]))) {
          t = atPos(t, auto)(Term.Annotate(t, annots(skipNewLines = false)))
        } else {
          t = {
            val tpt = typeOrInfixType(location)
            // this does not correspond to syntax, but is necessary to
            // accept closures. We might restrict closures to be between {...} only.
            atPos(t, tpt)(Term.Ascribe(t, tpt))
          }
        }
      } else if (token.is[`match`]) {
        next()
        t = atPos(t, auto)(Term.Match(t, inBracesOrNil(caseClauses())))
      }

      // Note the absense of `else if` here!!
      if (token.is[`=>`]) {
        // This is a tricky one. In order to parse lambdas, we need to recognize token sequences
        // like `(...) => ...`, `id | _ => ...` and `implicit id | _ => ...`.
        //
        // If we exclude `implicit` (which is parsed elsewhere anyway), then we can see that
        // these sequences are non-trivially ambiguous with tuples and self-type annotations
        // (i.e. are not resolvable with static lookahead).
        //
        // Therefore, when we encounter `=>`, the part in parentheses is already parsed into a Term,
        // and we need to figure out whether that term represents what we expect from a lambda's param list
        // in order to disambiguate. The term that we have at hand might wildly differ from the param list that one would expect.
        // For example, when parsing `() => x`, we arrive at `=>` having `Lit.Unit` as the parsed term.
        // That's why we later need `convertToParams` to make sense of what the parser has produced.
        //
        // Rules:
        // 1) `() => ...` means lambda
        // 2) `x => ...` means self-type annotation, but only in template position
        // 3) `(x) => ...` means self-type annotation, but only in template position
        // 4a) `x: Int => ...` means self-type annotation in template position
        // 4b) `x: Int => ...` means lambda in block position
        // 4c) `x: Int => ...` means ascription, i.e. `x: (Int => ...)`, in expression position
        // 5) `(x: Int) => ...` means lambda
        // 6) `(x, y) => ...` or `(x: Int, y: Int) => ...` or with more entries means lambda
        //
        // A funny thing is that scalac's parser tries to disambiguate between self-type annotations and lambdas
        // even if it's not parsing the first statement in the template. E.g. `class C { foo; x => x }` will be
        // a parse error, because `x => x` will be deemed a self-type annotation, which ends up being inapplicable there.
        val looksLikeLambda = {
          val inParens = t.tokens.nonEmpty && t.tokens.head.is[`(`] && t.tokens.last.is[`)`]
          object NameLike { def unapply(tree: Tree): Boolean = tree.isInstanceOf[Term.Name] || tree.isInstanceOf[Term.Placeholder] }
          object ParamLike {
            def unapply(tree: Tree): Boolean = tree match {
              case Term.Quasi(0, _) => true
              case Term.Quasi(1, ParamLike()) => true
              case NameLike() => true
              case Term.Ascribe(NameLike(), _) => true
              case _ => false
            }
          }
          t match {
            case Lit(()) => true // 1
            case NameLike() => location != InTemplate // 2-3
            case ParamLike() => inParens || location == InBlock // 4-5
            case Term.Tuple(xs) => xs.forall(ParamLike.unapply) // 6
            case _ => false
          }
        }
        if (looksLikeLambda) {
          next()
          t = atPos(t, auto)(Term.Function(convertToParams(t), if (location != InBlock) expr() else block()))
        } else {
          // do nothing, which will either allow self-type annotation parsing to kick in
          // or will trigger an unexpected token error down the line
        }
      }
      t
  })

  /** {{{
   *  Expr ::= implicit Id => Expr
   *  }}}
   */

  def implicitClosure(location: Location): Term.Function = {
    require(token.isNot[`implicit`] && debug(token))
    val implicitPos = in.prevTokenPos
    val paramName = termName()
    val paramTpt = if (token.is[`:`]) { next(); Some(typeOrInfixType(location)) } else None
    val param = atPos(implicitPos, auto)(Term.Param(List(atPos(implicitPos, implicitPos)(Mod.Implicit())), paramName, paramTpt, None))
    accept[`=>`]
    atPos(implicitPos, auto)(Term.Function(List(param), if (location != InBlock) expr() else block()))
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
    // 2) We need to carry startPos/endPos separately from lhs.position
    //    because their extent may be bigger than lhs because of parentheses or whatnot.
    case class UnfinishedInfix(startPos: Pos, lhs: Lhs, endPos: Pos, op: Term.Name, targs: List[Type]) {
      def precedence = op.precedence
    }

    // The stack of unfinished infix expressions, e.g. Stack([a + ]) in `a + b [*] c`.
    // `push` takes `b`, reads `*`, checks for type arguments and adds [b *] on the top of the stack.
    // Other methods working on the stack are self-explanatory.
    var stack: List[UnfinishedInfix] = Nil
    def head = stack.head
    def push(startPos: Pos, lhs: Lhs, endPos: Pos, op: Term.Name, targs: List[Type]): Unit = stack ::= UnfinishedInfix(startPos, lhs, endPos, op, targs)
    def pop(): UnfinishedInfix = try head finally stack = stack.tail

    def reduceStack(stack: List[UnfinishedInfix], curr: Rhs, endPos: Pos): Lhs = {
      val opPrecedence = if (token.is[Ident]) termName(advance = false).precedence else 0
      val leftAssoc    = !token.is[Ident] || termName(advance = false).isLeftAssoc

      def isDone          = this.stack == stack
      def lowerPrecedence = !isDone && (opPrecedence < this.head.precedence)
      def samePrecedence  = !isDone && (opPrecedence == this.head.precedence)
      def canReduce       = lowerPrecedence || leftAssoc && samePrecedence

      if (samePrecedence) {
        checkAssoc(this.head.op, leftAssoc)
      }

      // Pop off an unfinished infix expression off the stack and finish it with the rhs.
      // Then convert the result, so that it can become someone else's rhs.
      // Repeat while precedence and associativity allow.
      def loop(rhs: Rhs): Lhs = {
        if (!canReduce) {
          toLhs(rhs)
        } else {
          val lhs = pop()
          val fin = finishInfixExpr(lhs, rhs, endPos)
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
    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Rhs, endPos: Pos): FinishedInfix
  }

  // Infix syntax in terms is borderline crazy.
  //
  // For example, did you know that `a * b + (c, d) * (f, g: _*)` means:
  // a.$times(b).$plus(scala.Tuple2(c, d).$times(f, g: _*))?!
  //
  // Therefore, Lhs = List[Term], Rhs = List[Term.Arg], FinishedInfix = Term.
  //
  // Actually there's even crazier stuff in scala-compiler.jar.
  // Apparently you can parse and typecheck `a + (bs: _*) * c`,
  // however I'm going to error out on this.
  object termInfixContext extends InfixContext {
    type Lhs = List[Term]
    type Rhs = List[Term.Arg]
    type FinishedInfix = Term

    def toRhs(fin: FinishedInfix): Rhs = List(fin)
    def toLhs(rhs: Rhs): Lhs = rhs.map({
      case term: Term => term
      case arg => syntaxError("`: _*' annotations are only allowed in arguments to *-parameters", at = arg)
    })

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Rhs, endPos: Pos): FinishedInfix = {
      val UnfinishedInfix(startPos, lhses, halfPos, op, targs) = unf
      val lhs = atPos(startPos, halfPos)(makeTupleTerm(lhses)) // `a + (b, c) * d` leads to creation of a tuple!
      atPos(startPos, endPos)(Term.ApplyInfix(lhs, op, targs, rhs))
    }
  }

  // In comparison with terms, patterns are trivial.
  implicit object patInfixContext extends InfixContext {
    type Lhs = Pat
    type Rhs = Pat
    type FinishedInfix = Pat

    def toRhs(fin: FinishedInfix): Rhs = fin
    def toLhs(rhs: Rhs): Lhs = rhs

    protected def finishInfixExpr(unf: UnfinishedInfix, rhs: Rhs, endPos: Pos): FinishedInfix = {
      val UnfinishedInfix(startPos, lhs, _, op, _) = unf
      val args = rhs match { case Pat.Tuple(args) => args.toList; case _ => List(rhs) }
      atPos(startPos, endPos)(Pat.ExtractInfix(lhs, op, args))
    }
  }

  def checkAssoc(op: Term.Name, leftAssoc: Boolean): Unit = (
    if (op.isLeftAssoc != leftAssoc)
      syntaxError("left- and right-associative operators with same precedence may not be mixed", at = op)
  )

  /** {{{
   *  PostfixExpr   ::= InfixExpr [Id [nl]]
   *  InfixExpr     ::= PrefixExpr
   *                  | InfixExpr Id [nl] InfixExpr
   *  }}}
   */
  def postfixExpr(): Term = {
    val ctx  = termInfixContext
    val base = ctx.stack

    // Skip to later in the `postfixExpr` method to start mental debugging.
    def loop(startPosK: Pos, rhsK: ctx.Rhs, endPosK: Pos): ctx.Rhs = {
      if (!token.is[Ident] && !token.is[Unquote]) {
        // Infix chain has ended.
        // In the running example, we're at `a + b[]`
        // with base = List([a +]), rhsK = List([b]).
        rhsK
      } else {
        newLineOptWhenFollowing(_.is[ExprIntro])
        if (token.is[ExprIntro]) {
          // Infix chain continues.
          // In the running example, we're at `a [+] b`
          // with base = List(), rhsK = [a].
          val lhsK = ctx.reduceStack(base, rhsK, endPosK) // lhsK = [a]
          val op = termName() // op = [+]
          val targs = if (token.is[`[`]) exprTypeArgs() else Nil // targs = Nil
          ctx.push(startPos, lhsK, endPos, op, targs) // afterwards, ctx.stack = List([a +])

          // startPosKplus1/endPosKplus1 may be bigger than then extent of rhsKplus1,
          // so we really have to track them separately.
          val startTokKplus1 = in.token
          var startPosKplus1: Pos = IndexPos(in.tokenPos)
          val rhsKplus1 = argumentExprsOrPrefixExpr()
          var endPosKplus1: Pos = IndexPos(in.prevTokenPos)
          if (startTokKplus1.isNot[`{`] && startTokKplus1.isNot[`(`]) {
            startPosKplus1 = TreePos(rhsKplus1.head)
            endPosKplus1 = TreePos(rhsKplus1.head)
          }

          // Try to continue the infix chain.
          loop(startPosKplus1, rhsKplus1, endPosKplus1) // base = List([a +]), rhsKplus1 = List([b])
        } else {
          // Infix chain has ended with a postfix expression.
          // This never happens in the running example.
          ???
          // val lhs = ctx.toLhs(reduceStack(base, rhsK, endPos))
          // val op = termName()
          // if (token.is[`[`]) syntaxError("type application is not allowed for postfix operators", at = token)
          // val qual = reduceStack(stack, lhs, endPos)
          // atPos(qual, op)(Term.Select(qual, op))
        }
      }
    }

    // Start the infix chain.
    // We'll use `a + b` as our running example.
    val rhs0 = ctx.toRhs(prefixExpr())

    // Iteratively read the infix chain via `loop`.
    // rhs0 is now [a]
    // If the next token is not an ident or an unquote, the infix chain ends immediately,
    // and `postfixExpr` becomes a fallthrough.
    val rhsN = loop(rhs0.head, rhs0, rhs0.head)

    // Infix chain has ended.
    // base contains pending UnfinishedInfix parts and rhsN is the final rhs.
    // For our running example, this'll be List([a +]) and [b].
    // Afterwards, `lhsResult` will be List([a + b]).
    val lhsResult = ctx.reduceStack(base, rhsN, in.prevTokenPos)

    // This is something not captured by the type system.
    // When applied to a result of `loop`, `reduceStack` will produce a singleton list.
    lhsResult match { case List(finResult) => finResult; case _ => unreachable(debug(lhsResult)) }
  }

  /** {{{
   *  PrefixExpr   ::= [`-' | `+' | `~' | `!' | `&'] SimpleExpr
   *  }}}
   */
  def prefixExpr(): Term =
    if (!isUnaryOp) simpleExpr()
    else {
      val op = termName()
      if (op.value == "-" && token.is[NumericLiteral])
        simpleExprRest(atPos(op, auto)(literal(isNegated = true)), canApply = true)
      else
        atPos(op, auto)(Term.ApplyUnary(op, simpleExpr()))
    }

  /** {{{
   *  SimpleExpr    ::= new (ClassTemplate | TemplateBody)
   *                  |  BlockExpr
   *                  |  SimpleExpr1 [`_']
   *  SimpleExpr1   ::= literal
   *                  |  xLiteral
   *                  |  Path
   *                  |  `(' [Exprs] `)'
   *                  |  SimpleExpr `.' Id
   *                  |  SimpleExpr TypeArgs
   *                  |  SimpleExpr1 ArgumentExprs
   *  }}}
   */
  def simpleExpr(): Term = autoPos {
    var canApply = true
    val t: Term = {
      token match {
        case _: Literal =>
          literal()
        case _: Interpolation.Id =>
          interpolateTerm()
        case _: Xml.Start =>
          xmlTerm()
        case _: Ident | _: `this` | _: `super` | _: Unquote =>
          path() match {
            case q: Term.Ref.Quasi => q.become[Term.Quasi]
            case path => path
          }
        case _: `_ ` =>
          next()
          atPos(in.prevTokenPos, in.prevTokenPos)(Term.Placeholder())
        case _: `(` =>
          makeTupleTermParens(commaSeparated(expr()))
        case _: `{` =>
          canApply = false
          blockExpr()
        case _: `new` =>
          canApply = false
          next()
          atPos(in.prevTokenPos, auto)(Term.New(template()))
        case _ =>
          syntaxError(s"illegal start of simple expression", at = token)
      }
    }
    simpleExprRest(t, canApply = canApply)
  }

  def simpleExprRest(t: Term, canApply: Boolean): Term = atPos(t, auto) {
    if (canApply) newLineOptWhenFollowedBy[`{`]
    token match {
      case _: `.` =>
        next()
        simpleExprRest(selector(t), canApply = true)
      case _: `[` =>
        t match {
          case _: Term.Quasi | _: Term.Name | _: Term.Select | _: Term.Apply =>
            var app: Term = t
            while (token.is[`[`])
              app = atPos(t, auto)(Term.ApplyType(app, exprTypeArgs()))

            simpleExprRest(app, canApply = true)
          case _ =>
            t
        }
      case _: `(` | _: `{` if (canApply) =>
        simpleExprRest(atPos(t, auto)(Term.Apply(t, argumentExprs())), canApply = true)
      case _: `_ ` =>
        next()
        Term.Eta(t)
      case _ =>
        t
    }
  }

  def argumentExprsOrPrefixExpr(): List[Term.Arg] = {
    if (token.isNot[`{`] && token.isNot[`(`]) prefixExpr() :: Nil
    else {
      def argsToTerm(args: List[Term.Arg], openParenPos: Int, closeParenPos: Int): Term = {
        def badRep(rep: Term.Arg.Repeated) = syntaxError("repeated argument not allowed here", at = rep)
        def loop(args: List[Term.Arg]): List[Term] = args match {
          case Nil                                                 => Nil
          case (t: Term) :: rest                                   => t :: loop(rest)
          case (nmd @ Term.Arg.Named(name, rhs: Term)) :: rest     => atPos(nmd, nmd)(Term.Assign(name, rhs)) :: loop(rest)
          case (Term.Arg.Named(_, rep: Term.Arg.Repeated)) :: rest => badRep(rep)
          case (rep: Term.Arg.Repeated) :: rest                    => badRep(rep)
          case _                                                   => unreachable(debug(args))
        }
        atPos(openParenPos, closeParenPos)(makeTupleTerm(loop(args)))
      }
      val openParenPos = in.tokenPos
      val args = argumentExprs()
      val closeParenPos = in.prevTokenPos
      token match {
        case _: `.` | _: `[` | _: `(` | _: `{` | _: `_ ` =>
          simpleExprRest(argsToTerm(args, openParenPos, closeParenPos), canApply = true) :: Nil
        case _ =>
          args match {
            case arg :: Nil => atPos(openParenPos, closeParenPos)(arg) :: Nil
            case other => other
          }
      }
    }
  }

  def argumentExpr(): Term.Arg = {
    expr() match {
      case q: Quasi =>
        q.become[Term.Arg.Quasi]
      case Term.Ascribe(t1, Type.Placeholder(Type.Bounds(None, None))) if isIdentOf("*") =>
        next()
        atPos(t1, auto)(Term.Arg.Repeated(t1))
      case Term.Assign(t1: Term.Name, Term.Ascribe(t2, Type.Placeholder(Type.Bounds(None, None)))) if isIdentOf("*") =>
        next()
        atPos(t1, auto)(Term.Arg.Named(t1, atPos(t2, auto)(Term.Arg.Repeated(t2))))
      case Term.Assign(t2: Term.Name, rhs) =>
        atPos(t2, auto)(Term.Arg.Named(t2, rhs))
      case other =>
        other
    }
  }

  /** {{{
   *  ArgumentExprs ::= `(' [Exprs] `)'
   *                  | [nl] BlockExpr
   *  }}}
   */
  def argumentExprs(): List[Term.Arg] = token match {
    case _: `{` =>
      List(blockExpr())
    case _: `(` =>
      inParens(token match {
        case _: `)` =>
          Nil
        case tok: Ellipsis if tok.rank == 2 =>
          List(ellipsis(2, unquote[Term.Arg]))
        case _ =>
          commaSeparated(argumentExpr)
      })
    case _ =>
      Nil
  }

  /** {{{
   *  BlockExpr ::= `{' (CaseClauses | Block) `}'
   *  }}}
   */
  def blockExpr(): Term = autoPos {
    inBraces {
      if (token.is[CaseIntro] || (token.is[Ellipsis] && ahead(token.is[`case`]))) Term.PartialFunction(caseClauses())
      else block()
    }
  }

  /** {{{
   *  Block ::= BlockStatSeq
   *  }}}
   *  @note  Return tree does not carry position.
   */
  def block(): Term = autoPos {
    Term.Block(blockStatSeq())
  }

  def caseClause(): Case = atPos(in.prevTokenPos, auto) {
    require(token.isNot[`case`] && debug(token))
    Case(pattern().require[Pat], guard(), {
      accept[`=>`]
      val start = in.tokenPos
      def end = in.prevTokenPos
      blockStatSeq() match {
        case List(q: Quasi) => q.become[Term.Quasi]
        case List(term: Term) => term
        case other => atPos(start, end)(Term.Block(other))
      }
    })
  }

  /** {{{
   *  CaseClauses ::= CaseClause {CaseClause}
   *  CaseClause  ::= case Pattern [Guard] `=>' Block
   *  }}}
   */
  def caseClauses(): List[Case] = {
    val cases = new ListBuffer[Case]
    while (token.is[CaseIntro] || token.is[Ellipsis]) {
      if (token.is[Ellipsis]) {
        cases += ellipsis(1, unquote[Case], accept[`case`])
        while (token.is[StatSep]) next()
      } else if (token.is[`case`] && ahead(token.is[Unquote])) {
        next()
        cases += unquote[Case]
        while (token.is[StatSep]) next()
      } else {
        next()
        cases += caseClause()
      }
    }
    if (cases.isEmpty)  // trigger error if there are no cases
      accept[`case`]
    cases.toList
  }

  /** {{{
   *  Guard ::= if PostfixExpr
   *  }}}
   */
  def guard(): Option[Term] =
    if (token.is[`if`]) { next(); Some(autoPos(postfixExpr())) }
    else None

  /** {{{
   *  Enumerators ::= Generator {semi Enumerator}
   *  Enumerator  ::=  Generator
   *                |  Guard
   *                |  val Pattern1 `=' Expr
   *  }}}
   */
  def enumerators(): List[Enumerator] = {
    val enums = new ListBuffer[Enumerator]
    enums ++= enumerator(isFirst = true)
    while (token.is[StatSep]) {
      next()
      enums ++= enumerator(isFirst = false)
    }
    enums.toList
  }

  def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true): List[Enumerator] =
    if (token.is[`if`] && !isFirst) autoPos(Enumerator.Guard(guard().get)) :: Nil
    else if (token.is[Ellipsis]) {
      ellipsis(1, unquote[Enumerator.Generator]) :: Nil
    } else if (token.is[Unquote] && ahead(!token.is[`=`] && !token.is[`<-`])) { // support for q"for ($enum1; ..$enums; $enum2)"
      unquote[Enumerator.Generator] :: Nil
    }
    else generator(!isFirst, allowNestedIf)

  /** {{{
   *  Generator ::= Pattern1 (`<-' | `=') Expr [Guard]
   *  }}}
   */
  def generator(eqOK: Boolean, allowNestedIf: Boolean = true): List[Enumerator] = {
    val startPos = in.tokenPos
    val hasVal = token.is[`val`]
    if (hasVal)
      next()

    val pat   = noSeq.pattern1()
    val point = token.start
    val hasEq = token.is[`=`]

    if (hasVal) {
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated", at = token)
      else syntaxError("val in for comprehension must be followed by assignment", at = token)
    }

    if (hasEq && eqOK) next()
    else accept[`<-`]
    val rhs = expr()

    val head = {
      if (hasEq) atPos(startPos, auto)(Enumerator.Val(pat.require[Pat], rhs))
      else atPos(startPos, auto)(Enumerator.Generator(pat.require[Pat], rhs))
    }
    val tail = {
      def loop(): List[Enumerator] = {
        if (token.isNot[`if`]) Nil
        else autoPos(Enumerator.Guard(guard().get)) :: loop()
      }
      if (allowNestedIf) loop()
      else Nil
    }
    head :: tail
  }

/* -------- PATTERNS ------------------------------------------- */

  /** Methods which implicitly propagate whether the initial call took
   *  place in a context where sequences are allowed.  Formerly, this
   *  was threaded through methods as boolean seqOK.
   */
  trait SeqContextSensitive extends PatternContextSensitive {
    // is a sequence pattern _* allowed?
    def isSequenceOK: Boolean

    // are we in an XML pattern?
    def isXML: Boolean = false

    def functionArgType(): Type.Arg = paramType()
    def argType(): Type = typ()

    /** {{{
     *  Patterns ::= Pattern { `,' Pattern }
     *  SeqPatterns ::= SeqPattern { `,' SeqPattern }
     *  }}}
     */
    def patterns(): List[Pat.Arg] = commaSeparated(pattern())

    /** {{{
     *  Pattern  ::=  Pattern1 { `|' Pattern1 }
     *  SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
     *  }}}
     */
    def pattern(): Pat.Arg = {
      def loop(pat: Pat.Arg): Pat.Arg =
        if (!isRawBar) pat
        else { next(); atPos(pat, auto)(Pat.Alternative(pat.require[Pat], pattern().require[Pat])) }
      loop(pattern1())
    }

    private def topLevelNamesToPats[T >: Pat](op: => T): T = {
      // NOTE: As per quasiquotes.md
      // * p"x" => Pat.Var.Term (ok)
      // * p"X" => Pat.Var.Term (needs postprocessing, parsed as Term.Name)
      // * p"`x`" => Term.Name (ok)
      // * p"`X`" => Term.Name (ok)
      val nonbqIdent = isIdent && !isBackquoted
      val pat = op
      pat match {
        case pat: Term.Name if nonbqIdent => atPos(pat, pat)(Pat.Var.Term(pat))
        case _ => pat
      }
    }

    def quasiquotePattern(): Pat.Arg = {
      topLevelNamesToPats(pattern())
    }

    def quasiquotePatternArg(): Pat.Arg = {
      topLevelNamesToPats(argumentPattern())
    }

    /** {{{
     *  Pattern1    ::= varid `:' TypePat
     *                |  `_' `:' TypePat
     *                |  Pattern2
     *  SeqPattern1 ::= varid `:' TypePat
     *                |  `_' `:' TypePat
     *                |  [SeqPattern2]
     *  }}}
     */
    def pattern1(): Pat.Arg = autoPos {
      val p = pattern2()
      if (token.isNot[`:`]) p
      else {
        p match {
          case p: Pat.Quasi =>
            nextOnce()
            Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
          case p: Pat.Var.Term if dialect.bindToSeqWildcardDesignator == ":" && isColonWildcardStar =>
            nextOnce()
            val wildcard = autoPos({ nextTwice(); Pat.Arg.SeqWildcard() })
            Pat.Bind(p, wildcard)
          case p: Pat.Var.Term =>
            nextOnce()
            Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
          case p: Pat.Wildcard if dialect.bindToSeqWildcardDesignator == ":" && isColonWildcardStar =>
            nextThrice()
            Pat.Arg.SeqWildcard()
          case p: Pat.Wildcard =>
            nextOnce()
            Pat.Typed(p, patternTyp(allowInfix = false, allowImmediateTypevars = false))
          case p =>
            p
        }
      }
    }

    /** {{{
     *  Pattern2    ::=  varid [ @ Pattern3 ]
     *                |   Pattern3
     *  SeqPattern2 ::=  varid [ @ SeqPattern3 ]
     *                |   SeqPattern3
     *  }}}
     */
    def pattern2(): Pat.Arg = autoPos {
      val p = pattern3()
      if (token.isNot[`@`]) p
      else p match {
        case p: Pat.Quasi =>
          next()
          val lhs = p.become[Pat.Var.Term.Quasi]
          val rhs = pattern3() match {
            case q @ Pat.Quasi(0, _) => q.become[Pat.Arg.Quasi]
            case p => p
          }
          Pat.Bind(lhs, rhs)
        case p: Term.Name =>
          syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)", at = p)
        case p: Pat.Var.Term =>
          next()
          Pat.Bind(p, pattern3())
        case p: Pat.Wildcard =>
          next()
          pattern3()
        case p =>
          p
      }
    }

    /** {{{
     *  Pattern3    ::= SimplePattern
     *                |  SimplePattern {Id [nl] SimplePattern}
     *  }}}
     */
    def pattern3(): Pat.Arg = {
      val ctx = patInfixContext
      val lhs = simplePattern(badPattern3)
      val base = ctx.stack
      // See SI-3189, SI-4832 for motivation. Cf SI-3480 for counter-motivation.
      def isCloseDelim = token match {
        case _: `}` => isXML
        case _: `)` => !isXML
        case _      => false
      }
      def checkWildStar: Option[Pat.Arg.SeqWildcard] = lhs match {
        case Pat.Wildcard() if isSequenceOK && isRawStar && dialect.bindToSeqWildcardDesignator == "@" => peekingAhead (
          // TODO: used to be Star(lhs) | EmptyTree, why start had param?
          if (isCloseDelim) Some(atPos(lhs, auto)(Pat.Arg.SeqWildcard()))
          else None
        )
        case _ => None
      }
      def loop(rhs: ctx.Rhs): ctx.Rhs = ctx.reduceStack(base, rhs, rhs) match {
        case lhs1 if isIdentExcept("|") || token.is[Unquote] =>
          val op = termName()
          if (token.is[`[`]) syntaxError("infix patterns cannot have type arguments", at = token)
          ctx.push(lhs1, lhs1, lhs1, op, Nil)
          val rhs1 = simplePattern(badPattern3)
          loop(rhs1)
        case lhs1 =>
          lhs1 // TODO: more rigorous type discipline
      }
      checkWildStar getOrElse loop(lhs) // TODO: more rigorous type discipline
    }

    def badPattern3(token: Token): Nothing = {
      import patInfixContext._
      def isComma                = token.is[`,`]
      def isDelimiter            = token.is[`)`] || token.is[`}`]
      def isCommaOrDelimiter     = isComma || isDelimiter
      val (isUnderscore, isStar) = stack match {
        case UnfinishedInfix(_, Pat.Wildcard(), _, Term.Name("*"), _) :: _ => (true,   true)
        case UnfinishedInfix(_, _, _, Term.Name("*"), _) :: _              => (false,  true)
        case _                                                    => (false, false)
      }
      def isSeqPatternClose = isUnderscore && isStar && isSequenceOK && isDelimiter
      val preamble = "bad simple pattern:"
      val subtext = (isUnderscore, isStar, isSequenceOK) match {
        case (true,  true, true)  if isComma            => "bad use of _* (a sequence pattern must be the last pattern)"
        case (true,  true, true)  if isDelimiter        => "bad brace or paren after _*"
        case (true,  true, false) if isDelimiter        => "bad use of _* (sequence pattern not allowed)"
        case (false, true, true)  if isDelimiter        => "use _* to match a sequence"
        case (false, true, _)     if isCommaOrDelimiter => "trailing * is not a valid pattern"
        case _                                          => null
      }
      val msg = if (subtext != null) s"$preamble $subtext" else "illegal start of simple pattern"
      // better recovery if don't skip delims of patterns
      val skip = !isCommaOrDelimiter || isSeqPatternClose
      syntaxError(msg, at = token)
    }

    /** {{{
     *  SimplePattern    ::= varid
     *                    |  `_'
     *                    |  literal
     *                    |  XmlPattern
     *                    |  StableId  /[TypeArgs]/ [`(' [Patterns] `)']
     *                    |  StableId  [`(' [Patterns] `)']
     *                    |  StableId  [`(' [Patterns] `,' [varid `@'] `_' `*' `)']
     *                    |  `(' [Patterns] `)'
     *  }}}
     *
     * XXX: Hook for IDE
     */
    def simplePattern(): Pat =
      // simple diagnostics for this entry point
      simplePattern(token => syntaxError("illegal start of simple pattern", at = token))
    def simplePattern(onError: Token => Nothing): Pat = autoPos(token match {
      case _: Ident | _: `this` | _: Unquote =>
        val isBackquoted = parser.isBackquoted
        val sid = stableId()
        val isVarPattern = sid match {
          case _: Term.Name.Quasi => false
          case Term.Name(value) => !isBackquoted && ((value.head.isLower && value.head.isLetter) || value.head == '_')
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
          case _: `[` => patternTypeArgs()
          case _        => Nil
        }
        (token, sid) match {
          case (_: `(`, _)                          => Pat.Extract(sid, targs, argumentPatterns())
          case (_, _) if targs.nonEmpty             => syntaxError("pattern must be a value", at = token)
          case (_, name: Term.Name.Quasi)           => name.become[Pat.Quasi]
          case (_, name: Term.Name) if isVarPattern => Pat.Var.Term(name)
          case (_, name: Term.Name)                 => name
          case (_, select: Term.Select)             => select
          case _                                    => unreachable(debug(token, token.show[Structure], sid, sid.show[Structure]))
        }
      case _: `_ ` =>
        next()
        Pat.Wildcard()
      case _: Literal =>
        literal()
      case _: Interpolation.Id =>
        interpolatePat()
      case _: Xml.Start =>
        xmlPat()
      case _: `(` =>
        val patterns = inParens(if (token.is[`)`]) Nil else noSeq.patterns()).map {
          case q: Pat.Arg.Quasi => q.become[Pat.Quasi]
          case p => p.require[Pat]
        }
        makeTuple[Pat](patterns, () => Lit(()), Pat.Tuple(_))
      case _ =>
        onError(token)
    })
  }
  /** The implementation of the context sensitive methods for parsing outside of patterns. */
  object outPattern extends PatternContextSensitive {
    def argType(): Type = typ()
    def functionArgType(): Type.Arg = paramType()
  }
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
  /** These are default entry points into the pattern context sensitive methods:
   *  they are all initiated from non-pattern context.
   */
  def typ() = outPattern.typ()
  def startInfixType() = outPattern.infixType(InfixMode.FirstOp)
  def startModType() = outPattern.annotType()
  def exprTypeArgs() = outPattern.typeArgs()
  def exprSimpleType() = outPattern.simpleType()

  /** Default entry points into some pattern contexts. */
  def pattern(): Pat.Arg = noSeq.pattern()
  def quasiquotePattern(): Pat.Arg = noSeq.quasiquotePattern()
  def quasiquotePatternArg(): Pat.Arg = seqOK.quasiquotePatternArg()
  def seqPatterns(): List[Pat.Arg] = seqOK.patterns()
  def xmlSeqPatterns(): List[Pat.Arg] = xmlSeqOK.patterns() // Called from xml parser
  def argumentPattern(): Pat.Arg = seqOK.pattern()
  def argumentPatterns(): List[Pat.Arg] = inParens {
    if (token.is[`)`]) Nil
    else seqPatterns()
  }
  def xmlLiteralPattern(): Pat = syntaxError("XML literals are not supported", at = in.token)
  def patternTyp() = noSeq.patternTyp(allowInfix = true, allowImmediateTypevars = false)
  def quasiquotePatternTyp() = noSeq.quasiquotePatternTyp(allowInfix = true, allowImmediateTypevars = false)
  def patternTypeArgs() = noSeq.patternTypeArgs()

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  def accessModifier(): Mod = autoPos {
    val mod = in.token match {
      case _: `private` => (name: Name.Qualifier) => Mod.Private(name)
      case _: `protected` => (name: Name.Qualifier) => Mod.Protected(name)
      case other => unreachable(debug(other, other.show[Structure]))
    }
    next()
    if (in.token.isNot[`[`]) mod(autoPos(Name.Anonymous()))
    else {
      next()
      val result = {
        if (in.token.is[`this`]) {
          val qual = atPos(in.tokenPos, in.prevTokenPos)(Name.Anonymous())
          next()
          mod(atPos(in.prevTokenPos, auto)(Term.This(qual)))
        } else if (in.token.is[Unquote]) {
          mod(unquote[Name.Qualifier.Quasi])
        } else {
          val name = termName()
          mod(atPos(name, name)(Name.Indeterminate(name.value)))
        }
      }
      accept[`]`]
      result
    }
  }

  /** {{{
   *  AccessModifier ::= (private | protected) [AccessQualifier]
   *  AccessQualifier ::= `[' (Id | this) `]'
   *  }}}
   */
  def accessModifierOpt(): Option[Mod] = {
    if (token.is[`private`] || token.is[`protected`]) Some(accessModifier())
    else if (token.is[Unquote]) Some(unquote[Mod])
    else None
  }

  def modifier(): Mod = autoPos(token match {
    case _: Unquote     => unquote[Mod]
    case _: Ellipsis    => ellipsis(1, unquote[Mod])
    case _: `abstract`  => next(); Mod.Abstract()
    case _: `final`     => next(); Mod.Final()
    case _: `sealed`    => next(); Mod.Sealed()
    case _: `implicit`  => next(); Mod.Implicit()
    case _: `lazy`      => next(); Mod.Lazy()
    case _: `override`  => next(); Mod.Override()
    case _: `private`   => accessModifier()
    case _: `protected` => accessModifier()
    case _              => syntaxError(s"modifier expected but ${token.name} found", at = token)
  })

  /** {{{
   *  Modifiers ::= {Modifier}
   *  Modifier  ::= LocalModifier
   *              |  AccessModifier
   *              |  override
   *  }}}
   */
  def modifiers(isLocal: Boolean = false): List[Mod] = {
    def appendMod(mods: List[Mod], mod: Mod): List[Mod] = {
      def validate() = {
        if (isLocal && !mod.tokens.head.is[LocalModifier]) syntaxError("illegal modifier for a local definition", at = mod)
        if (!mod.isInstanceOf[Mod.Quasi]) mods.foreach(m => if (m.productPrefix == mod.productPrefix) syntaxError("repeated modifier", at = mod))
        if (mod.hasAccessBoundary) mods.filter(_.hasAccessBoundary).foreach(mod => syntaxError("duplicate access qualifier", at = mod))
      }
      validate()
      mods :+ mod
    }
    // the only things that can come after $mod or $mods are either keywords or $pname/$tpname; the former is easy,
    // but in the case of the latter, we need to take care to not hastily parse those names as modifiers
    def continueLoop = ahead(token.is[`:`] || token.is[`=`] || token.is[EOF] || token.is[`[`] || token.is[`<:`] || token.is[`>:`] || token.is[`<%`])
    def loop(mods: List[Mod]): List[Mod] = token match {
      case _: Unquote => if (continueLoop) mods else loop(appendMod(mods, modifier()))
      case _: Modifier | _: Ellipsis => loop(appendMod(mods, modifier()))
      case _: `\n` if !isLocal => next(); loop(mods)
      case _                   => mods
    }
    loop(Nil)
  }

  /** {{{
   *  LocalModifiers ::= {LocalModifier}
   *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
   *  }}}
   */
  def localModifiers(): List[Mod] = modifiers(isLocal = true)

  /** {{{
   *  Annotations      ::= {`@' SimpleType {ArgumentExprs}}
   *  ConsrAnnotations ::= {`@' SimpleType ArgumentExprs}
   *  }}}
   */
  def annots(skipNewLines: Boolean, allowArgss: Boolean = true): List[Mod.Annot] = {
    val annots = new ListBuffer[Mod.Annot]
    while (token.is[`@`] || (token.is[Ellipsis] && ahead(token.is[`@`]))) {
      if (token.is[Ellipsis]) {
        annots += ellipsis(1, unquote[Mod.Annot], accept[`@`])
      } else {
        next()
        if (token.is[Unquote]) annots += unquote[Mod.Annot]
        else annots += atPos(in.prevTokenPos, auto)(Mod.Annot(constructorCall(exprSimpleType(), allowArgss)))
      }
      if (skipNewLines) newLineOpt()
    }
    annots.toList
  }

  def constructorAnnots(): List[Mod.Annot] = annots(skipNewLines = false, allowArgss = false)

/* -------- PARAMETERS ------------------------------------------- */

  /** {{{
   *  ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
   *  ParamClause       ::= [nl] `(' [Params] `)'
   *  Params            ::= Param {`,' Param}
   *  Param             ::= {Annotation} Id [`:' ParamType] [`=' Expr]
   *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
   *  ClassParamClause  ::= [nl] `(' [ClassParams] `)'
   *  ClassParams       ::= ClassParam {`,' ClassParam}
   *  ClassParam        ::= {Annotation}  [{Modifier} (`val' | `var')] Id [`:' ParamType] [`=' Expr]
   *  }}}
   */
  def paramClauses(ownerIsType: Boolean, ownerIsCase: Boolean = false): List[List[Term.Param]] = {
    var parsedImplicits = false
    def paramClause(): List[Term.Param] = token match {
      case _: `)` =>
        Nil
      case tok: Ellipsis if tok.rank == 2 =>
        List(ellipsis(2, unquote[Term.Param]))
      case _ =>
        if (token.is[`implicit`]) {
          next()
          parsedImplicits = true
        }
        commaSeparated(param(ownerIsCase, ownerIsType, isImplicit = parsedImplicits))
    }
    val paramss = new ListBuffer[List[Term.Param]]
    newLineOptWhenFollowedBy[`(`]
    while (!parsedImplicits && token.is[`(`]) {
      next()
      paramss += paramClause()
      accept[`)`]
      newLineOptWhenFollowedBy[`(`]
    }
    paramss.toList
  }

  /** {{{
   *  ParamType ::= Type | `=>' Type | Type `*'
   *  }}}
   */
  def paramType(): Type.Arg = autoPos(token match {
    case _: `=>` =>
      next()
      Type.Arg.ByName(typ())
    case _ =>
      val t = typ()
      if (!isRawStar) t
      else {
        next()
        Type.Arg.Repeated(t)
      }
  })

  def param(ownerIsCase: Boolean, ownerIsType: Boolean, isImplicit: Boolean): Term.Param = autoPos {
    var mods: List[Mod] = annots(skipNewLines = false)
    if (isImplicit) mods ++= List(atPos(in.tokenPos, in.prevTokenPos)(Mod.Implicit()))
    if (ownerIsType) {
      mods ++= modifiers()
      mods.getAll[Mod.Lazy].foreach { m =>
        syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead", at = m)
      }
    }
    val (isValParam, isVarParam) = (ownerIsType && token.is[`val`], ownerIsType && token.is[`var`])
    if (isValParam) { mods :+= atPos(in.tokenPos, in.tokenPos)(Mod.ValParam()); next() }
    if (isVarParam) { mods :+= atPos(in.tokenPos, in.tokenPos)(Mod.VarParam()); next() }
    val name = termName() match {
      case q: Term.Name.Quasi => q.become[Term.Param.Name.Quasi]
      case x => x
    }
    val tpt =
      if (token.isNot[`:`] && name.isInstanceOf[Term.Param.Name.Quasi])
        None
      else {
        accept[`:`]
        val tpt = paramType() match {
          case q: Type.Quasi => q.become[Type.Arg.Quasi]
          case x => x
        }
        if (tpt.isInstanceOf[Type.Arg.ByName]) {
          def mayNotBeByName(subj: String) =
            syntaxError(s"$subj parameters may not be call-by-name", at = name)
          val isLocalToThis: Boolean = {
            val isExplicitlyLocal = mods.accessBoundary.exists(_.isInstanceOf[Term.This])
            if (ownerIsCase) isExplicitlyLocal
            else isExplicitlyLocal || (!isValParam && !isVarParam)
          }
          if (ownerIsType && !isLocalToThis) {
            if (isVarParam)
              mayNotBeByName("`var'")
            else
              mayNotBeByName("`val'")
          } else if (isImplicit)
            mayNotBeByName("implicit")
        }
        Some(tpt)
      }
    val default =
      if (token.isNot[`=`]) None
      else {
        next()
        Some(expr())
      }
    Term.Param(mods, name, tpt, default)
  }

  /** {{{
   *  TypeParamClauseOpt    ::= [TypeParamClause]
   *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
   *  VariantTypeParam      ::= {Annotation} [`+' | `-'] TypeParam
   *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
   *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
   *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {<% Type} {":" Type}
   *  }}}
   */
  def typeParamClauseOpt(ownerIsType: Boolean, ctxBoundsAllowed: Boolean): List[Type.Param] = {
    newLineOptWhenFollowedBy[`[`]
    if (token.isNot[`[`]) Nil
    else inBrackets(commaSeparated(typeParam(ownerIsType, ctxBoundsAllowed)))
  }

  def typeParam(ownerIsType: Boolean, ctxBoundsAllowed: Boolean): Type.Param = autoPos {
    if (token.is[Unquote]) return unquote[Type.Param]
    var mods: List[Mod] = if (token.is[Ellipsis]) List(ellipsis(1, unquote[Mod])) else annots(skipNewLines = true)
    if (ownerIsType && token.is[Ident]) {
      if (isIdentOf("+")) {
        next()
        mods ++= List(atPos(in.prevTokenPos, in.prevTokenPos)(Mod.Covariant()))
      } else if (isIdentOf("-")) {
        next()
        mods ++= List(atPos(in.prevTokenPos, in.prevTokenPos)(Mod.Contravariant()))
      }
    }
    val nameopt =
      if (token.is[Ident]) typeName()
      else if (token.is[Unquote]) unquote[Type.Param.Name]
      else if (token.is[`_ `]) { next(); atPos(in.prevTokenPos, in.prevTokenPos)(Name.Anonymous()) }
      else syntaxError("identifier or `_' expected", at = token)
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    val tbounds = this.typeBounds()
    val vbounds = new ListBuffer[Type]
    val cbounds = new ListBuffer[Type]
    if (ctxBoundsAllowed) {
      while (token.is[`<%`]) {
        // TODO: dialect?
        // if (settings.future) {
        //   val msg = ("Use an implicit parameter instead.\n" +
        //              "Example: Instead of `def f[A <% Int](a: A)` " +
        //              "use `def f[A](a: A)(implicit ev: A => Int)`.")
        //   deprecationWarning(s"View bounds are deprecated. $msg")
        // }
        next()
        if (token.is[Ellipsis]) vbounds += ellipsis(1, unquote[Type])
        else vbounds += typ()
      }
      while (token.is[`:`]) {
        next()
        if (token.is[Ellipsis]) cbounds += ellipsis(1, unquote[Type])
        else cbounds += typ()
      }
    }
    Type.Param(mods, nameopt, tparams, tbounds, vbounds.toList, cbounds.toList)
  }

  /** {{{
   *  TypeBounds ::= [`>:' Type] [`<:' Type]
   *  }}}
   */
  def typeBounds() =
    autoPos(Type.Bounds(bound[`>:`], bound[`<:`]))

  def bound[T <: Token : TokenInfo]: Option[Type] =
    if (token.is[T]) { next(); Some(typ()) } else None

/* -------- DEFS ------------------------------------------- */


  /** {{{
   *  Import  ::= import ImportExpr {`,' ImportExpr}
   *  }}}
   */
  def importStmt(): Import = autoPos {
    accept[`import`]
    Import(commaSeparated(importer()))
  }

  /** {{{
   *  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
   *  }}}
   */
  def importer(): Importer = autoPos {
    val sid = stableId()
    def dotselectors = { accept[`.`]; Importer(sid, importees()) }
    sid match {
      case Term.Select(sid: Term.Ref, name: Term.Name) if sid.isStableId =>
        if (token.is[`.`]) dotselectors
        else Importer(sid, atPos(name, name)(Importee.Name(atPos(name, name)(Name.Indeterminate(name.value)))) :: Nil)
      case _ =>
        dotselectors
    }
  }

  /** {{{
   *  ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
   *  }}}
   */
  def importees(): List[Importee] =
    if (token.isNot[`{`]) List(importWildcardOrName())
    else inBraces(commaSeparated(importee()))

  def importWildcardOrName(): Importee = autoPos {
    if (token.is[`_ `]) { next(); Importee.Wildcard() }
    else if (token.is[Unquote]) Importee.Name(unquote[Name.Indeterminate.Quasi])
    else { val name = termName(); Importee.Name(atPos(name, name)(Name.Indeterminate(name.value))) }
  }

  /** {{{
   *  ImportSelector ::= Id [`=>' Id | `=>' `_']
   *  }}}
   */
  def importee(): Importee = autoPos {
    importWildcardOrName() match {
      case from: Importee.Name if token.is[`=>`] =>
        next()
        importWildcardOrName() match {
          case to: Importee.Name     => Importee.Rename(from.value, to.value)
          case to: Importee.Wildcard => Importee.Unimport(from.value)
          case other                 => unreachable(debug(other, other.show[Structure]))
        }
      // NOTE: this is completely nuts
      case from: Importee.Wildcard if token.is[`=>`] && ahead(token.is[`_ `]) =>
        nextTwice()
        from
      case other =>
        other
    }
  }

  def nonLocalDefOrDcl(): Stat = {
    val anns = annots(skipNewLines = true)
    val mods = anns ++ modifiers()
    defOrDclOrSecondaryCtor(mods) match {
      case s if s.isTemplateStat => s
      case other                 => syntaxError("is not a valid template statement", at = other)
    }
  }

  /** {{{
   *  Def    ::= val PatDef
   *           | var PatDef
   *           | def FunDef
   *           | type [nl] TypeDef
   *           | TmplDef
   *  Dcl    ::= val PatDcl
   *           | var PatDcl
   *           | def FunDcl
   *           | type [nl] TypeDcl
   *  }}}
   */
  def defOrDclOrSecondaryCtor(mods: List[Mod]): Stat = {
    mods.getAll[Mod.Lazy].foreach { m =>
      if (token.isNot[`val`]) syntaxError("lazy not allowed here. Only vals can be lazy", at = m)
    }
    token match {
      case _: `val` | _: `var` =>
        patDefOrDcl(mods)
      case _: `def` =>
        funDefOrDclOrSecondaryCtor(mods)
      case _: `type` =>
        typeDefOrDcl(mods)
      case _ =>
        tmplDef(mods)
    }
  }

  /** {{{
   *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
   *  ValDcl ::= Id {`,' Id} `:' Type
   *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
   *  }}}
   */
  def patDefOrDcl(mods: List[Mod]): Stat = atPos(mods, auto) {
    val isMutable = token.is[`var`]
    next()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2().require[Pat]).map {
      case q: Pat.Quasi => q.become[Pat.Var.Term.Quasi]
      case name: Term.Name => atPos(name, name)(Pat.Var.Term(name))
      case pat => pat
    }
    val tp: Option[Type] = typedOpt()

    if (tp.isEmpty || token.is[`=`]) {
      accept[`=`]
      val rhs =
        if (token.is[`_ `] && tp.nonEmpty && isMutable && lhs.forall(_.isInstanceOf[Pat.Var.Term])) {
          next()
          None
        } else Some(expr())

      if (isMutable) Defn.Var(mods, lhs, tp, rhs)
      else Defn.Val(mods, lhs, tp, rhs.get)
    } else {
      mods.getAll[Mod.Lazy].foreach { m => syntaxError("lazy values may not be abstract", at = m) }
      val ids = lhs.map {
        case name: Pat.Var.Term => name
        case other              => syntaxError("pattern definition may not be abstract", at = other)
      }

      if (isMutable) Decl.Var(mods, ids, tp.get)
      else Decl.Val(mods, ids, tp.get)
    }
  }

  /** {{{
   *  FunDef ::= FunSig [`:' Type] `=' [`macro'] Expr
   *          |  FunSig [nl] `{' Block `}'
   *          |  `this' ParamClause ParamClauses
   *                 (`=' ConstrExpr | [nl] ConstrBlock)
   *  FunDcl ::= FunSig [`:' Type]
   *  FunSig ::= id [FunTypeParamClause] ParamClauses
   *  }}}
   */
  def funDefOrDclOrSecondaryCtor(mods: List[Mod]): Stat = {
    if (ahead(token.isNot[`this`])) funDefRest(mods)
    else secondaryCtor(mods)
  }

  def funDefRest(mods: List[Mod]): Stat = atPos(mods, auto) {
    accept[`def`]
    val name = termName()
    def warnProcedureDeprecation =
      deprecationWarning(s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.", at = name)
    val tparams = typeParamClauseOpt(ownerIsType = false, ctxBoundsAllowed = true)
    val paramss = paramClauses(ownerIsType = false).require[Seq[Seq[Term.Param]]]
    newLineOptWhenFollowedBy[`{`]
    val restype = fromWithinReturnType(typedOpt())
    if (token.is[StatSep] || token.is[`}`]) {
      if (restype.isEmpty) {
        warnProcedureDeprecation
        Decl.Def(mods, name, tparams, paramss, atPos(in.tokenPos, in.prevTokenPos)(Type.Name("Unit"))) // TODO: hygiene!
      } else
        Decl.Def(mods, name, tparams, paramss, restype.get)
    } else if (restype.isEmpty && token.is[`{`]) {
      warnProcedureDeprecation
      Defn.Def(mods, name, tparams, paramss, Some(atPos(in.tokenPos, in.prevTokenPos)(Type.Name("Unit"))), expr()) // TODO: hygiene!
    } else {
      var isMacro = false
      val rhs = {
        if (token.is[`=`]) {
          next()
          isMacro = token.is[`macro`]
          if (isMacro) next()
        } else {
          accept[`=`]
        }
        expr()
      }
      if (isMacro) Defn.Macro(mods, name, tparams, paramss, restype, rhs)
      else Defn.Def(mods, name, tparams, paramss, restype, rhs)
    }
  }

  /** {{{
   *  TypeDef ::= type Id [TypeParamClause] `=' Type
   *            | FunSig `=' Expr
   *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
   *  }}}
   */
  def typeDefOrDcl(mods: List[Mod]): Member.Type with Stat = atPos(mods, auto) {
    accept[`type`]
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    def aliasType() = Defn.Type(mods, name, tparams, typ())
    def abstractType() = Decl.Type(mods, name, tparams, typeBounds())
    token match {
      case _: `=` => next(); aliasType()
      case _: `>:` | _: `<:` | _: `,` | _: `}` => abstractType()
      case token if token.is[StatSep] => abstractType()
      case _ => syntaxError("`=', `>:', or `<:' expected", at = token)
    }
  }

  /** Hook for IDE, for top-level classes/objects. */
  def topLevelTmplDef: Member with Stat =
    tmplDef(annots(skipNewLines = true) ++ modifiers())

  /** {{{
   *  TmplDef ::= [case] class ClassDef
   *            |  [case] object ObjectDef
   *            |  [override] trait TraitDef
   *  }}}
   */
  def tmplDef(mods: List[Mod]): Member with Stat = {
    mods.getAll[Mod.Lazy].foreach { m => syntaxError("classes cannot be lazy", at = m) }
    token match {
      case _: `trait` =>
        traitDef(mods)
      case _: `class ` =>
        classDef(mods)
      case _: `case` if ahead(token.is[`class `]) =>
        val casePos = in.tokenPos
        next()
        classDef(mods :+ atPos(casePos, casePos)(Mod.Case()))
      case _: `object` =>
        objectDef(mods)
      case _: `case` if ahead(token.is[`object`]) =>
        val casePos = in.tokenPos
        next()
        objectDef(mods :+ atPos(casePos, casePos)(Mod.Case()))
      case _ =>
        syntaxError(s"expected start of definition", at = token)
    }
  }

  /** {{{
   *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
   *  }}}
   */
  def traitDef(mods: List[Mod]): Defn.Trait = atPos(mods, auto) {
    accept[`trait`]
    Defn.Trait(mods, typeName(),
               typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false),
               primaryCtor(OwnedByTrait),
               templateOpt(OwnedByTrait))
  }

  /** {{{
   *  ClassDef ::= Id [TypeParamClause] {Annotation}
   *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
   *  }}}
   */
  def classDef(mods: List[Mod]): Defn.Class = atPos(mods, auto) {
    accept[`class `]
    // TODO:
    // if (ofCaseClass && token.isNot[`(`])
    //  syntaxError(token.offset, "case classes without a parameter list are not allowed;\n"+
    //                             "use either case objects or case classes with an explicit `()' as a parameter list.")
    // TODO:
    // if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods.isImplicit)))) {
    //  token match {
    //    case _: `[` => syntaxError("no type parameters allowed here")
    //    case _: EOF => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
    //    case _      => syntaxError(start, "auxiliary constructor needs non-implicit parameter list")
    //  }
    // }
    Defn.Class(mods, typeName(),
               typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = true),
               primaryCtor(if (mods.has[Mod.Case]) OwnedByCaseClass else OwnedByClass), templateOpt(OwnedByClass))
  }

  /** {{{
   *  ObjectDef       ::= Id ClassTemplateOpt
   *  }}}
   */
  def objectDef(mods: List[Mod]): Defn.Object = atPos(mods, auto) {
    accept[`object`]
    Defn.Object(mods, termName(), templateOpt(OwnedByObject))
  }

/* -------- CONSTRUCTORS ------------------------------------------- */

  // TODO: we need to store some string in Ctor.Name in order to represent constructor calls (which also use Ctor.Name)
  // however, when representing constructor defns, we can't easily figure out that name
  // a natural desire would be to have this name equal to the name of the enclosing class/trait/object
  // but unfortunately we can't do that, because we can create ctors in isolation from their enclosures
  // therefore, I'm going to use `Term.Name("this")` here for the time being

  def primaryCtor(owner: TemplateOwner): Ctor.Primary = autoPos {
    if (owner.isClass) {
      val mods = constructorAnnots() ++ accessModifierOpt()
      val name = atPos(in.tokenPos, in.prevTokenPos)(Ctor.Name("this"))
      val paramss = paramClauses(ownerIsType = true, owner == OwnedByCaseClass)
      Ctor.Primary(mods, name, paramss)
    } else if (owner.isTrait) {
      Ctor.Primary(Nil, atPos(in.tokenPos, in.prevTokenPos)(Ctor.Name("this")), Nil)
    } else {
      unreachable(debug(owner))
    }
  }

  def secondaryCtor(mods: List[Mod]): Ctor.Secondary = atPos(mods, auto) {
    accept[`def`]
    val thisPos = in.tokenPos
    accept[`this`]
    // TODO: ownerIsType = true is most likely a bug here
    // secondary constructors can't have val/var parameters
    val paramss = paramClauses(ownerIsType = true)
    newLineOptWhenFollowedBy[`{`]
    val body = token match {
      case _: `{` => constrBlock()
      case _      => accept[`=`]; constrExpr()
    }
    Ctor.Secondary(mods, atPos(thisPos, thisPos)(Ctor.Name("this")), paramss, body)
  }

  def quasiquoteCtor(): Ctor = autoPos {
    val anns = annots(skipNewLines = true)
    val mods = anns ++ modifiers()
    accept[`def`]
    val name = atPos(in.tokenPos, in.tokenPos)(Ctor.Name("this"))
    accept[`this`]
    val paramss = paramClauses(ownerIsType = true)
    newLineOptWhenFollowedBy[`{`]
    if (token.is[EOF]) {
      Ctor.Primary(mods, name, paramss)
    } else {
      val body = token match {
        case _: `{` => constrBlock()
        case _      => accept[`=`]; constrExpr()
      }
      Ctor.Secondary(mods, name, paramss, body)
    }
  }

  /** {{{
   *  ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
   *  }}}
   */
  def constrBlock(): Term.Block = autoPos {
    accept[`{`]
    val supercall = selfInvocation()
    val stats =
      if (!token.is[StatSep]) Nil
      else { next(); blockStatSeq() }
    accept[`}`]
    Term.Block(supercall +: stats)
  }

  /** {{{
   *  ConstrExpr      ::=  SelfInvocation
   *                    |  ConstrBlock
   *  }}}
   */
  def constrExpr(): Term =
    if (token.is[`{`]) constrBlock()
    else selfInvocation()

  /** {{{
   *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
   *  }}}
   */
  def selfInvocation(): Term =
    if (token.is[Unquote])
      unquote[Term]
    else {
      var result: Term = autoPos(Ctor.Name("this"))
      accept[`this`]
      newLineOptWhenFollowedBy[`{`]
      while (token.is[`(`] || token.is[`{`]) {
        result = atPos(result, auto)(Term.Apply(result, argumentExprs()))
        newLineOptWhenFollowedBy[`{`]
      }
      result
    }

  def constructorCall(tpe: Type, allowArgss: Boolean = true): Ctor.Call = {
    object Types {
      def unapply(tpes: Seq[Type.Arg]): Option[Seq[Type]] = {
        if (tpes.forall(t => t.isInstanceOf[Type] || t.isInstanceOf[Type.Arg.Quasi])) Some(tpes.map {
          case q: Type.Arg.Quasi => q.become[Type.Quasi]
          case t: Type => t.require[Type]
        })
        else None
      }
    }
    def convert(tpe: Type): Ctor.Call = {
      // TODO: we should really think of a different representation for constructor invocations...
      // if anything, this mkCtorRefFunction is a testament to how problematic our current encoding is
      // the Type.ApplyInfix => Term.ApplyType conversion is weird as well
      def mkCtorRefFunction(tpe: Type) = {
        val arrow = scannerTokens.slice(tpe.tokens.head.index, tpe.tokens.last.index + 1).find(_.is[`=>`]).get
        atPos(arrow, arrow)(Ctor.Ref.Function(atPos(arrow, arrow)(Ctor.Name("=>"))))
      }
      atPos(tpe, tpe)(tpe match {
        case q: Type.Quasi => q.become[Ctor.Ref.Name.Quasi]
        case Type.Name(value) => Ctor.Name(value)
        case Type.Select(qual, name: Type.Name.Quasi) => Ctor.Ref.Select(qual, atPos(name, name)(name.become[Ctor.Name.Quasi]))
        case Type.Select(qual, name) => Ctor.Ref.Select(qual, atPos(name, name)(Ctor.Name(name.value)))
        case Type.Project(qual, name: Type.Name.Quasi) => Ctor.Ref.Project(qual, atPos(name, name)(name.become[Ctor.Name.Quasi]))
        case Type.Project(qual, name) => Ctor.Ref.Project(qual, atPos(name, name)(Ctor.Name(name.value)))
        case Type.Function(Types(params), ret) => Term.ApplyType(mkCtorRefFunction(tpe), params :+ ret)
        case Type.Annotate(tpe, annots) => Term.Annotate(convert(tpe), annots)
        case Type.Apply(tpe, args) => Term.ApplyType(convert(tpe), args)
        case Type.ApplyInfix(lhs, op, rhs) => Term.ApplyType(convert(op), List(lhs, rhs))
        case _ => syntaxError("this type can't be used in a constructor call", at = tpe)
      })
    }
    var result = convert(tpe)
    var done = false
    while (token.is[`(`] && !done) {
      result = atPos(result, auto)(Term.Apply(result, argumentExprs()))
      if (!allowArgss) done = true
    }
    result
  }

/* -------- TEMPLATES ------------------------------------------- */

  sealed trait TemplateOwner {
    def isTerm = this eq OwnedByObject
    def isClass = (this eq OwnedByCaseClass) || (this eq OwnedByClass)
    def isTrait = this eq OwnedByTrait
  }
  object OwnedByTrait extends TemplateOwner
  object OwnedByCaseClass extends TemplateOwner
  object OwnedByClass extends TemplateOwner
  object OwnedByObject extends TemplateOwner

  /** {{{
   *  ClassParents       ::= ModType {`(' [Exprs] `)'} {with ModType}
   *  TraitParents       ::= ModType {with ModType}
   *  }}}
   */
  def templateParents(): List[Ctor.Call] = {
    val parents = new ListBuffer[Ctor.Call]
    def readAppliedParent() =
      if (token.is[Ellipsis]) parents += ellipsis(1, unquote[Ctor.Call])
      else parents += constructorCall(startModType())
    readAppliedParent()
    while (token.is[`with`]) { next(); readAppliedParent() }
    parents.toList
  }

  /** {{{
   *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
   *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
   *  EarlyDefs     ::= `{' [EarlyDef {semi EarlyDef}] `}'
   *  EarlyDef      ::= Annotations Modifiers PatDef
   *  }}}
   */
  def template(): Template = autoPos {
    newLineOptWhenFollowedBy[`{`]
    if (token.is[`{`]) {
      // @S: pre template body cannot stub like post body can!
      val (self, body) = templateBody(isPre = true)
      if (token.is[`with`] && self.name.isInstanceOf[Name.Anonymous] && self.decltpe.isEmpty) {
        val edefs = body.map(ensureEarlyDef)
        next()
        val parents = templateParents()
        val (self1, body1) = templateBodyOpt(parenMeansSyntaxError = false)
        Template(edefs, parents, self1, body1)
      } else {
        Template(Nil, Nil, self, Some(body))
      }
    } else if (token.is[Unquote]) {
      unquote[Template]
    } else {
      val parents = templateParents()
      val (self, body) = templateBodyOpt(parenMeansSyntaxError = false)
      Template(Nil, parents, self, body)
    }
  }

  def ensureEarlyDef(tree: Stat): Stat = tree match {
    case v: Stat.Quasi => v
    case v: Defn.Val => v
    case v: Defn.Var => v
    case t: Defn.Type => t
    case other => syntaxError("not a valid early definition", at = other)
  }

  /** {{{
   *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
   *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
   *  TraitExtends     ::= `extends' | `<:'
   *  }}}
   */
  def templateOpt(owner: TemplateOwner): Template = {
    if (token.is[`extends`] || (token.is[`<:`] && owner.isTrait)) {
      next()
      template()
    } else {
      val startPos = in.tokenPos
      val (self, body) = templateBodyOpt(parenMeansSyntaxError = !owner.isClass)
      atPos(startPos, auto)(Template(Nil, Nil, self, body))
    }
  }

  /** {{{
   *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateBody(isPre: Boolean): (Term.Param, List[Stat]) =
    inBraces(templateStatSeq(isPre = isPre))

  def templateBodyOpt(parenMeansSyntaxError: Boolean): (Term.Param, Option[List[Stat]]) = {
    newLineOptWhenFollowedBy[`{`]
    if (token.is[`{`]) {
      val (self, body) = templateBody(isPre = false)
      (self, Some(body))
    } else {
      if (token.is[`(`]) {
        if (parenMeansSyntaxError) syntaxError("traits or objects may not have parameters", at = token)
        else syntaxError("unexpected opening parenthesis", at = token)
      }
      (autoPos(Term.Param(Nil, autoPos(Name.Anonymous()), None, None)), None)
    }
  }

  /** {{{
   *  Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
   *  }}}
   */
  def refinement(): List[Stat] = inBraces(refineStatSeq())

  def existentialStats(): List[Stat] = refinement() map {
    case stat if stat.isExistentialStat => stat
    case other                          => syntaxError("not a legal existential clause", at = other)
  }

/* -------- STATSEQS ------------------------------------------- */

  def statSeq[T <: Tree: AstInfo](statpf: PartialFunction[Token, T],
                                      errorMsg: String = "illegal start of definition"): List[T] = {
    val stats = new ListBuffer[T]
    while (!token.is[StatSeqEnd]) {
      def isDefinedInEllipsis = { if (token.is[`(`] || token.is[`{`]) next(); statpf.isDefinedAt(token) }
      if (statpf.isDefinedAt(token) || (token.is[Ellipsis] && ahead(isDefinedInEllipsis)))
        stats += statpf(token)
      else if (!token.is[StatSep])
        syntaxError(errorMsg, at = token)
      acceptStatSepOpt()
    }
    stats.toList
  }

  /** {{{
   *  TopStatSeq ::= TopStat {semi TopStat}
   *  TopStat ::= Annotations Modifiers TmplDef
   *            | Packaging
   *            | package object objectDef
   *            | Import
   *            |
   *  }}}
   */
  def topStatSeq(): List[Stat] = statSeq(topStat, errorMsg = "expected class or object definition")
  def topStat: PartialFunction[Token, Stat] = {
    case token if token.is[Ellipsis] =>
      ellipsis(1, unquote[Stat])
    case token if token.is[Unquote] =>
      unquote[Stat]
    case token if token.is[`package `]  =>
      packageOrPackageObjectDef()
    case token if token.is[`import`] =>
      importStmt()
    case token if token.is[TemplateIntro] =>
      topLevelTmplDef
  }

  /** {{{
   *  TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStats
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateStatSeq(isPre : Boolean): (Term.Param, List[Stat]) = {
    var self: Term.Param = autoPos(Term.Param(Nil, autoPos(Name.Anonymous()), None, None))
    var firstOpt: Option[Stat] = None
    if (token.is[ExprIntro]) {
      val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
      if (token.is[`=>`]) {
        first match {
          case q: Term.Quasi =>
            self = q.become[Term.Param.Quasi]
          case name: Term.Placeholder =>
            self = atPos(first, first)(Term.Param(Nil, atPos(name, name)(Name.Anonymous()), None, None))
          case name @ Term.This(Name.Anonymous()) =>
            self = atPos(first, first)(Term.Param(Nil, atPos(name, name)(Name.Anonymous()), None, None))
          case name: Term.Name =>
            self = atPos(first, first)(Term.Param(Nil, name, None, None))
          case Term.Ascribe(name: Term.Placeholder, tpt) =>
            self = atPos(first, first)(Term.Param(Nil, atPos(name, name)(Name.Anonymous()), Some(tpt), None))
          case Term.Ascribe(name @ Term.This(Name.Anonymous()), tpt) =>
            self = atPos(first, first)(Term.Param(Nil, atPos(name, name)(Name.Anonymous()), Some(tpt), None))
          case Term.Ascribe(name: Term.Name, tpt) =>
            self = atPos(first, first)(Term.Param(Nil, name, Some(tpt), None))
          case _ =>
        }
        next()
      } else {
        firstOpt = Some(first match {
          case q: Term.Quasi => q.become[Stat.Quasi]
          case other => other
        })
        acceptStatSepOpt()
      }
    }
    (self, firstOpt ++: templateStats())
  }

  /** {{{
   *  TemplateStats    ::= TemplateStat {semi TemplateStat}
   *  TemplateStat     ::= Import
   *                     | Annotations Modifiers Def
   *                     | Annotations Modifiers Dcl
   *                     | Expr1
   *                     | super ArgumentExprs {ArgumentExprs}
   *                     |
   *  }}}
   */
  def templateStats(): List[Stat] = statSeq(templateStat)
  def templateStat: PartialFunction[Token, Stat] = {
    case token if token.is[Ellipsis] =>
      ellipsis(1, unquote[Stat])
    case token if token.is[`import`] =>
      importStmt()
    case token if token.is[DefIntro] =>
      nonLocalDefOrDcl()
    case token if token.is[ExprIntro] =>
      expr(InTemplate)
  }

  /** {{{
   *  RefineStatSeq    ::= RefineStat {semi RefineStat}
   *  RefineStat       ::= Dcl
   *                     | type TypeDef
   *                     |
   *  }}}
   */
  def refineStatSeq(): List[Stat] = {
    val stats = new ListBuffer[Stat]
    while (!token.is[StatSeqEnd]) {
      stats ++= refineStat()
      if (token.isNot[`}`]) acceptStatSep()
    }
    stats.toList
  }

  def refineStat(): Option[Stat] =
    if (token.is[Ellipsis]) {
      Some(ellipsis(1, unquote[Stat]))
    } else if (token.is[DclIntro]) {
      defOrDclOrSecondaryCtor(Nil) match {
        case stat if stat.isRefineStat => Some(stat)
        case other                     => syntaxError("is not a valid refinement declaration", at = other)
      }
    } else if (!token.is[StatSep]) {
      syntaxError(
        "illegal start of declaration"+
        (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
         else ""), at = token)
    } else None

  def localDef(implicitMod: Option[Mod.Implicit]): Stat = {
    val mods = (implicitMod ++: annots(skipNewLines = true)) ++ localModifiers()
    if (mods forall { case _: Mod.Implicit | _: Mod.Lazy | _: Mod.Annot => true; case _ => false })
      (defOrDclOrSecondaryCtor(mods) match {
        case stat if stat.isBlockStat => stat
        case other                    => syntaxError("is not a valid block statement", at = other)
      })
    else
      (tmplDef(mods) match {
        case stat if stat.isBlockStat => stat
        case other                    => syntaxError("is not a valid block statement", at = other)
      })
  }

  /** {{{
   *  BlockStatSeq ::= { BlockStat semi } [ResultExpr]
   *  BlockStat    ::= Import
   *                 | Annotations [implicit] [lazy] Def
   *                 | Annotations LocalModifiers TmplDef
   *                 | Expr1
   *                 |
   *  }}}
   */
  def blockStatSeq(): List[Stat] = {
    val stats = new ListBuffer[Stat]
    while (!token.is[StatSeqEnd] && !token.is[CaseDefEnd]) {
      if (token.is[`import`]) {
        stats += importStmt()
        acceptStatSepOpt()
      }
      else if (token.is[DefIntro] && !token.is[NonlocalModifier]) {
        if (token.is[`implicit`]) {
          val implicitPos = in.tokenPos
          next()
          if (token.is[Ident]) stats += implicitClosure(InBlock)
          else stats += localDef(Some(atPos(implicitPos, implicitPos)(Mod.Implicit())))
        } else {
          stats += localDef(None)
        }
        acceptStatSepOpt()
      }
      else if (token.is[ExprIntro]) {
        stats += expr(InBlock)
        if (!token.is[CaseDefEnd]) acceptStatSep()
      }
      else if (token.is[StatSep]) {
        next()
      }
      else if (token.is[Ellipsis]) {
        stats += ellipsis(1, unquote[Stat])
      }
      else {
        syntaxError("illegal start of statement", at = token)
      }
    }
    stats.toList
  }


  def packageOrPackageObjectDef(): Stat = autoPos {
    require(token.is[`package `] && debug(token))
    if (ahead(token.is[`object`])) packageObjectDef()
    else packageDef()
  }

  def packageDef(): Pkg = autoPos {
    accept[`package `]
    Pkg(qualId(), inBracesOrNil(topStatSeq()))
  }

  def packageObjectDef(): Pkg.Object = autoPos {
    accept[`package `]
    accept[`object`]
    Pkg.Object(Nil, termName(), templateOpt(OwnedByObject))
  }

  /** {{{
   *  CompilationUnit ::= {package QualId semi} TopStatSeq
   *  }}}
   */
  def compilationUnit(): Source = autoPos {
    def inBracelessPackage() = token.is[`package `] && !ahead(token.is[`object`]) && ahead{ qualId(); token.isNot[`{`] }
    def bracelessPackageStats(): Seq[Stat] = {
      if (token.is[EOF]) {
        Nil
      } else if (token.is[StatSep]) {
        next()
        bracelessPackageStats()
      } else if (token.is[`package `] && !ahead(token.is[`object`])) {
        val startPos = in.tokenPos
        accept[`package `]
        val qid = qualId()
        if (token.is[`{`]) {
          val pkg = atPos(startPos, auto)(Pkg(qid, inBraces(topStatSeq())))
          acceptStatSepOpt()
          pkg +: bracelessPackageStats()
        } else {
          List(atPos(startPos, auto)(Pkg(qid, bracelessPackageStats())))
        }
      } else if (token.is[`{`]) {
        inBraces(topStatSeq())
      } else {
        topStatSeq()
      }
    }
    if (inBracelessPackage) {
      val startPos = in.tokenPos
      accept[`package `]
      Source(List(atPos(startPos, auto)(Pkg(qualId(), bracelessPackageStats()))))
    } else {
      Source(topStatSeq())
    }
  }
}

object ScalametaParser {
  def toParse[T](fn: ScalametaParser => T): Parse[T] = new Parse[T] {
    def apply(input: Input)(implicit dialect: Dialect): Parsed[T] = {
      try {
        val parser = new ScalametaParser(input)(dialect)
        Parsed.Success(fn(parser))
      } catch {
        case details @ ParseException(pos, message) => Parsed.Error(pos, message, details)
      }
    }
  }
}

class Location private(val value: Int) extends AnyVal
object Location {
  val Local      = new Location(0)
  val InBlock    = new Location(1)
  val InTemplate = new Location(2)
}

object InfixMode extends Enumeration {
  val FirstOp, LeftOp, RightOp = Value
}
