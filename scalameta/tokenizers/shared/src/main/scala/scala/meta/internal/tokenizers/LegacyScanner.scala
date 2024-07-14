package scala.meta
package internal
package tokenizers

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.inputs._
import scala.meta.internal.inputs._
import scala.meta.internal.tokens.Chars._

import scala.annotation.switch
import scala.annotation.tailrec
import scala.collection.mutable

class LegacyScanner(input: Input, dialect: Dialect)(implicit reporter: Reporter) {

  import LegacyToken._

  private val unquoteDialect = dialect.unquoteParentDialect

  private val curr: LegacyTokenData = new LegacyTokenData
  private val next: LegacyTokenData = new LegacyTokenData
  private var prev: LegacyTokenData = curr

  private val reader: CharArrayReader = new CharArrayReader(input, dialect, reporter)

  import curr._
  import reader._
  import reporter._

  private var openComments = 0

  @tailrec
  private def skipLineComment(): Unit = ch match {
    case SU | CR | LF =>
    case '$' if isUnquoteNextNoDollar() =>
      syntaxError("can't unquote into single-line comments", at = begCharOffset)
    case _ => nextCommentChar(); skipLineComment()
  }
  private def maybeOpen(): Unit = {
    nextCommentChar()
    if (ch == '*') {
      nextCommentChar()
      openComments += 1
    }
  }
  private def maybeClose(): Boolean = {
    nextCommentChar()
    (ch == '/') && {
      openComments -= 1
      val close = openComments == 0
      if (close) nextChar() else nextCommentChar()
      close
    }
  }
  @tailrec
  private final def skipNestedComments(): Unit = ch match {
    case '/' => maybeOpen(); skipNestedComments()
    case '*' => if (!maybeClose()) skipNestedComments()
    case SU => incompleteInputError("unclosed comment", at = offset)
    case '$' if isUnquoteNextNoDollar() =>
      syntaxError("can't unquote into multi-line comments", at = begCharOffset)
    case _ => nextCommentChar(); skipNestedComments()
  }

  private def isAtEnd = endCharOffset >= buf.length

  /**
   * A character buffer for literals
   */
  private val cbuf = new java.lang.StringBuilder

  /**
   * append Unicode character to "cbuf" buffer
   */
  private def putChar(c: Int): Unit = cbuf.appendCodePoint(c)

  private def putCharAndNext(c: Int): Unit = {
    putChar(c)
    nextChar()
  }

  @inline
  private def putCharAndNext(): Unit = putCharAndNext(ch)

  private def putCharAndNextRaw(): Unit = {
    putChar(ch)
    nextRawChar()
  }

  /**
   * Determines whether this scanner should emit identifier deprecation warnings, e.g. when seeing
   * `macro' or `then', which are planned to become keywords in future versions of Scala.
   */
  private def emitIdentifierDeprecationWarnings = true

  /** Clear buffer and set name and token */
  private def finishNamed(isBackquoted: Boolean = false): Unit = curr
    .setIdentifier(getAndResetCBuf(), dialect, check = !isBackquoted) { x =>
      if (x.token == IDENTIFIER && emitIdentifierDeprecationWarnings) deprecationWarning(
        s"${x.strVal} is now a reserved word; usage as an identifier is deprecated",
        at = x.token
      )
    }

  /* much like endOffset, end is inclusive */
  private def finishComposite(token: LegacyToken, endExclusive: Offset): Unit = {
    val start = offset
    curr.token = token
    curr.strVal = new String(input.chars, start, endExclusive - start)
    curr.endOffset = endExclusive
    reader.nextCharFrom(endExclusive)
  }

  /** Clear buffer and set string */
  private def setStrVal(): Unit = strVal = getAndResetCBuf()

  private def setTokStrVal(tokenValue: Int): Unit = {
    setStrVal()
    token = tokenValue
  }

  private def resetCBuf(): Unit = cbuf.setLength(0)

  private def getAndResetCBuf(): String =
    try cbuf.toString
    finally resetCBuf()

  /**
   * a stack of tokens which indicates whether line-ends can be statement separators also used for
   * keeping track of nesting levels. We keep track of the closing symbol of a region. This can be
   *   - RPAREN if region starts with '('
   *   - RBRACKET if region starts with '['
   *   - RBRACE if region starts with '{'
   *   - ARROW if region starts with `case`
   *   - STRINGLIT if region is a string interpolation expression starting with '${' (the STRINGLIT
   *     appears twice in succession on the stack iff the expression is a multiline string literal).
   */
  private var sepRegions: List[LegacyToken] = List()

  @inline
  private def pushSepRegions(sr: LegacyToken) = sepRegions = sr :: sepRegions

  private def popSepRegionsIf(token: LegacyToken) = sepRegions match {
    case head :: tail if head == token => sepRegions = tail; true
    case _ => false
  }

  @tailrec
  private def popSepRegionsUntil(token: LegacyToken): Boolean = sepRegions match {
    case head :: tail => sepRegions = tail; head == token || popSepRegionsUntil(token)
    case _ => false
  }

  /**
   * A map of upcoming xml literal parts that are left to be returned in nextToken().
   *
   * The keys are offset start positions of an xml literal and the values are the respective offset
   * end positions and a boolean indicating if the part is the last part.
   */
  private val upcomingXmlLiteralParts = mutable.Map.empty[Offset, (Offset, Boolean)]

// Get next token ------------------------------------------------------------

  def initialize(bof: Boolean = false): Unit = if (endCharOffset == 0) {
    nextChar()
    if (bof && '#' == ch && !wasMultiChar && buf(endCharOffset) == '!') {
      next.offset = begCharOffset
      do putCharAndNext() while (ch != CR && ch != LF && ch != FF && ch != SU)
      next.strVal = getAndResetCBuf()
      next.endOffset = begCharOffset
      next.token = SHEBANG
    }
  }

  @inline
  def nextTokenOrEof(): LegacyTokenData = nextToken { prev.token = PASTEOF }

  /**
   * Produce next token, filling curr TokenData fields of Scanner.
   */
  def nextToken(): LegacyTokenData = nextToken(throw new UnexpectedInputEndException(prev))

  private def nextToken(onEof: => Unit): LegacyTokenData = {
    val lastToken = prev.token
    // Adapt sepRegions according to last token
    (lastToken: @switch) match {
      case EOF | PASTEOF => onEof; return prev
      case LPAREN => pushSepRegions(RPAREN)
      case LBRACKET => pushSepRegions(RBRACKET)
      case LBRACE => pushSepRegions(RBRACE)
      case CASE => pushSepRegions(ARROW)
      case RBRACE => popSepRegionsUntil(RBRACE)
      case RBRACKET | RPAREN | ARROW => popSepRegionsIf(lastToken)
      case STRINGLIT => popSepRegionsIf(lastToken) && popSepRegionsIf(STRINGPART)
      case _ =>
    }

    // Read a token or copy it from `next` tokenData
    if (prev eq next) next.token = EMPTY
    if (next.token == EMPTY) {
      resetCBuf()
      offset = begCharOffset
      endOffset = -1
      fetchToken()
      def setEnd(tok: LegacyTokenData): Boolean = {
        if (tok.endOffset >= tok.offset) return false
        tok.endOffset = if (endCharOffset >= buf.length && ch == SU) buf.length else begCharOffset
        true
      }
      setEnd(curr) || next.token == EMPTY || setEnd(next)
      prev = curr
    } else prev = next
    prev
  }

  /**
   * read next token, filling TokenData fields of Scanner.
   */
  private final def fetchToken(): Unit = {
    sepRegions match {
      case STRINGLIT :: tail => // STRINGPART follows STRINGLIT in multiline interpolation
        return if (token == STRINGPART) getStringSplice()
        else getStringPart(multiLine = tail.headOption.contains(STRINGPART))
      case _ =>
    }
    if (fetchXmlPart()) return

    @inline
    def getIdentRestCheckInterpolation() = {
      getIdentRest()
      if (ch == '"' && token == IDENTIFIER) token = INTERPOLATIONID
    }

    (ch: @switch) match {
      case ' ' =>
        nextChar()
        strVal = " "
        token = WHITESPACE_SPC
      case '\t' =>
        nextChar()
        strVal = "\t"
        token = WHITESPACE_TAB
      case FF =>
        nextChar()
        strVal = "\f"
        token = WHITESPACE_FF
      case LF =>
        nextChar()
        strVal = "\n"
        token = WHITESPACE_LF
      case CR =>
        nextChar()
        if (ch != LF) {
          strVal = "\r"
          token = WHITESPACE_CR
        } else if (wasMultiChar) {
          nextChar()
          strVal = "\n"
          token = WHITESPACE_LF
        } else {
          nextChar()
          strVal = "\r\n"
          token = WHITESPACE_CRLF
        }
      // nextToken()
      case
          // uppercase alpha
          'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' |
          'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' |
          // lowercase alpha
          'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' |
          'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
          // other ident chars
          '_' =>
        putCharAndNext()
        getIdentRestCheckInterpolation()
      case '$' =>
        if (isUnquoteNextNoDollar()) getUnquote()
        else {
          putCharAndNext()
          if (dialect.allowSpliceAndQuote && peekNonWhitespace().ch == '{')
            setTokStrVal(MACROSPLICE)
          else getIdentRestCheckInterpolation()
        }
      case '<' => // is XMLSTART?
        def fetchLT() = {
          val last = if (endCharOffset >= 2) buf(endCharOffset - 2) else ' '
          nextChar()
          last match {
            case ' ' | '\t' | '\n' | '{' | '(' | '>' if isNameStart(ch) || ch == '!' || ch == '?' =>
              if (dialect.allowXmlLiterals) getXml()
              else syntaxError("xml literals are not supported", at = offset)
            case _ =>
              // Console.println("found '<', but last is '"+in.last+"'"); // DEBUG
              putChar('<')
              getOperatorRest()
          }
        }
        fetchLT()
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | /*'<' | */
          '>' | '?' | ':' | '=' | '&' | '|' | '\\' =>
        putCharAndNext()
        getOperatorRest()
      case '/' =>
        nextChar()
        ch match {
          case '/' =>
            token = COMMENT
            skipLineComment()
          case '*' =>
            token = COMMENT
            openComments = 1
            nextCommentChar()
            skipNestedComments()
          case _ =>
            putChar('/')
            getOperatorRest()
        }
      case '0' =>
        def fetchZero() = {
          nextChar()
          if (ch == 'x' || ch == 'X') {
            nextChar()
            base = 16
            getNumber()
          } else if (dialect.allowBinaryLiterals && (ch == 'b' || ch == 'B')) {
            nextChar()
            base = 2
            getNumber()
          } else {
            // since we didn't store '0', might need to do it later if there are no more digits
            base = 10
            getNumber(hadLeadingZero = true)
          }
        }
        fetchZero()
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        base = 10
        getNumber()
      case '`' => getBackquotedIdent()
      case '"' =>
        def fetchDoubleQuote(): Unit =
          if (token == INTERPOLATIONID) {
            nextRawChar()
            offset = begCharOffset
            if (ch == '"') {
              nextChar()
              if (ch == '"') {
                nextRawChar()
                offset = begCharOffset
                getStringPart(multiLine = true)
                pushSepRegions(STRINGPART) // indicate string part
                pushSepRegions(STRINGLIT) // once more to indicate multi line string part
              } else {
                token = STRINGLIT
                endOffset = offset
                strVal = ""
              }
            } else {
              getStringPart(multiLine = false)
              pushSepRegions(STRINGLIT) // indicate single line string part
            }
          } else {
            nextChar()
            if (ch == '"') {
              nextChar()
              if (ch == '"') {
                nextRawChar()
                getMultilineStringLit()
              } else {
                token = STRINGLIT
                strVal = ""
              }
            } else getStringLit()
          }
        fetchDoubleQuote()
      case '\'' =>
        def isNonLiteralBraceOrBracket = {
          val nextNonWhitespace = peekNonWhitespace()
          (nextNonWhitespace.ch == '{' || nextNonWhitespace.ch == '[') &&
          peekRawChar(nextNonWhitespace.end).ch != '\''
        }
        def fetchSingleQuote() = {
          nextRawChar()
          if (isUnquoteDollar())
            syntaxError("can't unquote into character literals", at = begCharOffset)
          else if (ch == LF && buf(begCharOffset) != '\\')
            syntaxError("can't use unescaped LF in character literals", at = begCharOffset)
          else if (isIdentifierStart(ch)) charLitOr(getIdentRest)
          else if (isOperatorPart(ch) && (ch != '\\' || wasMultiChar)) charLitOr(getOperatorRest)
          else if (dialect.allowSpliceAndQuote && isNonLiteralBraceOrBracket)
            setTokStrVal(MACROQUOTE)
          else {
            getLitChar()
            if (ch == '\'') {
              nextChar()
              setTokStrVal(CHARLIT)
            } else syntaxError("unclosed character literal", at = offset)
          }
        }
        fetchSingleQuote()
      case '.' =>
        nextChar()
        if (isDigit()) { putChar('.'); setFractionOnDot() }
        else if (unquoteDialect != null && ch == '.') {
          base = 0
          while (ch == '.') {
            base += 1
            nextChar()
          }
          token = ELLIPSIS
        } else token = DOT
      case ';' => nextChar(); token = SEMI
      case ',' => nextChar(); token = COMMA
      case '(' => nextChar(); token = LPAREN
      case '{' => nextChar(); token = LBRACE
      case ')' => nextChar(); token = RPAREN
      case '}' => nextChar(); token = RBRACE
      case '[' => nextChar(); token = LBRACKET
      case ']' => nextChar(); token = RBRACKET
      case SU =>
        if (isAtEnd) {
          // NOTE: sometimes EOF's offset is `input.chars.length - 1`, and that might mess things up
          offset = input.chars.length
          token = EOF
        } else {
          syntaxError("illegal character", at = offset)
          nextChar()
        }
      case _ =>
        def fetchOther() =
          if (ch == '\u21D2') { nextChar(); token = ARROW }
          else if (ch == '\u2190') { nextChar(); token = LARROW }
          else if (Character.isUnicodeIdentifierStart(ch)) {
            putCharAndNext()
            getIdentRest()
          } else if (isSpecial(ch)) {
            putCharAndNext()
            getOperatorRest()
          } else {
            syntaxError(
              "illegal character '" + ("" + '\\' + 'u' + "%04x".format(ch.toInt)) + "'",
              at = offset
            )
            nextChar()
          }
        fetchOther()
    }
  }

// Identifiers ---------------------------------------------------------------

  private def getBackquotedIdent(): Unit = {
    nextChar()
    if (getLitChars('`')) {
      nextChar()
      finishNamed(isBackquoted = true)
      if (strVal.isEmpty) syntaxError("empty quoted identifier", at = offset)
    } else if (ch == '$') syntaxError("can't unquote into quoted identifiers", at = begCharOffset)
    else syntaxError("unclosed quoted identifier", at = offset)
  }

  @tailrec
  private final def getIdentRest(): Unit =
    if (ch == '_') {
      putCharAndNext()
      if (isIdentifierPart(ch)) getIdentRest() else getOperatorRest()
    } else if (if (ch == '$') !isUnquoteNextNoDollar() else isUnicodeIdentifierPart(ch)) {
      putCharAndNext()
      getIdentRest()
    } else finishNamed()

  @tailrec
  private def getOperatorRest(): Unit = (ch: @switch) match {
    case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' | '?' | ':' | '=' | '&' |
        '|' | '\\' => putCharAndNext(); getOperatorRest()
    case '/' =>
      val peekNextChar = peekRawChar().ch
      if (peekNextChar == '/' || peekNextChar == '*') finishNamed()
      else {
        putCharAndNext()
        getOperatorRest()
      }
    case _ =>
      if (isSpecial(ch)) { putCharAndNext(); getOperatorRest() }
      else finishNamed()
  }

  // True means that we need to switch into unquote reading mode.
  private def isUnquoteNextNoDollar(): Boolean = unquoteDialect != null && {
    // Skip the first dollar and move on to whatever we've been doing:
    // starting or continuing tokenization of an identifier,
    // or continuing reading a string literal, or whatever.
    !nextCharIf(_ == '$')
  }
  @inline
  private def isUnquoteDollar(): Boolean = ch == '$' && isUnquoteNextNoDollar()

// Literals -----------------------------------------------------------------

  private def getStringLit() =
    if (getLitChars('"')) {
      nextChar()
      finishStringLit()
    } else if (ch == '$') syntaxError("can't unquote into string literals", at = begCharOffset)
    else syntaxError("unclosed string literal", at = offset)

  @tailrec
  private def getMultilineStringLit(): Unit =
    if (ch == '"') { if (!canFinishMultilineStringLit()) getMultilineStringLit() }
    else if (ch == SU) incompleteInputError("unclosed multi-line string literal", at = offset)
    else if (isUnquoteDollar())
      syntaxError("can't unquote into string literals", at = begCharOffset)
    else {
      putCharAndNextRaw()
      getMultilineStringLit()
    }

  private def finishStringLit() = setTokStrVal(STRINGLIT)

  @scala.annotation.tailrec
  private def getStringPart(multiLine: Boolean): Unit = {
    def unclosedLiteralError() = {
      if (!multiLine) syntaxError("unclosed string interpolation", at = offset)
      incompleteInputError("unclosed multi-line string interpolation", at = offset)
    }

    if (wasMultiChar) {
      putCharAndNextRaw()
      getStringPart(multiLine)
    } else (ch: @switch) match {
      case '"' =>
        if (multiLine) {
          if (!canFinishMultilineStringLit(withoutQuotes = true)) getStringPart(true)
        } else {
          endOffset = begCharOffset
          nextChar()
          finishStringLit()
        }
      case '\\' if !multiLine =>
        putCharAndNextRaw()
        if (ch == '"' || ch == '\\') putCharAndNextRaw()
        getStringPart(multiLine)
      case '$' =>
        val dollarOffset = begCharOffset
        if (isUnquoteNextNoDollar())
          syntaxError("can't unquote into string interpolations", at = begCharOffset)
        nextRawChar()
        val done = (ch: @switch) match {
          case '$' => false
          case '"' if dialect.allowInterpolationDolarQuoteEscape => false
          case _ => true
        }
        if (done) {
          setTokStrVal(STRINGPART)
          endOffset = dollarOffset
        } else {
          putCharAndNextRaw()
          getStringPart(multiLine)
        }
      case SU => unclosedLiteralError()
      case CR | LF if !multiLine => unclosedLiteralError()
      case _ =>
        putCharAndNextRaw()
        getStringPart(multiLine)
    }
  }

  private def getStringSplice(): Unit = {
    def identifier() = {
      do putCharAndNextRaw() while (isUnicodeIdentifierPart(ch))
      curr.setIdentifier(getAndResetCBuf(), dialect) { x =>
        if (x.token != IDENTIFIER && x.token != THIS) syntaxError(
          "invalid unquote: `$'ident, `$'BlockExpr, `$'this or `$'_ expected",
          at = x.offset
        )
      }
    }

    (ch: @switch) match {
      case '{' =>
        nextRawChar()
        token = LBRACE
      case '_' if dialect.allowSpliceUnderscores =>
        nextRawChar()
        if (Character.isUnicodeIdentifierStart(ch)) {
          putChar('_')
          identifier()
        } else token = USCORE
      case _ if Character.isUnicodeIdentifierStart(ch) => identifier()
      case _ =>
        var supportedCombos = List("`$$'", "`$'ident", "`$'this", "`$'BlockExpr")
        if (dialect.allowSpliceUnderscores) supportedCombos = supportedCombos :+ "`$'_"
        val s_supportedCombos = supportedCombos.mkString("Not one of: ", ", ", "")
        syntaxError(s_supportedCombos, at = offset)
    }
  }

  private def fetchXmlPart(): Boolean =
    // Clean up map, should be empty at EOF.
    upcomingXmlLiteralParts.remove(offset) match {
      case Some((end, isLastPart)) =>
        finishComposite(XMLLIT, end)
        if (isLastPart) {
          next.endOffset = end
          next.token = XMLLITEND
        }
        true
      case _ => false
    }

  private def canFinishMultilineStringLit(withoutQuotes: Boolean = false): Boolean = {
    var qte1 = begCharOffset
    nextRawChar()
    if (ch == '"') {
      var qte2 = begCharOffset
      nextRawChar()
      if (ch == '"') {
        var qte3 = begCharOffset
        nextChar()
        while (ch == '"') {
          qte1 = qte2
          qte2 = qte3
          qte3 = begCharOffset
          putCharAndNext()
        }
        if (withoutQuotes) endOffset = qte1
        finishStringLit()
        return true
      }
      putChar('"')
    }
    putChar('"')
    false
  }

  /**
   * copy current character into cbuf, interpreting any escape sequences, and advance to next
   * character.
   */
  protected def getLitChar(): Unit =
    if (ch == '\\' && !wasMultiChar) {
      val start = begCharOffset
      nextChar()
      if ('0' <= ch && ch <= '7') {
        val leadch = ch
        var oct: Int = digit2int(ch, 8)
        nextChar()
        if ('0' <= ch && ch <= '7') {
          oct = oct * 8 + digit2int(ch, 8)
          nextChar()
          if (leadch <= '3' && '0' <= ch && ch <= '7') {
            oct = oct * 8 + digit2int(ch, 8)
            nextChar()
          }
        }
        val alt = if (oct == LF) "\\n" else "\\u%04x".format(oct)
        deprecationWarning(s"Octal escape literals are deprecated, use $alt instead.", at = start)
        putChar(oct)
      } else putCharAndNext(ch match {
        case 'b' => '\b'
        case 't' => '\t'
        case 'n' => '\n'
        case 'f' => '\f'
        case 'r' => '\r'
        case '\"' => '\"'
        case '\'' => '\''
        case '\\' => '\\'
        case _ => putChar('\\'); ch
      })
    } else if (isUnquoteDollar()) {} // bail and let the caller handle this
    else putCharAndNext()

  @tailrec
  private def getLitChars(delimiter: Char): Boolean = {
    @inline
    def naturalBreak = (ch == SU || ch == CR || ch == LF) && !wasMultiChar
    ch == delimiter || !isAtEnd && !naturalBreak && {
      val offset = endCharOffset
      getLitChar()
      offset != endCharOffset && getLitChars(delimiter)
    }
  }

  @tailrec
  private def readDigits(base: Int, prevSeparatorOffset: Int = -1): Unit =
    if (digit2int(ch, base) >= 0) {
      putCharAndNext()
      readDigits(base)
    } else if (isNumberSeparator()) {
      val offset = begCharOffset
      nextChar()
      readDigits(base, offset)
    } else if (prevSeparatorOffset >= 0)
      syntaxError("trailing number separator", at = prevSeparatorOffset)

  /**
   * read fractional part and exponent of floating point number if one is present.
   */
  private def setFractionOnDot(): Unit = {
    readDigits(10)
    token = DOUBLELIT
    getFractionExponentAndTypeSuffix()
    setFractionDone()
  }

  private def getFractionExponentAndTypeSuffix(): Boolean = {
    val hasExponent = getFractionExponent()
    getFractionTypeSuffix() || hasExponent
  }

  private def getFractionExponent(): Boolean = (ch == 'e' || ch == 'E') && {
    putCharAndNext()
    if (ch == '+' || ch == '-') putCharAndNext()
    if (isDigit()) {
      readDigits(10)
      token = DOUBLELIT
    } else {
      val errorOffset = begCharOffset
      val isLeadingSeparator = isNumberSeparator(checkOnly = true) && { nextChar(); isDigit() }
      val error =
        if (isLeadingSeparator) "leading number separator"
        else s"Invalid literal floating-point number, exponent not followed by integer"
      syntaxError(error, at = errorOffset)
    }
    true
  }

  @inline
  private def getFractionTypeSuffix(): Boolean = {
    ch match {
      case 'd' | 'D' => token = DOUBLELIT
      case 'f' | 'F' => token = FLOATLIT
      case _ => return false
    }
    nextChar()
    true
  }

  private def setFractionDone(): Unit = {
    checkNoLetter()
    setStrVal()
  }

  private def checkNoLetter(): Unit = if (isIdentifierPart(ch) && ch >= ' ' && !isNumberSeparator())
    syntaxError("Invalid literal number, followed by identifier character", at = begCharOffset)

  /**
   * Read a number into strVal and set base
   */
  private def getNumber(hadLeadingZero: Boolean = false): Unit = {
    readDigits(base)
    val noMoreDigits = cbuf.length() == 0
    if (hadLeadingZero && noMoreDigits) putChar('0')

    def setNumberInt(tokenValue: LegacyToken) = {
      if (hadLeadingZero && !noMoreDigits) // octal deprecated in 2.10, removed in 2.11
        syntaxError("Non-zero integral values may not have a leading zero.", at = offset)
      setTokStrVal(tokenValue)
    }

    def setNumberInteger() =
      if (ch == 'l' || ch == 'L') {
        nextChar()
        setNumberInt(LONGLIT)
      } else {
        checkNoLetter()
        setNumberInt(INTLIT)
      }

    if (base == 10)
      if (getFractionExponentAndTypeSuffix()) setFractionDone()
      else if (ch == '.') {
        val nextChar = peekRawChar()
        if (CharArrayReader.isDigit(nextChar.ch)) {
          putChar(ch) // '.'
          setNextRawChar(nextChar)
          setFractionOnDot()
        } else setNumberInt(INTLIT)
      } else setNumberInteger()
    else setNumberInteger()
  }

  /**
   * Parse character literal if current character is followed by \', or follow with given op and
   * return a symbol literal token
   */
  private def charLitOr(op: () => Unit): Unit = {
    putCharAndNext()
    if (ch == '\'') {
      nextChar()
      setTokStrVal(CHARLIT)
    } else {
      op()
      token = SYMBOLLIT
    }
  }

  private def getXml(): Unit = {
    // 1. Collect positions of scala expressions inside this xml literal.
    import fastparse.Parsed
    val start = offset
    val xmlParser = new XmlParser(dialect)
    val result: Int = fastparse.parse(input.text, xmlParser.XmlExpr(_), startIndex = start) match {
      case x: Parsed.Success[_] => x.index
      case x: Parsed.Failure => syntaxError(
          "malformed xml literal, expected:" + EOL + x.extra.trace().terminalsMsg,
          at = x.index
        )
    }

    // 2. Populate upcomingXmlLiteralParts with xml literal part positions.
    val lastFrom = xmlParser.splicePositions.foldLeft(start) { (lastFrom, pos) =>
      // pos contains the start and end positions of a scala expression.
      // We want the range of the xml literal part which starts at lastFrom
      // and ends at pos.from.
      val to = pos.from - 1
      upcomingXmlLiteralParts.update(lastFrom, (to, false))
      pos.to + 1
    }
    // The final xml literal part is not followed by any embedded scala expr.
    upcomingXmlLiteralParts.update(lastFrom, (result, true))

    // 3. Return only the first xml part.
    fetchXmlPart()
  }

// Unquotes -----------------------------------------------------------------

  private def getUnquote(): Unit = {
    val start = endCharOffset
    val exploratoryInput = Input.Slice(input, start, input.chars.length)
    val exploratoryScanner = new LegacyScanner(exploratoryInput, unquoteDialect)(new Reporter {
      override def input: Input = exploratoryInput
      override protected def error(msg: String, at: Position): Nothing =
        syntaxError(s"invalid unquote: $msg", at = start + at.start)
    })
    exploratoryScanner.initialize()
    val ltd = exploratoryScanner.nextToken()
    val ltdEnd = ltd.token match {
      case LBRACE =>
        @tailrec
        def loop(balance: Int): LegacyTokenData = {
          val ltd = exploratoryScanner.nextToken()
          ltd.token match {
            case LBRACE => loop(balance + 1)
            case RBRACE => if (balance > 0) loop(balance - 1) else ltd
            case _ => loop(balance)
          }
        }
        loop(0)
      case IDENTIFIER | THIS | USCORE =>
        // do nothing, this is the end of the unquote
        ltd
      case _ =>
        syntaxError("invalid unquote: `$'ident, `$'BlockExpr, `$'this or `$'_ expected", at = start)
    }
    finishComposite(UNQUOTE, start + ltdEnd.endOffset)
  }

// Errors -----------------------------------------------------------------

  private def setInvalidToken(tok: LegacyTokenData, offset: Int)(message: String): Unit =
    if (tok.token != INVALID) {
      tok.setInvalidToken(message)
      tok.offset = offset
      tok.endOffset = offset
    }

  @inline
  private def setInvalidToken(tok: LegacyTokenData)(message: => String): Unit =
    setInvalidToken(tok, begCharOffset)(message)

  override def toString = token.toString

}
