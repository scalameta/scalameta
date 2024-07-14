package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.internal.tokens.Chars._

import scala.annotation.switch
import scala.annotation.tailrec
import scala.collection.mutable

class LegacyScanner(input: Input, dialect: Dialect)(implicit reporter: Reporter) {

  import LegacyToken._

  private val unquoteDialect = dialect.unquoteParentDialect

  private val curr: LegacyTokenData = new LegacyTokenData
  private val next: LegacyTokenData = new LegacyTokenData

  private val reader: CharArrayReader = new CharArrayReader(input, dialect, reporter)

  import curr._
  import reader._
  import reporter._

  private var openComments = 0
  private def putCommentChar(): Unit = nextCommentChar()

  @tailrec
  private def skipLineComment(): Unit = ch match {
    case SU | CR | LF =>
    case '$' if isUnquoteNextNoDollar() =>
      syntaxError("can't unquote into single-line comments", at = begCharOffset)
    case _ => putCommentChar(); skipLineComment()
  }
  private def maybeOpen(): Unit = {
    putCommentChar()
    if (ch == '*') {
      putCommentChar()
      openComments += 1
    }
  }
  private def maybeClose(): Boolean = {
    putCommentChar()
    (ch == '/') && {
      openComments -= 1
      val close = openComments == 0
      if (close) nextChar() else putCommentChar()
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
    case _ => putCommentChar(); skipNestedComments()
  }

  private def skipToCommentEnd(): Unit = {
    val isLineComment = ch == '/'
    putCommentChar()
    if (isLineComment) skipLineComment()
    else {
      openComments = 1
      if (ch == '*') {
        putCommentChar()
        // Check for the amazing corner case of /**/
        if (ch == '/') nextChar() else skipNestedComments()
      } else skipNestedComments()
    }
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
    reader.nextCharFrom(endExclusive)
  }

  /** Clear buffer and set string */
  private def setStrVal(): Unit = strVal = getAndResetCBuf()

  private def getAndResetCBuf(): String =
    try cbuf.toString
    finally cbuf.setLength(0)

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

  @inline
  private def popSepRegions() = sepRegions = sepRegions.tail

  @inline
  private def isHead(legacyTokens: List[LegacyToken], head: LegacyToken) = legacyTokens.nonEmpty &&
    legacyTokens.head == head

  @inline
  private def isSepRegionsHead(head: LegacyToken) = isHead(sepRegions, head)

  /**
   * A map of upcoming xml literal parts that are left to be returned in nextToken().
   *
   * The keys are offset start positions of an xml literal and the values are the respective offset
   * end positions and a boolean indicating if the part is the last part.
   */
  private val upcomingXmlLiteralParts = mutable.Map.empty[Offset, (Offset, Boolean)]

// Get next token ------------------------------------------------------------

  /**
   * Are we directly in a string interpolation expression?
   */
  private def inStringInterpolation = isSepRegionsHead(STRINGLIT)

  /**
   * STRINGPART follows STRINGLIT in multiline interpolation
   */
  @inline
  private def startsStringPart(sr: List[LegacyToken]) = isHead(sr, STRINGPART)

  private def popStringInterpolation(): Unit = if (inStringInterpolation) {
    popSepRegions()
    if (startsStringPart(sepRegions)) popSepRegions()
  }

  def initialize(bof: Boolean = false): Unit = if (endCharOffset == 0) {
    nextChar()
    if (bof && '#' == ch && !wasMultiChar && buf(endCharOffset) == '!') {
      next.offset = begCharOffset
      do { putChar(ch); nextChar() } while (ch != CR && ch != LF && ch != FF && ch != SU)
      next.strVal = getAndResetCBuf()
      next.endOffset = begCharOffset
      next.token = SHEBANG
    }
  }

  @inline
  def nextTokenOrEof(): LegacyTokenData = if (token == EOF) null else nextToken()

  /**
   * Produce next token, filling curr TokenData fields of Scanner.
   */
  def nextToken(): LegacyTokenData = {
    val lastToken = token
    // Adapt sepRegions according to last token
    (lastToken: @switch) match {
      case EOF => throw new UnexpectedInputEndException(curr)
      case LPAREN => pushSepRegions(RPAREN)
      case LBRACKET => pushSepRegions(RBRACKET)
      case LBRACE => pushSepRegions(RBRACE)
      case CASE => pushSepRegions(ARROW)
      case RBRACE =>
        while (sepRegions.nonEmpty && sepRegions.head != RBRACE) popSepRegions()
        if (sepRegions.nonEmpty) popSepRegions()
      case RBRACKET | RPAREN => if (isSepRegionsHead(lastToken)) popSepRegions()
      case ARROW => if (isSepRegionsHead(lastToken)) popSepRegions()
      case STRINGLIT => popStringInterpolation()
      case _ =>
    }

    // Read a token or copy it from `next` tokenData
    if (next.token == EMPTY) {
      offset = begCharOffset
      fetchToken()
      (if (next.token == EMPTY) curr else next).endOffset =
        if (endCharOffset >= buf.length && ch == SU) buf.length else begCharOffset
    } else {
      curr.copyFrom(next)
      next.token = EMPTY
    }

    curr
  }

  /**
   * read next token, filling TokenData fields of Scanner.
   */
  private final def fetchToken(): Unit = {
    if (inStringInterpolation) return getStringPart(multiLine = startsStringPart(sepRegions.tail))
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
        putChar(ch)
        nextChar()
        getIdentRestCheckInterpolation()
      case '$' =>
        if (isUnquoteNextNoDollar()) getUnquote()
        else {
          putChar(ch)
          nextChar()
          if (dialect.allowSpliceAndQuote && lookaheadReader.nextNonWhitespace == '{') {
            token = MACROSPLICE
            setStrVal()
          } else getIdentRestCheckInterpolation()
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
        putChar(ch)
        nextChar()
        getOperatorRest()
      case '/' =>
        nextChar()
        ch match {
          case '/' | '*' =>
            skipToCommentEnd()
            token = COMMENT
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
      case '\"' =>
        def fetchDoubleQuote(): Unit =
          if (token == INTERPOLATIONID) {
            nextRawChar()
            offset += 1
            if (ch == '\"') {
              nextChar()
              if (ch == '\"') {
                offset += 2
                nextRawChar()
                getStringPart(multiLine = true)
                pushSepRegions(STRINGPART) // indicate string part
                pushSepRegions(STRINGLIT) // once more to indicate multi line string part
              } else {
                token = STRINGLIT
                strVal = ""
              }
            } else {
              getStringPart(multiLine = false)
              pushSepRegions(STRINGLIT) // indicate single line string part
            }
          } else {
            nextChar()
            if (ch == '\"') {
              nextChar()
              if (ch == '\"') {
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
        def fetchSingleQuote() = {
          nextRawChar()
          if (isUnquoteDollar())
            syntaxError("can't unquote into character literals", at = begCharOffset)
          else if (ch == LF && buf(begCharOffset) != '\\')
            syntaxError("can't use unescaped LF in character literals", at = begCharOffset)
          else if (isIdentifierStart(ch)) charLitOr(getIdentRest)
          else if (isOperatorPart(ch) && (ch != '\\' || wasMultiChar)) charLitOr(getOperatorRest)
          else {
            def isNonLiteralBraceOrBracket = {
              val lookahead = lookaheadReader
              val nextNonWhitespace = lookahead.nextNonWhitespace
              (nextNonWhitespace == '{' || nextNonWhitespace == '[') && {
                lookahead.nextRawChar()
                lookahead.ch != '\''
              }
            }
            if (dialect.allowSpliceAndQuote && isNonLiteralBraceOrBracket) {
              token = MACROQUOTE
              setStrVal()
            } else {
              getLitChar()
              if (ch == '\'') {
                nextChar()
                token = CHARLIT
                setStrVal()
              } else syntaxError("unclosed character literal", at = offset)
            }
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
            putChar(ch)
            nextChar()
            getIdentRest()
          } else if (isSpecial(ch)) {
            putChar(ch)
            nextChar()
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
      putChar(ch)
      nextChar()
      if (isIdentifierPart(ch)) getIdentRest() else getOperatorRest()
    } else if (if (ch == '$') !isUnquoteNextNoDollar() else isUnicodeIdentifierPart(ch)) {
      putChar(ch)
      nextChar()
      getIdentRest()
    } else finishNamed()

  @tailrec
  private def getOperatorRest(): Unit = (ch: @switch) match {
    case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' | '?' | ':' | '=' | '&' |
        '|' | '\\' => putChar(ch); nextChar(); getOperatorRest()
    case '/' =>
      val peekNextChar = lookaheadReader.getc()
      if (peekNextChar == '/' || peekNextChar == '*') finishNamed()
      else {
        putChar('/')
        nextChar()
        getOperatorRest()
      }
    case _ =>
      if (isSpecial(ch)) { putChar(ch); nextChar(); getOperatorRest() }
      else finishNamed()
  }

  // True means that we need to switch into unquote reading mode.
  private def isUnquoteNextNoDollar(): Boolean = unquoteDialect != null && {
    val isDollar = lookaheadReader.getc() == '$'
    if (isDollar)
      // Skip the first dollar and move on to whatever we've been doing:
      // starting or continuing tokenization of an identifier,
      // or continuing reading a string literal, or whatever.
      nextChar()
    !isDollar
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
    if (ch == '\"') { if (!canFinishMultilineStringLit()) getMultilineStringLit() }
    else if (ch == SU) incompleteInputError("unclosed multi-line string literal", at = offset)
    else if (isUnquoteDollar())
      syntaxError("can't unquote into string literals", at = begCharOffset)
    else {
      putChar(ch)
      nextRawChar()
      getMultilineStringLit()
    }

  private def finishStringLit() = {
    setStrVal()
    token = STRINGLIT
  }
  private def finishStringPart() = {
    setStrVal()
    token = STRINGPART
    endOffset = endCharOffset - 2
    next.offset = begCharOffset
  }

  @scala.annotation.tailrec
  private def getStringPart(multiLine: Boolean): Unit = {
    def identifier() = {
      do {
        putChar(ch)
        nextRawChar()
      } while (isUnicodeIdentifierPart(ch))
      next.setIdentifier(getAndResetCBuf(), dialect) { x =>
        if (x.token != IDENTIFIER && x.token != THIS) syntaxError(
          "invalid unquote: `$'ident, `$'BlockExpr, `$'this or `$'_ expected",
          at = x.offset
        )
      }
    }
    if (ch == '"')
      if (multiLine) { if (!canFinishMultilineStringLit()) getStringPart(true) }
      else {
        nextChar()
        finishStringLit()
      }
    else if (ch == '\\' && !multiLine) {
      putChar(ch)
      nextRawChar()
      if (ch == '"' || ch == '\\') {
        putChar(ch)
        nextRawChar()
      }
      getStringPart(multiLine)
    } else if (ch == '$' && !wasMultiChar)
      if (isUnquoteNextNoDollar())
        syntaxError("can't unquote into string interpolations", at = begCharOffset)
      else {
        nextRawChar()
        if (ch == '$' || (ch == '"' && dialect.allowInterpolationDolarQuoteEscape)) {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine)
        } else if (ch == '{') {
          finishStringPart()
          nextRawChar()
          next.token = LBRACE
        } else if (ch == '_' && dialect.allowSpliceUnderscores) {
          finishStringPart()
          nextRawChar()
          if (Character.isUnicodeIdentifierStart(ch)) {
            putChar('_')
            identifier()
          } else next.token = USCORE
        } else if (Character.isUnicodeIdentifierStart(ch)) {
          finishStringPart()
          identifier()
        } else {
          var supportedCombos = List("`$$'", "`$'ident", "`$'this", "`$'BlockExpr")
          if (dialect.allowSpliceUnderscores) supportedCombos = supportedCombos :+ "`$'_"
          val s_supportedCombos = supportedCombos.mkString("Not one of: ", ", ", "")
          syntaxError(s_supportedCombos, at = offset)
        }
      }
    else {
      val isUnclosedLiteral = !wasMultiChar && (ch == SU || (!multiLine && (ch == CR || ch == LF)))
      if (isUnclosedLiteral)
        if (multiLine) incompleteInputError("unclosed multi-line string interpolation", at = offset)
        else syntaxError("unclosed string interpolation", at = offset)
      else {
        putChar(ch)
        nextRawChar()
        getStringPart(multiLine)
      }
    }
  }

  private def fetchXmlPart(): Boolean =
    // Clean up map, should be empty at EOF.
    upcomingXmlLiteralParts.remove(offset) match {
      case Some((end, isLastPart)) =>
        finishComposite(XMLLIT, end)
        if (isLastPart) {
          curr.endOffset = end
          next.token = XMLLITEND
        }
        true
      case _ => false
    }

  private def canFinishMultilineStringLit(): Boolean = {
    nextRawChar()
    if (ch == '"') {
      nextRawChar()
      if (ch == '"') {
        while ({ nextChar(); ch == '"' }) putChar('"')
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
      } else {
        ch match {
          case 'b' => putChar('\b')
          case 't' => putChar('\t')
          case 'n' => putChar('\n')
          case 'f' => putChar('\f')
          case 'r' => putChar('\r')
          case '\"' => putChar('\"')
          case '\'' => putChar('\'')
          case '\\' => putChar('\\')
          case _ => invalidEscape()
        }
        nextChar()
      }
    } else if (isUnquoteDollar()) {
      // bail and let the caller handle this
    } else {
      putChar(ch)
      nextChar()
    }

  private def invalidEscape(): Unit = {
    syntaxError("invalid escape character", at = begCharOffset)
    putChar(ch)
  }

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
  private def readDigits(base: Int, wasSeparator: Boolean = false): Unit =
    if (digit2int(ch, base) >= 0) {
      putChar(ch)
      nextChar()
      readDigits(base)
    } else if (isNumberSeparator()) {
      nextChar()
      readDigits(base, true)
    } else if (wasSeparator) {
      val pos = if (ch == SU) begCharOffset else begCharOffset - 1
      syntaxError("trailing number separator", at = pos)
    }

  /**
   * read fractional part and exponent of floating point number if one is present.
   */
  private def setFractionOnDot(): Unit = {
    readDigits(10)
    token = DOUBLELIT
    getFractionExponent()
    getFractionWithSuffix()
    setFractionDone()
  }

  private def getFractionExponent(): Boolean = (ch == 'e' || ch == 'E') && {
    val lookahead = lookaheadReader
    lookahead.nextChar()
    val sign = lookahead.ch == '+' || lookahead.ch == '-'
    if (sign) lookahead.nextChar()
    if (!lookahead.isDigit()) {
      if (lookahead.isNumberSeparator(checkOnly = true)) {
        val separatorOffset = lookahead.begCharOffset
        lookahead.nextChar()
        if (lookahead.isDigit()) syntaxError("leading number separator", at = separatorOffset)
      }
      syntaxError(
        s"Invalid literal floating-point number, exponent not followed by integer",
        at = begCharOffset
      )
    }
    putChar(ch)
    nextChar()
    if (sign) {
      putChar(ch)
      nextChar()
    }
    readDigits(10)
    token = DOUBLELIT
    true
  }

  @inline
  private def getFractionWithSuffix(): Boolean = {
    val ok = ch match {
      case 'd' | 'D' => token = DOUBLELIT; true
      case 'f' | 'F' => token = FLOATLIT; true
      case _ => false
    }
    if (ok) nextChar()
    ok
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
      setStrVal()
      token = tokenValue
    }

    def setNumberInteger() =
      if (ch == 'l' || ch == 'L') {
        nextChar()
        setNumberInt(LONGLIT)
      } else {
        checkNoLetter()
        setNumberInt(INTLIT)
      }

    if (base == 10) {
      val isFractionExp = getFractionExponent()
      if (getFractionWithSuffix() || isFractionExp) setFractionDone()
      else if (ch == '.') {
        val lookahead = lookaheadReader
        lookahead.nextChar()
        if (lookahead.isDigit()) {
          putChar(ch) // '.'
          nextChar()
          setFractionOnDot()
        } else setNumberInt(INTLIT)
      } else setNumberInteger()
    } else setNumberInteger()
  }

  /**
   * Parse character literal if current character is followed by \', or follow with given op and
   * return a symbol literal token
   */
  private def charLitOr(op: () => Unit): Unit = {
    putChar(ch)
    nextChar()
    if (ch == '\'') {
      nextChar()
      token = CHARLIT
      setStrVal()
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
          s"malformed xml literal, expected:\n${x.extra.trace().terminalsMsg}",
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
    require(ch == '$')
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

  override def toString = token.toString

}
