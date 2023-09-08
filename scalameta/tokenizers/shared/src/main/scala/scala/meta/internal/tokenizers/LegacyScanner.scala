package scala.meta
package internal
package tokenizers

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import Chars._
import LegacyToken._
import scala.meta.inputs._
import scala.meta.tokenizers.TokenizeException

class LegacyScanner(input: Input, dialect: Dialect) {
  val reporter: Reporter = Reporter(input)
  val curr: LegacyTokenData = new LegacyTokenData {}
  val next: LegacyTokenData = new LegacyTokenData {}
  val prev: LegacyTokenData = new LegacyTokenData {}
  val reader: CharArrayReader = new CharArrayReader(input, dialect, reporter)

  import curr._, reader._, reporter._
  curr.input = this.input
  next.input = this.input
  prev.input = this.input

  private def isDigit(c: Char) = java.lang.Character isDigit c
  private var openComments = 0
  protected def putCommentChar(): Unit = nextCommentChar()

  @tailrec private def skipLineComment(): Unit = ch match {
    case SU | CR | LF =>
    case '$' if isUnquoteNextNoDollar() =>
      syntaxError("can't unquote into single-line comments", at = charOffset - 1)
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
  @tailrec final def skipNestedComments(): Unit = ch match {
    case '/' => maybeOpen(); skipNestedComments()
    case '*' => if (!maybeClose()) skipNestedComments()
    case SU => incompleteInputError("unclosed comment", at = offset)
    case '$' if isUnquoteNextNoDollar() =>
      syntaxError("can't unquote into multi-line comments", at = charOffset - 1)
    case _ => putCommentChar(); skipNestedComments()
  }
  def skipDocComment(): Unit = skipNestedComments()
  def skipBlockComment(): Unit = skipNestedComments()

  private def skipToCommentEnd(isLineComment: Boolean): Unit = {
    putCommentChar()
    if (isLineComment) skipLineComment()
    else {
      openComments = 1
      if (ch == '*') {
        putCommentChar()
        // Check for the amazing corner case of /**/
        if (ch == '/')
          nextChar()
        else
          skipDocComment()
      } else skipBlockComment()
    }
  }

  /**
   * Precondition: ch == '/' Returns true if a comment was skipped.
   */
  def skipComment(): Boolean = ch match {
    case '/' | '*' => skipToCommentEnd(isLineComment = ch == '/'); true
    case _ => false
  }
  def flushDoc(): Unit = ()

  /**
   * To prevent doc comments attached to expressions from leaking out of scope onto the next
   * documentable entity, they are discarded upon passing a right brace, bracket, or parenthesis.
   */
  def discardDocBuffer(): Unit = ()

  def isAtEnd = charOffset >= buf.length

  def resume(lastCode: LegacyToken) = {
    token = lastCode
    if (next.token != EMPTY)
      syntaxError("unexpected end of input: possible missing '}' in XML block", at = offset)

    nextToken()
  }

  /**
   * A character buffer for literals
   */
  val cbuf = new java.lang.StringBuilder

  /**
   * append Unicode character to "cbuf" buffer
   */
  protected def putChar(c: Char): Unit = {
//      assert(cbuf.size < 10000, cbuf)
    cbuf.append(c)
  }

  /**
   * Determines whether this scanner should emit identifier deprecation warnings, e.g. when seeing
   * `macro' or `then', which are planned to become keywords in future versions of Scala.
   */
  protected def emitIdentifierDeprecationWarnings = true

  /** Clear buffer and set name and token */
  private def finishNamed(isBackquoted: Boolean = false): Unit = {
    curr.setIdentifier(getAndResetCBuf(), dialect, check = !isBackquoted) { x =>
      if (x.token == IDENTIFIER && emitIdentifierDeprecationWarnings)
        deprecationWarning(
          s"${x.name} is now a reserved word; usage as an identifier is deprecated",
          at = x.token
        )
    }
  }

  /* much like endOffset, end is inclusive */
  private def finishComposite(token: LegacyToken, endExclusive: Offset): Unit = {
    val start = offset
    curr.token = token
    curr.strVal = new String(input.chars, start, endExclusive - start)
    curr.endOffset = endExclusive - 1
    reader.charOffset = endExclusive
    reader.nextChar()
  }

  /** Clear buffer and set string */
  private def setStrVal(): Unit = {
    strVal = getAndResetCBuf()
  }

  private def getAndResetCBuf(): String = {
    try cbuf.toString
    finally cbuf.setLength(0)
  }

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
  var sepRegions: List[LegacyToken] = List()

  @inline private def pushSepRegions(sr: LegacyToken) = sepRegions = sr :: sepRegions

  @inline private def popSepRegions() = sepRegions = sepRegions.tail

  @inline private def isHead(legacyTokens: List[LegacyToken], head: LegacyToken) =
    legacyTokens.nonEmpty && legacyTokens.head == head

  @inline private def isSepRegionsHead(head: LegacyToken) = isHead(sepRegions, head)

  /**
   * A map of upcoming xml literal parts that are left to be returned in nextToken().
   *
   * The keys are offset start positions of an xml literal and the values are the respective offset
   * end positions and a boolean indicating if the part is the last part.
   */
  val upcomingXmlLiteralParts = mutable.Map.empty[Offset, (Offset, Boolean)]

// Get next token ------------------------------------------------------------

  /**
   * Are we directly in a string interpolation expression?
   */
  private def inStringInterpolation =
    isSepRegionsHead(STRINGLIT)

  /**
   * STRINGPART follows STRINGLIT in multiline interpolation
   */
  @inline private def startsStringPart(sr: List[LegacyToken]) = isHead(sr, STRINGPART)

  private def popStringInterpolation(): Unit =
    if (inStringInterpolation) {
      popSepRegions()
      if (startsStringPart(sepRegions))
        popSepRegions()
    }

  /**
   * read next token and return last offset
   */
  def skipToken(): Offset = {
    val off = offset
    nextToken()
    off
  }

  /**
   * Produce next token, filling curr TokenData fields of Scanner.
   */
  def nextToken(): Unit = {
    val lastToken = token
    // Adapt sepRegions according to last token
    (lastToken: @switch) match {
      case LPAREN =>
        pushSepRegions(RPAREN)
      case LBRACKET =>
        pushSepRegions(RBRACKET)
      case LBRACE =>
        pushSepRegions(RBRACE)
      case CASE =>
        pushSepRegions(ARROW)
      case RBRACE =>
        while (sepRegions.nonEmpty && sepRegions.head != RBRACE)
          popSepRegions()
        if (sepRegions.nonEmpty)
          popSepRegions()

        discardDocBuffer()
      case RBRACKET | RPAREN =>
        if (isSepRegionsHead(lastToken)) popSepRegions()

        discardDocBuffer()
      case ARROW =>
        if (isSepRegionsHead(lastToken)) popSepRegions()
      case STRINGLIT =>
        popStringInterpolation()
      case _ =>
    }

    // Read a token or copy it from `next` tokenData
    if (next.token == EMPTY) {
      lastOffset = charOffset - 1
      if (lastOffset > 0 && buf(lastOffset) == '\n' && buf(lastOffset - 1) == '\r') {
        lastOffset -= 1
      }

      fetchToken()
      if (token == ERROR) popStringInterpolation()
    } else {
      curr copyFrom next
      next.token = EMPTY
    }

    // NOTE: endOffset is used to determine range positions for certain tokens.
    // Most tokens (e.g. `(' or `;') have constant widths, so their range positions can be calculated trivially from their offsets,
    // however some tokens have variable widths,
    // and for them we need to remember where their parsing ended in order to calculate their positions.
    // That's what endOffset does (indirectly): each token's position should be [curr.offset, curr.endOffset]
    //
    // Now how do we calculate endOffset?
    // 1) What we have at hand is `charOffset`, which is the position right after the position of the character that's just been read.
    // 2) This means that `charOffset - 1` is the position of the character that's just been read.
    // 3) Since reading that character terminated fetchToken, this means that that character is the first character of the next token.
    // 4) This means that `charOffset - 2` is where the last character of the our current token lies.
    //
    // The only corner case here is EOF. In that case the virtual position of the character that's just been read (or, more precisely,
    // that's been attempted to be read) seems to be `buf.length`, but some other logic in the scanner suggests that sometimes it can even
    // be `buf.length + 1` or more. Therefore, we don't bother ourselves with doing decrements and just assign endOffset to be `buf.length - 1`.
    //
    // upd. Speaking of corner cases, positions of tokens emitted by string interpolation tokenizers are simply insane,
    // and need to be reverse engineered having some context (previous tokens, number of quotes in the interpolation) in mind.
    // Therefore I don't even attempt to handle them here, and instead apply fixups elsewhere when converting legacy TOKENS into new LegacyToken instances.
    if (curr.token != STRINGPART) { // endOffset of STRINGPART tokens is set elsewhere
      curr.endOffset = charOffset - 2
      if (charOffset >= buf.length && ch == SU) curr.endOffset = buf.length - 1
    }
  }

  /**
   * read next token, filling TokenData fields of Scanner.
   */
  protected final def fetchToken(): Unit = {
    offset = charOffset - 1

    if (inStringInterpolation) return getStringPart(multiLine = startsStringPart(sepRegions.tail))
    else if (fetchXmlPart()) return

    @inline def getIdentRestCheckInterpolation() = {
      getIdentRest()
      if (ch == '"' && token == IDENTIFIER)
        token = INTERPOLATIONID
    }

    (ch: @switch) match {
      case ' ' | '\t' | CR | LF | FF =>
        token = WHITESPACE
        strVal = ch.toString
        nextChar()
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
        if (isUnquoteNextNoDollar()) {
          getUnquote()
        } else {
          putChar(ch)
          nextChar()
          if (dialect.allowSpliceAndQuote && lookaheadReader.nextNonWhitespace == '{') {
            token = MACROSPLICE
            setStrVal()
          } else {
            getIdentRestCheckInterpolation()
          }
        }
      case '<' => // is XMLSTART?
        def fetchLT() = {
          val last = if (charOffset >= 2) buf(charOffset - 2) else ' '
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
        if (skipComment()) {
          token = COMMENT
        } else {
          putChar('/')
          getOperatorRest()
        }
      case '0' =>
        def fetchZero() = {
          putChar(ch)
          nextChar()
          if (ch == 'x' || ch == 'X') {
            putChar(ch)
            nextChar()
            base = 16
          } else {
            base = 8
          }
          getNumber()
        }
        fetchZero()
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        base = 10
        getNumber()
      case '`' =>
        getBackquotedIdent()
      case '\"' =>
        def fetchDoubleQuote(): Unit = {
          if (token == INTERPOLATIONID) {
            nextRawChar()
            if (ch == '\"') {
              if (lookaheadReader.getc() == '\"') {
                nextRawChar() // now eat it
                offset += 3
                nextRawChar()
                getStringPart(multiLine = true)
                pushSepRegions(STRINGPART) // indicate string part
                pushSepRegions(STRINGLIT) // once more to indicate multi line string part
              } else {
                nextChar()
                token = STRINGLIT
                strVal = ""
              }
            } else {
              offset += 1
              getStringPart(multiLine = false)
              pushSepRegions(STRINGLIT) // indicate single line string part
            }
          } else {
            nextChar()
            if (ch == '\"') {
              nextChar()
              if (ch == '\"') {
                nextRawChar()
                getRawStringLit()
              } else {
                token = STRINGLIT
                strVal = ""
              }
            } else {
              getStringLit()
            }
          }
        }
        fetchDoubleQuote()
      case '\'' =>
        def fetchSingleQuote() = {
          nextRawChar()
          if (isUnquoteDollar())
            syntaxError("can't unquote into character literals", at = charOffset - 1)
          else if (ch == LF && !isUnicodeEscape)
            syntaxError("can't use unescaped LF in character literals", at = charOffset - 1)
          else if (isIdentifierStart(ch))
            charLitOr(getIdentRest)
          else if (isOperatorPart(ch) && (ch != '\\' || isUnicodeEscape))
            charLitOr(getOperatorRest)
          else {
            def isNotBraceOrBracketLiteral = {
              val lookahead = lookaheadReader
              val nextNonWhitespace = lookahead.nextNonWhitespace
              lookahead.nextRawChar()
              (nextNonWhitespace == '{' || nextNonWhitespace == '[') && lookahead.ch != '\''
            }
            if (dialect.allowSpliceAndQuote && isNotBraceOrBracketLiteral) {
              token = MACROQUOTE
              setStrVal()
            } else {
              getLitChar()
              if (ch == '\'') {
                nextChar()
                token = CHARLIT
                setStrVal()
              } else {
                syntaxError("unclosed character literal", at = offset)
              }
            }
          }
        }
        fetchSingleQuote()
      case '.' =>
        nextChar()
        if ('0' <= ch && ch <= '9') {
          putChar('.'); getFraction()
        } else if (dialect.allowUnquotes && ch == '.') {
          base = 0
          while (ch == '.') {
            base += 1
            nextChar()
          }
          token = ELLIPSIS
        } else {
          token = DOT
        }
      case ';' =>
        nextChar(); token = SEMI
      case ',' =>
        nextChar(); token = COMMA
      case '(' =>
        nextChar(); token = LPAREN
      case '{' =>
        nextChar(); token = LBRACE
      case ')' =>
        nextChar(); token = RPAREN
      case '}' =>
        nextChar(); token = RBRACE
      case '[' =>
        nextChar(); token = LBRACKET
      case ']' =>
        nextChar(); token = RBRACKET
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
        def fetchOther() = {
          if (ch == '\u21D2') {
            nextChar(); token = ARROW
          } else if (ch == '\u2190') {
            nextChar(); token = LARROW
          } else if (Character.isUnicodeIdentifierStart(ch)) {
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
      if (name.isEmpty) syntaxError("empty quoted identifier", at = offset)
    } else if (ch == '$') {
      syntaxError("can't unquote into quoted identifiers", at = charOffset - 1)
    } else {
      syntaxError("unclosed quoted identifier", at = offset)
    }
  }

  @tailrec
  private final def getIdentRest(): Unit = {
    @inline def isNonUnquoteIdentifierPart(c: Char) = {
      if (c == '$') !isUnquoteNextNoDollar()
      else isUnicodeIdentifierPart(c)
    }
    if (ch == '_') {
      putChar(ch)
      nextChar()
      if (isIdentifierPart(ch))
        getIdentRest()
      else
        getOperatorRest()
    } else if (isNonUnquoteIdentifierPart(ch)) {
      putChar(ch)
      nextChar()
      getIdentRest()
    } else finishNamed()
  }

  @tailrec
  private def getOperatorRest(): Unit = (ch: @switch) match {
    case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' | '?' | ':' | '=' | '&' |
        '|' | '\\' =>
      putChar(ch); nextChar(); getOperatorRest()
    case '/' =>
      val peekNextChar = lookaheadReader.getc()
      if (peekNextChar == '/' || peekNextChar == '*') {
        finishNamed()
      } else {
        putChar('/')
        nextChar()
        getOperatorRest()
      }
    case _ =>
      if (isSpecial(ch)) { putChar(ch); nextChar(); getOperatorRest() }
      else finishNamed()
  }

  // True means that we need to switch into unquote reading mode.
  private def isUnquoteNextNoDollar(): Boolean = {
    dialect.allowUnquotes && {
      val isDollar = lookaheadReader.getc() == '$'
      if (isDollar) {
        // Skip the first dollar and move on to whatever we've been doing:
        // starting or continuing tokenization of an identifier,
        // or continuing reading a string literal, or whatever.
        nextChar()
      }
      !isDollar
    }
  }
  @inline private def isUnquoteDollar(): Boolean = ch == '$' && isUnquoteNextNoDollar()

// Literals -----------------------------------------------------------------

  private def getStringLit() = {
    if (getLitChars('"')) {
      setStrVal()
      nextChar()
      token = STRINGLIT
    } else if (ch == '$') {
      syntaxError("can't unquote into string literals", at = charOffset - 1)
    } else {
      syntaxError("unclosed string literal", at = offset)
    }
  }

  @tailrec
  private def getRawStringLit(): Unit = {
    if (ch == '\"') {
      nextRawChar()
      if (isTripleQuote()) {
        setStrVal()
        token = STRINGLIT
      } else
        getRawStringLit()
    } else if (ch == SU) {
      incompleteInputError("unclosed multi-line string literal", at = offset)
    } else if (isUnquoteDollar()) {
      syntaxError("can't unquote into string literals", at = charOffset - 1)
    } else {
      putChar(ch)
      nextRawChar()
      getRawStringLit()
    }
  }

  @scala.annotation.tailrec
  private def getStringPart(multiLine: Boolean): Unit = {
    def finishStringPart() = {
      setStrVal()
      token = STRINGPART
      next.lastOffset = charOffset - 1
      next.offset = charOffset - 1
    }
    def identifier() = {
      do {
        putChar(ch)
        nextRawChar()
      } while (isUnicodeIdentifierPart(ch))
      next.setIdentifier(getAndResetCBuf(), dialect) { x =>
        if (x.token != IDENTIFIER && x.token != THIS)
          syntaxError(
            "invalid unquote: `$'ident, `$'BlockExpr, `$'this or `$'_ expected",
            at = x.offset
          )
      }
    }
    if (ch == '"') {
      if (multiLine) {
        nextRawChar()
        if (isTripleQuote()) {
          setStrVal()
          token = STRINGLIT
        } else
          getStringPart(multiLine)
      } else {
        nextChar()
        setStrVal()
        token = STRINGLIT
      }
    } else if (ch == '\\' && !multiLine) {
      putChar(ch)
      nextRawChar()
      if (ch == '"' || ch == '\\') {
        putChar(ch)
        nextRawChar()
      }
      getStringPart(multiLine)
    } else if (ch == '$' && !isUnicodeEscape) {
      if (isUnquoteNextNoDollar()) {
        syntaxError("can't unquote into string interpolations", at = charOffset - 1)
      } else {
        nextRawChar()
        if (ch == '$' || (ch == '"' && dialect.allowInterpolationDolarQuoteEscape)) {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine)
        } else if (ch == '{') {
          finishStringPart()
          endOffset = charOffset - 3
          nextRawChar()
          next.token = LBRACE
        } else if (ch == '_' && dialect.allowSpliceUnderscores) {
          finishStringPart()
          endOffset = charOffset - 3
          nextRawChar()
          if (Character.isUnicodeIdentifierStart(ch)) {
            putChar('_')
            identifier()
          } else next.token = USCORE
        } else if (Character.isUnicodeIdentifierStart(ch)) {
          finishStringPart()
          endOffset = charOffset - 3
          identifier()
        } else {
          var supportedCombos = List("`$$'", "`$'ident", "`$'this", "`$'BlockExpr")
          if (dialect.allowSpliceUnderscores) supportedCombos = supportedCombos :+ "`$'_"
          val s_supportedCombos = supportedCombos.init.mkString(", ") + supportedCombos.last
          syntaxError(s_supportedCombos, at = offset)
        }
      }
    } else {
      val isUnclosedLiteral =
        !isUnicodeEscape && (ch == SU || (!multiLine && (ch == CR || ch == LF)))
      if (isUnclosedLiteral) {
        if (multiLine)
          incompleteInputError("unclosed multi-line string interpolation", at = offset)
        else
          syntaxError("unclosed string interpolation", at = offset)
      } else {
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
        if (isLastPart) next.token = XMLLITEND
        true
      case _ => false
    }

  private def isTripleQuote(): Boolean =
    if (ch == '"') {
      nextRawChar()
      if (ch == '"') {
        nextChar()
        while (ch == '"') {
          putChar('"')
          nextChar()
        }
        true
      } else {
        putChar('"')
        putChar('"')
        false
      }
    } else {
      putChar('"')
      false
    }

  /**
   * copy current character into cbuf, interpreting any escape sequences, and advance to next
   * character.
   */
  protected def getLitChar(): Unit =
    if (ch == '\\' && !isUnicodeEscape) {
      nextChar()
      if ('0' <= ch && ch <= '7') {
        val start = charOffset - 2
        val leadch: Char = ch
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
        val alt = if (oct == LF) "\\n" else "\\u%04x" format oct
        def msg(what: String) = s"Octal escape literals are $what, use $alt instead."
        deprecationWarning(msg("deprecated"), at = start)
        putChar(oct.toChar)
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

  protected def invalidEscape(): Unit = {
    syntaxError("invalid escape character", at = charOffset - 1)
    putChar(ch)
  }

  @tailrec
  private def getLitChars(delimiter: Char): Boolean = {
    @inline def naturalBreak = (ch == SU || ch == CR || ch == LF) && !isUnicodeEscape
    ch == delimiter || !isAtEnd && !naturalBreak && {
      val offset = charOffset
      getLitChar()
      offset != charOffset && getLitChars(delimiter)
    }
  }

  @inline private def isNumberSeparator(c: Char): Boolean = {
    val isSeparator = c == '_'
    if (isSeparator && !dialect.allowNumericLiteralUnderscoreSeparators)
      syntaxError("numeric separators are not allowed", at = offset)
    else isSeparator
  }

  def checkNoTrailingSeparator(): Unit = {
    val last = cbuf.length() - 1
    if (last >= 0 && isNumberSeparator(cbuf.charAt(last))) {
      syntaxError("trailing separator is not allowed", at = offset + last)
      cbuf.setLength(last)
    }
  }

  /**
   * read fractional part and exponent of floating point number if one is present.
   */
  protected def getFraction(): Unit = {
    while ('0' <= ch && ch <= '9' || isNumberSeparator(ch)) {
      putChar(ch)
      nextChar()
    }
    checkNoTrailingSeparator()
    if (ch == 'e' || ch == 'E') {
      val lookahead = lookaheadReader
      lookahead.nextChar()
      if (lookahead.ch == '+' || lookahead.ch == '-') {
        lookahead.nextChar()
      }
      if ('0' <= lookahead.ch && lookahead.ch <= '9') {
        putChar(ch)
        nextChar()
        if (ch == '+' || ch == '-') {
          putChar(ch)
          nextChar()
        }
        while ('0' <= ch && ch <= '9' || isNumberSeparator(ch)) {
          putChar(ch)
          nextChar()
        }
        checkNoTrailingSeparator()
      }
      token = DOUBLELIT
    }
    if (ch == 'd' || ch == 'D') {
      putChar(ch)
      nextChar()
      token = DOUBLELIT
    } else if (ch == 'f' || ch == 'F') {
      putChar(ch)
      nextChar()
      token = FLOATLIT
    } else {
      token = DOUBLELIT
    }
    checkNoLetter()
    setStrVal()
  }

  def checkNoLetter(): Unit = {
    if (isIdentifierPart(ch) && ch >= ' ' && !isNumberSeparator(ch))
      syntaxError("Invalid literal number", at = offset)
  }

  /**
   * Read a number into strVal and set base
   */
  protected def getNumber(): Unit = {
    val base1 = if (base < 10) 10 else base
    // Read 8,9's even if format is octal, produce a malformed number error afterwards.
    // At this point, we have already read the first digit, so to tell an innocent 0 apart
    // from an octal literal 0123... (which we want to disallow), we check whether there
    // are any additional digits coming after the first one we have already read.
    var notSingleZero = false
    while (isNumberSeparator(ch) || digit2int(ch, base1) >= 0) {
      putChar(ch)
      nextChar()
      notSingleZero = true
    }
    token = INTLIT

    /* When we know for certain it's a number after using a touch of lookahead */
    def restOfNumber() = {
      putChar(ch)
      nextChar()
      getFraction()
    }
    def restOfUncertainToken() = {
      def isEfd = ch match { case 'e' | 'E' | 'f' | 'F' | 'd' | 'D' => true; case _ => false }
      def isL = ch match { case 'l' | 'L' => true; case _ => false }

      if (base <= 10 && isEfd)
        getFraction()
      else {
        // Checking for base == 8 is not enough, because base = 8 is set
        // as soon as a 0 is read in `case '0'` of method fetchToken.
        if (base == 8 && notSingleZero)
          syntaxError("Non-zero integral values may not have a leading zero.", at = offset)
        if (isL) {
          putChar(ch)
          setStrVal()
          nextChar()
          token = LONGLIT
        } else {
          setStrVal()
          checkNoLetter()
        }
      }
    }

    checkNoTrailingSeparator()

    if (base > 10 || ch != '.')
      restOfUncertainToken()
    else {
      val lookahead = lookaheadReader
      val c = lookahead.getc()

      /* Prohibit 1. */
      if (!isDigit(c))
        return setStrVal()

      val isDefinitelyNumber = (c: @switch) match {
        /** Another digit is a giveaway. */
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          true

        /* Backquoted idents like 22.`foo`. */
        case '`' =>
          return setStrVal()
        /** Note the early return */

        /* These letters may be part of a literal, or a method invocation on an Int.
         */
        case 'd' | 'D' | 'f' | 'F' =>
          !isIdentifierPart(lookahead.getc())

        /* A little more special handling for e.g. 5e7 */
        case 'e' | 'E' =>
          val ch = lookahead.getc()
          !isIdentifierPart(ch) || (isDigit(ch) || ch == '+' || ch == '-')

        case x =>
          !isIdentifierStart(x)
      }
      if (isDefinitelyNumber) restOfNumber()
      else restOfUncertainToken()
    }
  }

  /**
   * Parse character literal if current character is followed by \', or follow with given op and
   * return a symbol literal token
   */
  def charLitOr(op: () => Unit): Unit = {
    putChar(ch)
    nextChar()
    if (ch == '\'') {
      nextChar()
      token = CHARLIT
      setStrVal()
    } else {
      op()
      token = SYMBOLLIT
      strVal = name
    }
  }

  def getXml(): Unit = {
    // 1. Collect positions of scala expressions inside this xml literal.
    import fastparse.Parsed
    val start = offset
    val xmlParser = new XmlParser(dialect)
    val result: Int = fastparse.parse(input.text, xmlParser.XmlExpr(_), startIndex = start) match {
      case x: Parsed.Success[_] => x.index
      case x: Parsed.Failure =>
        syntaxError(
          s"malformed xml literal, expected:\n${x.extra.traced.terminalsMsg}",
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

  def getUnquote(): Unit = {
    require(ch == '$')
    val start = charOffset
    val endInclusive = {
      val exploratoryInput = Input.Slice(input, start, input.chars.length)
      val exploratoryScanner = new LegacyScanner(exploratoryInput, dialect.unquoteVariant())
      exploratoryScanner.reader.nextChar()
      exploratoryScanner.nextToken()
      exploratoryScanner.curr.token match {
        case LBRACE =>
          @tailrec
          def loop(balance: Int): Unit = {
            exploratoryScanner.nextToken()
            exploratoryScanner.curr.token match {
              case LBRACE =>
                loop(balance + 1)
              case RBRACE =>
                if (balance == 1) () // do nothing, this is the end of the unquote
                else loop(balance - 1)
              case _ =>
                loop(balance)
            }
          }
          try {
            loop(balance = 1)
          } catch {
            case TokenizeException(pos, message) =>
              syntaxError(s"invalid unquote: $message", at = start + pos.start)
          }
        case IDENTIFIER | THIS | USCORE =>
        // do nothing, this is the end of the unquote
        case _ =>
          syntaxError(
            "invalid unquote: `$'ident, `$'BlockExpr, `$'this or `$'_ expected",
            at = start
          )
      }
      start + exploratoryScanner.curr.endOffset
    }
    finishComposite(UNQUOTE, endInclusive + 1)
  }

// Errors -----------------------------------------------------------------

  override def toString = token.toString

  /**
   * Initialize scanner; call f on each scanned token data
   */
  def foreach(f: LegacyTokenData => Unit): Unit = {
    nextChar()
    do {
      nextToken()
      f(curr)
    } while (curr.token != EOF)
  }
}
