package scala.meta
package internal
package tokenizers

import java.util.NoSuchElementException
import scala.annotation.{ switch, tailrec }
import scala.collection.{ mutable, immutable }
import scala.language.postfixOps
import mutable.{ ListBuffer, ArrayBuffer }
import Chars._
import LegacyToken._

private[meta] class LegacyScanner(val input: Input.Real, decodeUni: Boolean = true)(implicit val dialect: Dialect) {
  val curr: LegacyTokenData   = new LegacyTokenData {}
  val next: LegacyTokenData   = new LegacyTokenData {}
  val prev: LegacyTokenData   = new LegacyTokenData {}
  val reader: CharArrayReader = new CharArrayReader(input.content, reporter.readerError, decodeUni)
  val reporter: Reporter      = Reporter(input)

  import curr._, reader._, reporter._
  curr.input = this.input
  next.input = this.input
  prev.input = this.input

  private def isDigit(c: Char) = java.lang.Character isDigit c
  private var openComments = 0
  protected def putCommentChar(): Unit = nextChar()

  @tailrec private def skipLineComment(): Unit = ch match {
    case SU | CR | LF =>
    case _            => nextChar() ; skipLineComment()
  }
  private def maybeOpen() {
    putCommentChar()
    if (ch == '*') {
      putCommentChar()
      openComments += 1
    }
  }
  private def maybeClose(): Boolean = {
    putCommentChar()
    (ch == '/') && {
      putCommentChar()
      openComments -= 1
      openComments == 0
    }
  }
  @tailrec final def skipNestedComments(): Unit = ch match {
    case '/' => maybeOpen() ; skipNestedComments()
    case '*' => if (!maybeClose()) skipNestedComments()
    case SU  => incompleteInputError("unclosed comment", at = offset)
    case _   => putCommentChar() ; skipNestedComments()
  }
  def skipDocComment(): Unit = skipNestedComments()
  def skipBlockComment(): Unit = skipNestedComments()

  private def skipToCommentEnd(isLineComment: Boolean) {
    nextChar()
    if (isLineComment) skipLineComment()
    else {
      openComments = 1
      val isDocComment = (ch == '*') && { nextChar(); true }
      if (isDocComment) {
        // Check for the amazing corner case of /**/
        if (ch == '/')
          nextChar()
        else
          skipDocComment()
      }
      else skipBlockComment()
    }
  }

  /** @pre ch == '/'
   *  Returns true if a comment was skipped.
   */
  def skipComment(): Boolean = ch match {
    case '/' | '*' => skipToCommentEnd(isLineComment = ch == '/') ; true
    case _         => false
  }
  def flushDoc(): Unit = ()

  /** To prevent doc comments attached to expressions from leaking out of scope
   *  onto the next documentable entity, they are discarded upon passing a right
   *  brace, bracket, or parenthesis.
   */
  def discardDocBuffer(): Unit = ()

  def isAtEnd = charOffset >= buf.length

  def resume(lastCode: LegacyToken) = {
    token = lastCode
    if (next.token != EMPTY)
      syntaxError("unexpected end of input: possible missing '}' in XML block", at = offset)

    nextToken()
  }

  /** A character buffer for literals
   */
  val cbuf = new StringBuilder

  /** append Unicode character to "cbuf" buffer
   */
  protected def putChar(c: Char) {
//      assert(cbuf.size < 10000, cbuf)
    cbuf.append(c)
  }

  /** Determines whether this scanner should emit identifier deprecation warnings,
   *  e.g. when seeing `macro` or `then`, which are planned to become keywords in future versions of Scala.
   */
  protected def emitIdentifierDeprecationWarnings = true

  /** Clear buffer and set name and token */
  private def finishNamed(idtoken: LegacyToken = IDENTIFIER) {
    name = cbuf.toString
    cbuf.clear()
    token = idtoken
    if (idtoken == IDENTIFIER) {
      if (kw2legacytoken contains name) {
        token = kw2legacytoken(name)
        if (token == IDENTIFIER) {
          if (emitIdentifierDeprecationWarnings)
            deprecationWarning(s"$name is now a reserved word; usage as an identifier is deprecated", at = token)
        }
      }
    }
  }

  /** Clear buffer and set string */
  private def setStrVal() {
    strVal = cbuf.toString
    cbuf.clear()
  }

  /** a stack of tokens which indicates whether line-ends can be statement separators
   *  also used for keeping track of nesting levels.
   *  We keep track of the closing symbol of a region. This can be
   *  RPAREN    if region starts with '('
   *  RBRACKET  if region starts with '['
   *  RBRACE    if region starts with '{'
   *  ARROW     if region starts with `case'
   *  STRINGLIT if region is a string interpolation expression starting with '${'
   *            (the STRINGLIT appears twice in succession on the stack iff the
   *             expression is a multiline string literal).
   */
  var sepRegions: List[LegacyToken] = List()

// Get next token ------------------------------------------------------------

  /** Are we directly in a string interpolation expression?
   */
  private def inStringInterpolation =
    sepRegions.nonEmpty && sepRegions.head == STRINGLIT

  /** Are we directly in a multiline string interpolation expression?
   *  @pre inStringInterpolation
   */
  private def inMultiLineInterpolation =
    inStringInterpolation && sepRegions.tail.nonEmpty && sepRegions.tail.head == STRINGPART

  /** read next token and return last offset
   */
  def skipToken(): Offset = {
    val off = offset
    nextToken()
    off
  }

  /** Produce next token, filling curr TokenData fields of Scanner.
   */
  def nextToken() {
    val lastToken = token
    // Adapt sepRegions according to last token
    (lastToken: @switch) match {
      case LPAREN =>
        sepRegions = RPAREN :: sepRegions
      case LBRACKET =>
        sepRegions = RBRACKET :: sepRegions
      case LBRACE =>
        sepRegions = RBRACE :: sepRegions
      case CASE =>
        sepRegions = ARROW :: sepRegions
      case RBRACE =>
        while (!sepRegions.isEmpty && sepRegions.head != RBRACE)
          sepRegions = sepRegions.tail
        if (!sepRegions.isEmpty)
          sepRegions = sepRegions.tail

        discardDocBuffer()
      case RBRACKET | RPAREN =>
        if (!sepRegions.isEmpty && sepRegions.head == lastToken)
          sepRegions = sepRegions.tail

        discardDocBuffer()
      case ARROW =>
        if (!sepRegions.isEmpty && sepRegions.head == lastToken)
          sepRegions = sepRegions.tail
      case STRINGLIT =>
        if (inMultiLineInterpolation)
          sepRegions = sepRegions.tail.tail
        else if (inStringInterpolation)
          sepRegions = sepRegions.tail
      case _ =>
    }

    // Read a token or copy it from `next` tokenData
    if (next.token == EMPTY) {
      lastOffset = charOffset - 1
      if (lastOffset > 0 && buf(lastOffset) == '\n' && buf(lastOffset - 1) == '\r') {
        lastOffset -= 1
      }
      if (inStringInterpolation) fetchStringPart() else fetchToken()
      if(token == ERROR) {
        if (inMultiLineInterpolation)
          sepRegions = sepRegions.tail.tail
        else if (inStringInterpolation)
          sepRegions = sepRegions.tail
      }
    } else {
      curr copyFrom next
      next.token = EMPTY
    }

    // NOTE: endOffset is used to determine range positions for certain tokens.
    // Most tokens (e.g. `(` or `;`) have constant widths, so their range positions can be calculated trivially from their offsets,
    // however some tokens (see who extends Tok.Dynamic for an exhaustive list) have variable widths,
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

  /** Is current token first one after a newline? */
  private def afterLineEnd(): Boolean =
    lastOffset < lineStartOffset &&
    (lineStartOffset <= offset ||
     lastOffset < lastLineStartOffset && lastLineStartOffset <= offset)

  /** Is there a blank line between the current token and the last one?
   *  @pre  afterLineEnd().
   */
  private def pastBlankLine(): Boolean = {
    var idx = lastOffset
    var ch = buf(idx)
    val end = offset
    while (idx < end) {
      if (ch == LF || ch == FF) {
        do {
          idx += 1; ch = buf(idx)
          if (ch == LF || ch == FF) {
//              println("blank line found at "+lastOffset+":"+(lastOffset to idx).map(buf(_)).toList)
            return true
          }
          if (idx == end) return false
        } while (ch <= ' ')
      }
      idx += 1; ch = buf(idx)
    }
    false
  }

  /** read next token, filling TokenData fields of Scanner.
   */
  protected final def fetchToken() {
    offset = charOffset - 1
    (ch: @switch) match {
      case ' ' | '\t' | CR | LF | FF =>
        token = WHITESPACE
        strVal = ch.toString
        nextChar()
        //nextToken()
      case 'A' | 'B' | 'C' | 'D' | 'E' |
           'F' | 'G' | 'H' | 'I' | 'J' |
           'K' | 'L' | 'M' | 'N' | 'O' |
           'P' | 'Q' | 'R' | 'S' | 'T' |
           'U' | 'V' | 'W' | 'X' | 'Y' |
           'Z' | '$' | '_' |
           'a' | 'b' | 'c' | 'd' | 'e' |
           'f' | 'g' | 'h' | 'i' | 'j' |
           'k' | 'l' | 'm' | 'n' | 'o' |
           'p' | 'q' | 'r' | 's' | 't' |
           'u' | 'v' | 'w' | 'x' | 'y' |  // scala-mode: need to understand multi-line case patterns
           'z' =>
        putChar(ch)
        nextChar()
        getIdentRest()
        if (ch == '"' && token == IDENTIFIER)
          token = INTERPOLATIONID
      case '<' => // is XMLSTART?
        def fetchLT() = {
          val last = if (charOffset >= 2) buf(charOffset - 2) else ' '
          nextChar()
          last match {
            case ' ' | '\t' | '\n' | '{' | '(' | '>' if isNameStart(ch) || ch == '!' || ch == '?' =>
              if (dialect.allowXmlLiterals) token = XMLSTART
              else syntaxError("xml literals are not supported", at = offset)
            case _ =>
              // Console.println("found '<', but last is '"+in.last+"'"); // DEBUG
              putChar('<')
              getOperatorRest()
          }
        }
        fetchLT()
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | /*'<' | */
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\' =>
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
        def fetchDoubleQuote() = {
          if (token == INTERPOLATIONID) {
            nextRawChar()
            if (ch == '\"') {
              val lookahead = lookaheadReader
              lookahead.nextChar()
              if (lookahead.ch == '\"') {
                nextRawChar()                        // now eat it
                offset += 3
                nextRawChar()
                getStringPart(multiLine = true)
                sepRegions = STRINGPART :: sepRegions // indicate string part
                sepRegions = STRINGLIT :: sepRegions // once more to indicate multi line string part
              } else {
                nextChar()
                token = STRINGLIT
                strVal = ""
              }
            } else {
              offset += 1
              getStringPart(multiLine = false)
              sepRegions = STRINGLIT :: sepRegions // indicate single line string part
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
          nextChar()
          if (isIdentifierStart(ch))
            charLitOr(getIdentRest)
          else if (isOperatorPart(ch) && (ch != '\\'))
            charLitOr(getOperatorRest)
          else {
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
        fetchSingleQuote()
      case '.' =>
        nextChar()
        if ('0' <= ch && ch <= '9') {
          putChar('.'); getFraction()
        } else if (dialect.allowEllipses && ch == '.') {
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
        if (isAtEnd) token = EOF
        else {
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
            syntaxError("illegal character '" + ("" + '\\' + 'u' + "%04x".format(ch.toInt)) + "'", at = offset)
            nextChar()
          }
        }
        fetchOther()
    }
  }

// Identifiers ---------------------------------------------------------------

  private def getBackquotedIdent() {
    nextChar()
    getLitChars('`')
    if (ch == '`') {
      nextChar()
      finishNamed(BACKQUOTED_IDENT)
      if (name.length == 0) syntaxError("empty quoted identifier", at = offset)
    }
    else syntaxError("unclosed quoted identifier", at = offset)
  }

  private def getIdentRest(): Unit = (ch: @switch) match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
         'F' | 'G' | 'H' | 'I' | 'J' |
         'K' | 'L' | 'M' | 'N' | 'O' |
         'P' | 'Q' | 'R' | 'S' | 'T' |
         'U' | 'V' | 'W' | 'X' | 'Y' |
         'Z' | '$' |
         'a' | 'b' | 'c' | 'd' | 'e' |
         'f' | 'g' | 'h' | 'i' | 'j' |
         'k' | 'l' | 'm' | 'n' | 'o' |
         'p' | 'q' | 'r' | 's' | 't' |
         'u' | 'v' | 'w' | 'x' | 'y' |
         'z' |
         '0' | '1' | '2' | '3' | '4' |
         '5' | '6' | '7' | '8' | '9' =>
      putChar(ch)
      nextChar()
      getIdentRest()
    case '_' =>
      putChar(ch)
      nextChar()
      getIdentOrOperatorRest()
    case SU => // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
      finishNamed()
    case _ =>
      if (Character.isUnicodeIdentifierPart(ch)) {
        putChar(ch)
        nextChar()
        getIdentRest()
      } else {
        finishNamed()
      }
  }

  private def getOperatorRest(): Unit = (ch: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '\\' =>
      putChar(ch); nextChar(); getOperatorRest()
    case '/' =>
      val lookahead = lookaheadReader
      lookahead.nextChar()
      if (lookahead.ch == '/' || lookahead.ch == '*') {
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

  private def getIdentOrOperatorRest() {
    if (isIdentifierPart(ch))
      getIdentRest()
    else ch match {
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\' | '/' =>
        getOperatorRest()
      case _ =>
        if (isSpecial(ch)) getOperatorRest()
        else finishNamed()
    }
  }


// Literals -----------------------------------------------------------------

  private def getStringLit() = {
    getLitChars('"')
    if (ch == '"') {
      setStrVal()
      nextChar()
      token = STRINGLIT
    } else syntaxError("unclosed string literal", at = offset)
  }

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
    } else {
      putChar(ch)
      nextRawChar()
      getRawStringLit()
    }
  }

  @scala.annotation.tailrec private def getStringPart(multiLine: Boolean): Unit = {
    def finishStringPart() = {
      setStrVal()
      token = STRINGPART
      next.lastOffset = charOffset - 1
      next.offset = charOffset - 1
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
    } else if (ch == '$') {
      nextRawChar()
      if (ch == '$') {
        putChar(ch)
        nextRawChar()
        getStringPart(multiLine)
      } else if (ch == '{') {
        finishStringPart()
        endOffset = charOffset - 3
        nextRawChar()
        next.token = LBRACE
      } else if (ch == '_') {
        finishStringPart()
        endOffset = charOffset - 3
        nextRawChar()
        next.token = USCORE
      } else if (Character.isUnicodeIdentifierStart(ch)) {
        finishStringPart()
        endOffset = charOffset - 3
        do {
          putChar(ch)
          nextRawChar()
        } while (ch != SU && Character.isUnicodeIdentifierPart(ch))
        next.token = IDENTIFIER
        next.name = cbuf.toString
        cbuf.clear()
        if (kw2legacytoken contains next.name) {
          next.token = kw2legacytoken(next.name)
        }
      } else {
        syntaxError("invalid string interpolation: `$$', `$'ident or `$'BlockExpr expected", at = offset)
      }
    } else {
      val isUnclosedLiteral = !isUnicodeEscape && (ch == SU || (!multiLine && (ch == CR || ch == LF)))
      if (isUnclosedLiteral) {
        if (multiLine)
          incompleteInputError("unclosed multi-line string literal", at = offset)
        else
          syntaxError("unclosed string literal", at = offset)
      }
      else {
        putChar(ch)
        nextRawChar()
        getStringPart(multiLine)
      }
    }
  }

  private def fetchStringPart() = {
    offset = charOffset - 1
    getStringPart(multiLine = inMultiLineInterpolation)
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

  /** copy current character into cbuf, interpreting any escape sequences,
   *  and advance to next character.
   */
  protected def getLitChar(): Unit =
    if (ch == '\\') {
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
        // TODO: syntax profile?
        // if (settings.future)
        //   syntaxError(start, msg("unsupported"))
        // else
          deprecationWarning(msg("deprecated"), at = start)
        putChar(oct.toChar)
      } else {
        ch match {
          case 'b'  => putChar('\b')
          case 't'  => putChar('\t')
          case 'n'  => putChar('\n')
          case 'f'  => putChar('\f')
          case 'r'  => putChar('\r')
          case '\"' => putChar('\"')
          case '\'' => putChar('\'')
          case '\\' => putChar('\\')
          case _    => invalidEscape()
        }
        nextChar()
      }
    } else  {
      putChar(ch)
      nextChar()
    }

  protected def invalidEscape(): Unit = {
    syntaxError("invalid escape character", at = charOffset - 1)
    putChar(ch)
  }

  private def getLitChars(delimiter: Char) = {
    while (ch != delimiter && !isAtEnd && (ch != SU && ch != CR && ch != LF || isUnicodeEscape))
      getLitChar()
  }

  /** read fractional part and exponent of floating point number
   *  if one is present.
   */
  protected def getFraction() {
    token = DOUBLELIT
    while ('0' <= ch && ch <= '9') {
      putChar(ch)
      nextChar()
    }
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
        while ('0' <= ch && ch <= '9') {
          putChar(ch)
          nextChar()
        }
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
    }
    checkNoLetter()
    setStrVal()
  }

  def checkNoLetter() {
    if (isIdentifierPart(ch) && ch >= ' ')
      syntaxError("Invalid literal number", at = offset)
  }

  /** Read a number into strVal and set base
  */
  protected def getNumber() {
    val base1 = if (base < 10) 10 else base
    // Read 8,9's even if format is octal, produce a malformed number error afterwards.
    // At this point, we have already read the first digit, so to tell an innocent 0 apart
    // from an octal literal 0123... (which we want to disallow), we check whether there
    // are any additional digits coming after the first one we have already read.
    var notSingleZero = false
    while (digit2int(ch, base1) >= 0) {
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
      def isEfd = ch match { case 'e' | 'E' | 'f' | 'F' | 'd' | 'D' => true ; case _ => false }
      def isL   = ch match { case 'l' | 'L' => true ; case _ => false }

      if (base <= 10 && isEfd)
        getFraction()
      else {
        // Checking for base == 8 is not enough, because base = 8 is set
        // as soon as a 0 is read in `case '0'` of method fetchToken.
        if (base == 8 && notSingleZero) syntaxError("Non-zero integral values may not have a leading zero.", at = offset)
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
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
          true

        /* Backquoted idents like 22.`foo`. */
        case '`' =>
          return setStrVal()  /** Note the early return */

        /* These letters may be part of a literal, or a method invocation on an Int.
         */
        case 'd' | 'D' | 'f' | 'F' =>
          !isIdentifierPart(lookahead.getc())

        /* A little more special handling for e.g. 5e7 */
        case 'e' | 'E' =>
          val ch = lookahead.getc()
          !isIdentifierPart(ch) || (isDigit(ch) || ch == '+' || ch == '-')

        case x  =>
          !isIdentifierStart(x)
      }
      if (isDefinitelyNumber) restOfNumber()
      else restOfUncertainToken()
    }
  }

  /** Parse character literal if current character is followed by \',
   *  or follow with given op and return a symbol literal token
   */
  def charLitOr(op: () => Unit) {
    putChar(ch)
    nextChar()
    if (ch == '\'') {
      nextChar()
      token = CHARLIT
      setStrVal()
    } else {
      op()
      token = SYMBOLLIT
      strVal = name.toString
    }
  }

// Errors -----------------------------------------------------------------

  override def toString() = token.toString

  /** Initialize scanner; call f on each scanned token data
   */
  def foreach(f: LegacyTokenData => Unit) {
    nextChar()
    nextToken()
    f(curr)
    while(curr.token != EOF) {
      nextToken()
      f(curr)
    }
  }
}
