package scala.meta
package internal
package tokenizers

import org.scalameta._
import org.scalameta.invariants._
import scala.meta.inputs._
import scala.meta.internal.tokenizers.ScalametaTokenizer.UnexpectedInputEndException
import scala.meta.tokenizers._
import scala.meta.tokens._

import scala.annotation.tailrec

class ScalametaTokenizer(input: Input, dialect: Dialect) {
  import LegacyToken._

  def tokenize(): Tokens = input.tokenCache.getOrElseUpdate(dialect, uncachedTokenize())

  private def uncachedTokenize(): Tokens = {
    val legacyTokens: Array[LegacyTokenData] = {
      val scanner = new LegacyScanner(input, dialect)
      val legacyTokenBuf = new java.util.ArrayList[LegacyTokenData]()
      scanner.foreach(curr => legacyTokenBuf.add(new LegacyTokenData {}.copyFrom(curr)))
      val underlying = new Array[LegacyTokenData](legacyTokenBuf.size())
      legacyTokenBuf.toArray(underlying)
      underlying
    }

    val tokens = new java.util.ArrayList[Token](legacyTokens.length)
    @inline
    def pushToken(token: Token): Unit = tokens.add(token)
    pushToken(new Token.BOF(input, dialect))

    // tokens is non-empty, contains BOF
    def lastEmittedToken: Token = tokens.get(tokens.size() - 1)
    def isAtLineStart: Boolean = lastEmittedToken.isInstanceOf[Token.AtEOLorF]

    def loop(startingFrom: Int, braceBalance: Int = 0): Int = {
      if (startingFrom >= legacyTokens.length) return startingFrom

      var legacyIndex = startingFrom
      def prev = legacyTokens(legacyIndex - 1)
      def curr =
        if (legacyIndex < legacyTokens.length) legacyTokens(legacyIndex)
        else throw new UnexpectedInputEndException()

      @inline
      def nextToken() = legacyIndex += 1
      def pushTokenAndNext(token: Token): Unit = {
        pushToken(token)
        nextToken()
      }
      @inline
      def getCurrToken(): Token = getToken(curr)
      def getNextTokenOrFail(): Token = {
        nextToken()
        getCurrToken()
      }

      def emitTokenWhitespace(token: Token.Whitespace): Unit = pushTokenAndNext(token)
      def emitTokenInterpolation(token: Token.Interpolation.Id) = {
        def pushPart(end: Offset) =
          pushToken(Token.Interpolation.Part(input, dialect, curr.offset, end, curr.strVal))
        @tailrec
        def emitContents(): Unit =
          if (curr.token == STRINGPART) {
            val dollarOffset = curr.endOffset + 1
            require(input.chars(dollarOffset) == '$')
            pushPart(dollarOffset)
            val postDollarOffset = dollarOffset + 1
            pushToken(
              Token.Interpolation.SpliceStart(input, dialect, dollarOffset, postDollarOffset)
            )
            val spliceToken = getNextTokenOrFail()
            pushToken(spliceToken)
            nextToken()
            prev.token match {
              case LBRACE => legacyIndex = loop(legacyIndex, braceBalance = 1)
              case IDENTIFIER | THIS =>
              case USCORE if input.chars(postDollarOffset) == '_' =>
              case _ => unreachable(debug(prev), s"unexpected interpolation: $spliceToken")
            }
            pushToken(Token.Interpolation.SpliceEnd(input, dialect, curr.offset, curr.offset))
            emitContents()
          } else require(curr.token == STRINGLIT)

        // NOTE: before emitStart, curr is the first token that follows INTERPOLATIONID
        // i.e. STRINGLIT (if the interpolation is empty) or STRINGPART (if it's not)
        // NOTE: before emitEnd, curr is the first token that follows the concluding STRINGLIT of the interpolation
        // for example, EOF in the case of `q""` or `q"$foobar"`
        pushToken(token)
        nextToken()
        val numQuotes = curr.offset - token.end

        pushToken(Token.Interpolation.Start(input, dialect, token.end, curr.offset))
        emitContents()

        val endEndPos = curr.endOffset + 1
        val endBegPos = endEndPos - numQuotes
        require(input.chars(endBegPos) == '\"')
        pushPart(endBegPos)

        pushToken(Token.Interpolation.End(input, dialect, endBegPos, endEndPos))
        nextToken()
      }
      def emitTokenXml(token: Token.Xml.Part) = {
        @tailrec
        def emitContents(): Unit = curr.token match {
          case XMLLIT =>
            pushToken(getXmlPart(curr))
            nextToken()
            emitContents()

          case LBRACE =>
            // We are at the start of an embedded scala expression
            pushToken(Token.Xml.SpliceStart(input, dialect, curr.offset, curr.offset))
            pushToken(getCurrToken())
            nextToken()
            legacyIndex = loop(legacyIndex, braceBalance = 1)
            pushToken(Token.Xml.SpliceEnd(input, dialect, curr.offset, curr.offset))
            emitContents()

          case XMLLITEND =>
            // We have reached the final xml part
            val xmlEndIndex = curr.endOffset + 1
            pushToken(Token.Xml.End(input, dialect, xmlEndIndex, xmlEndIndex))
            nextToken()
        }

        pushToken(Token.Xml.Start(input, dialect, token.start, token.start))
        pushToken(token)
        nextToken()
        emitContents()
      }
      @tailrec
      def emitToken(token: Token): Unit = token match {
        case _: Token.At if input.isInstanceOf[Input.Ammonite] && isAtLineStart =>
          getNextTokenOrFail() match {
            case t: Token.Whitespace =>
              pushToken(Token.EOF(input, dialect, token.start))
              pushToken(token)
              pushToken(Token.BOF(input, dialect, token.end))
              emitTokenWhitespace(t)
            case t =>
              pushToken(token)
              emitToken(t)
          }
        case t: Token.Whitespace => emitTokenWhitespace(t)
        case t: Token.Interpolation.Id => emitTokenInterpolation(t)
        case t: Token.Xml.Part => emitTokenXml(t)
        case _ => pushTokenAndNext(token)
      }

      emitToken(getCurrToken())

      lastEmittedToken match {
        case _: Token.RightBrace if braceBalance > 1 => loop(legacyIndex, braceBalance - 1)
        case _: Token.LeftBrace if braceBalance > 0 => loop(legacyIndex, braceBalance + 1)
        case _: Token.RightBrace => require(braceBalance > 0); legacyIndex
        case _: Token.LeftBrace => loop(loop(legacyIndex, 1))
        case _ => loop(legacyIndex, braceBalance)
      }
    }

    try loop(startingFrom = 0)
    catch {
      // ignore when token not correctly closed at the end
      case _: UnexpectedInputEndException => ()
    }
    val underlying = new Array[Token](tokens.size())
    tokens.toArray(underlying)
    Tokens(underlying, 0, underlying.length)
  }

  private def getXmlPart(curr: LegacyTokenData): Token = {
    val beg = curr.offset
    val end = curr.endOffset + 1
    val part = new String(input.chars, beg, end - beg)
    Token.Xml.Part(input, dialect, beg, end, part)
  }

  private def getToken(curr: LegacyTokenData): Token = {
    (curr.token: @scala.annotation.switch) match {
      case IDENTIFIER => Token.Ident(input, dialect, curr.offset, curr.endOffset + 1, curr.name)
      case INTLIT => Token.Constant.Int(input, dialect, curr.offset, curr.endOffset + 1, curr.intVal)
      case LONGLIT => Token.Constant
          .Long(input, dialect, curr.offset, curr.endOffset + 1, curr.longVal)
      case FLOATLIT => Token.Constant
          .Float(input, dialect, curr.offset, curr.endOffset + 1, curr.floatVal)
      case DOUBLELIT => Token.Constant
          .Double(input, dialect, curr.offset, curr.endOffset + 1, curr.doubleVal)
      case CHARLIT => Token.Constant
          .Char(input, dialect, curr.offset, curr.endOffset + 1, curr.charVal)
      case SYMBOLLIT => Token.Constant
          .Symbol(input, dialect, curr.offset, curr.endOffset + 1, scala.Symbol(curr.strVal))
      case STRINGLIT => Token.Constant
          .String(input, dialect, curr.offset, curr.endOffset + 1, curr.strVal)
      case STRINGPART => unreachable
      case TRUE => Token.KwTrue(input, dialect, curr.offset)
      case FALSE => Token.KwFalse(input, dialect, curr.offset)
      case NULL => Token.KwNull(input, dialect, curr.offset)

      case INTERPOLATIONID => Token.Interpolation
          .Id(input, dialect, curr.offset, curr.endOffset + 1, curr.name)
      case XMLLIT => getXmlPart(curr)
      case XMLLITEND => unreachable

      case NEW => Token.KwNew(input, dialect, curr.offset)
      case THIS => Token.KwThis(input, dialect, curr.offset)
      case SUPER => Token.KwSuper(input, dialect, curr.offset)

      case IMPLICIT => Token.KwImplicit(input, dialect, curr.offset)
      case OVERRIDE => Token.KwOverride(input, dialect, curr.offset)
      case PROTECTED => Token.KwProtected(input, dialect, curr.offset)
      case PRIVATE => Token.KwPrivate(input, dialect, curr.offset)
      case ABSTRACT => Token.KwAbstract(input, dialect, curr.offset)
      case FINAL => Token.KwFinal(input, dialect, curr.offset)
      case SEALED => Token.KwSealed(input, dialect, curr.offset)
      case LAZY => Token.KwLazy(input, dialect, curr.offset)
      case MACRO => Token.KwMacro(input, dialect, curr.offset)

      case PACKAGE => Token.KwPackage(input, dialect, curr.offset)
      case IMPORT => Token.KwImport(input, dialect, curr.offset)
      case EXPORT => Token.KwExport(input, dialect, curr.offset)
      case CLASS => Token.KwClass(input, dialect, curr.offset)
      case CASECLASS => unreachable
      case OBJECT => Token.KwObject(input, dialect, curr.offset)
      case CASEOBJECT => unreachable
      case TRAIT => Token.KwTrait(input, dialect, curr.offset)
      case EXTENDS => Token.KwExtends(input, dialect, curr.offset)
      case WITH => Token.KwWith(input, dialect, curr.offset)
      case TYPE => Token.KwType(input, dialect, curr.offset)
      case FORSOME => Token.KwForsome(input, dialect, curr.offset)
      case DEF => Token.KwDef(input, dialect, curr.offset)
      case VAL => Token.KwVal(input, dialect, curr.offset)
      case VAR => Token.KwVar(input, dialect, curr.offset)
      case ENUM => Token.KwEnum(input, dialect, curr.offset)
      case GIVEN => Token.KwGiven(input, dialect, curr.offset)

      case IF => Token.KwIf(input, dialect, curr.offset)
      case ELSE => Token.KwElse(input, dialect, curr.offset)
      case WHILE => Token.KwWhile(input, dialect, curr.offset)
      case DO => Token.KwDo(input, dialect, curr.offset)
      case FOR => Token.KwFor(input, dialect, curr.offset)
      case YIELD => Token.KwYield(input, dialect, curr.offset)
      case THEN => Token.KwThen(input, dialect, curr.offset)
      case THROW => Token.KwThrow(input, dialect, curr.offset)
      case TRY => Token.KwTry(input, dialect, curr.offset)
      case CATCH => Token.KwCatch(input, dialect, curr.offset)
      case FINALLY => Token.KwFinally(input, dialect, curr.offset)
      case CASE => Token.KwCase(input, dialect, curr.offset)
      case RETURN => Token.KwReturn(input, dialect, curr.offset)
      case MATCH => Token.KwMatch(input, dialect, curr.offset)

      case LPAREN => Token.LeftParen(input, dialect, curr.offset)
      case RPAREN => Token.RightParen(input, dialect, curr.offset)
      case LBRACKET => Token.LeftBracket(input, dialect, curr.offset)
      case RBRACKET => Token.RightBracket(input, dialect, curr.offset)
      case LBRACE => Token.LeftBrace(input, dialect, curr.offset)
      case RBRACE => Token.RightBrace(input, dialect, curr.offset)

      case COMMA => Token.Comma(input, dialect, curr.offset)
      case SEMI => Token.Semicolon(input, dialect, curr.offset)
      case DOT => Token.Dot(input, dialect, curr.offset)
      case COLON => Token.Colon(input, dialect, curr.offset)
      case EQUALS => Token.Equals(input, dialect, curr.offset)
      case AT => Token.At(input, dialect, curr.offset)
      case HASH => Token.Hash(input, dialect, curr.offset)
      case USCORE => Token.Underscore(input, dialect, curr.offset)
      case ARROW => Token.RightArrow(input, dialect, curr.offset, curr.endOffset + 1)
      case LARROW => Token.LeftArrow(input, dialect, curr.offset, curr.endOffset + 1)
      case SUBTYPE => Token.Subtype(input, dialect, curr.offset)
      case SUPERTYPE => Token.Supertype(input, dialect, curr.offset)
      case VIEWBOUND => Token.Viewbound(input, dialect, curr.offset)
      case TYPELAMBDAARROW => Token.TypeLambdaArrow(input, dialect, curr.offset)
      case CTXARROW => Token.ContextArrow(input, dialect, curr.offset)

      case MACROQUOTE => Token.MacroQuote(input, dialect, curr.offset)
      case MACROSPLICE => Token.MacroSplice(input, dialect, curr.offset)

      case WHITESPACE_SPC => Token.Space(input, dialect, curr.offset)
      case WHITESPACE_TAB => Token.Tab(input, dialect, curr.offset)
      case WHITESPACE_CR => Token.CR(input, dialect, curr.offset)
      case WHITESPACE_LF => Token.LF(input, dialect, curr.offset)
      case WHITESPACE_FF => Token.FF(input, dialect, curr.offset)

      case COMMENT =>
        var value = new String(input.chars, curr.offset, curr.endOffset - curr.offset + 1)
        if (value.startsWith("//")) value = value.stripPrefix("//")
        if (value.startsWith("/*")) value = value.stripPrefix("/*").stripSuffix("*/")
        Token.Comment(input, dialect, curr.offset, curr.endOffset + 1, value)

      case ELLIPSIS => Token.Ellipsis(input, dialect, curr.offset, curr.endOffset + 1, curr.base)
      case UNQUOTE => Token.Unquote(input, dialect, curr.offset, curr.endOffset + 1)

      case EOF => new Token.EOF(input, dialect)

      case EMPTY => unreachable
      case UNDEF => unreachable
      case ERROR => unreachable
    }
  }

}

object ScalametaTokenizer {
  class UnexpectedInputEndException() extends Exception

  def toTokenize: Tokenize = new Tokenize {
    def apply(input: Input, dialect: Dialect): Tokenized =
      try {
        val tokenizer = new ScalametaTokenizer(input, dialect)
        Tokenized.Success(tokenizer.tokenize())
      } catch {
        case details @ TokenizeException(pos, message) => Tokenized.Error(pos, message, details)
      }
  }
}
