package scala.meta
package internal
package tokenizers

import org.scalameta._
import scala.meta.inputs._
import scala.meta.tokenizers._
import scala.meta.tokens._

import scala.annotation.tailrec

class ScalametaTokenizer(input: Input, dialect: Dialect)(implicit options: TokenizerOptions) {
  import LegacyToken._

  def tokenize(): Tokens = input.tokenCache.getOrElseUpdate(dialect, uncachedTokenize())

  private def uncachedTokenize(): Tokens = {
    val scanner = new LegacyScanner(input, dialect)
    scanner.initialize(bof = true)

    implicit val tokens = new java.util.ArrayList[Token]()
    val whitespaceTokenizer: WhitespaceTokenizer = WhitespaceTokenizer(input, dialect)

    @inline
    def pushToken(token: Token): Unit = tokens.add(token)
    pushToken(new Token.BOF(input, dialect))

    def nextToken(): LegacyTokenData = {
      val nt = scanner.nextToken()
      if (nt.token == INVALID) {
        pushToken(getToken(nt))
        scanner.nextToken()
      } else nt
    }

    // tokens is non-empty, contains BOF
    def lastEmittedToken: Token = tokens.get(tokens.size() - 1)
    def isAtLineStart: Boolean = lastEmittedToken.isInstanceOf[Token.AtEOLorF]

    @tailrec
    def emitTokenWhitespace(token: Token.Whitespace): Token = {
      val next = nextToken() // might output INVALID token for current whitespace
      token match {
        case t: Token.HSpace => whitespaceTokenizer.pushHS(t)
        case t: Token.EOL => whitespaceTokenizer.pushVS(t)
        case t => unreachable(debug(t), "not a whitespace token")
      }
      getToken(next) match {
        case nt: Token.Whitespace => emitTokenWhitespace(nt)
        case nt => whitespaceTokenizer.flush(); nt
      }
    }
    def emitTokenInterpolation(token: Token.Interpolation.Id) = {
      def pushPart(curr: LegacyTokenData) =
        pushToken(Token.Interpolation.Part(input, dialect, curr.offset, curr.endOffset, curr.strVal))
      @tailrec
      def emitContents(beg: LegacyTokenData): LegacyTokenData =
        if (beg.token == STRINGPART) {
          val dollarOffset = beg.endOffset
          pushPart(beg)
          pushToken(Token.Interpolation.SpliceStart(input, dialect, dollarOffset, dollarOffset + 1))
          val splice = nextToken()
          val end =
            if (splice.token == STRINGPART) splice
            else {
              pushToken(getToken(splice))
              if (splice.token == LBRACE) loop(braceBalance = 1)
              nextToken()
            }
          pushToken(Token.Interpolation.SpliceEnd(input, dialect, end.offset, end.offset))
          emitContents(end)
        } else beg

      // NOTE: before emitStart, curr is the first token that follows INTERPOLATIONID
      // i.e. STRINGLIT (if the interpolation is empty) or STRINGPART (if it's not)
      // NOTE: before emitEnd, curr is the first token that follows the concluding STRINGLIT of the interpolation
      // for example, EOF in the case of `q""` or `q"$foobar"`
      pushToken(token)
      val beg = nextToken()

      pushToken(Token.Interpolation.Start(input, dialect, token.end, beg.offset))
      val end = emitContents(beg)

      val endEndPos = end.endOffset
      pushPart(end)

      val next = nextToken()
      pushToken(Token.Interpolation.End(input, dialect, endEndPos, next.offset))

      getToken(next)
    }
    def emitTokenXml(token: Token.Xml.Part) = {
      @tailrec
      def emitContents(beg: LegacyTokenData): Unit = beg.token match {
        case XMLLIT =>
          pushToken(getXmlPart(beg))
          emitContents(nextToken())

        case LBRACE =>
          // We are at the start of an embedded scala expression
          pushToken(Token.Xml.SpliceStart(input, dialect, beg.offset, beg.offset))
          pushToken(getToken(beg))
          loop(braceBalance = 1)
          val end = nextToken()
          pushToken(Token.Xml.SpliceEnd(input, dialect, end.offset, end.offset))
          emitContents(end)

        case XMLLITEND =>
          // We have reached the final xml part
          val xmlEndIndex = beg.endOffset
          pushToken(Token.Xml.End(input, dialect, xmlEndIndex, xmlEndIndex))
      }

      pushToken(Token.Xml.Start(input, dialect, token.start, token.start))
      pushToken(token)
      emitContents(nextToken())
    }
    @tailrec
    def emitToken(token: Token): Unit = token match {
      case _: Token.At if input.isInstanceOf[Input.Ammonite] && isAtLineStart =>
        getToken(nextToken()) match {
          case t: Token.Whitespace =>
            pushToken(Token.EOF(input, dialect, token.start))
            pushToken(token)
            pushToken(Token.BOF(input, dialect, token.end))
            emitToken(emitTokenWhitespace(t))
          case t =>
            pushToken(token)
            emitToken(t)
        }
      case t: Token.Whitespace => emitToken(emitTokenWhitespace(t))
      case t: Token.Interpolation.Id => emitToken(emitTokenInterpolation(t))
      case t: Token.Xml.Part => emitTokenXml(t)
      case _ => pushToken(token)
    }

    def loop(braceBalance: Int = 0): LegacyTokenData = {
      val curr = scanner.nextTokenOrEof()
      if (curr.ok) {
        emitToken(getToken(curr))
        lastEmittedToken match {
          case _: Token.RightBrace if braceBalance == 1 => curr // done
          case _: Token.RightBrace if braceBalance > 1 => loop(braceBalance - 1)
          case _: Token.LeftBrace if braceBalance > 0 => loop(braceBalance + 1)
          case _ => loop(braceBalance)
        }
      } else curr
    }

    try loop()
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
    val end = curr.endOffset
    val part = new String(input.chars, beg, end - beg)
    Token.Xml.Part(input, dialect, beg, end, part)
  }

  private def getToken(curr: LegacyTokenData): Token = {
    (curr.token: @scala.annotation.switch) match {
      case IDENTIFIER => Token.Ident(input, dialect, curr.offset, curr.endOffset, curr.strVal)
      case INTLIT => curr.intVal
          .fold(getInvalid(curr, _), Token.Constant.Int(input, dialect, curr.offset, curr.endOffset, _))
      case LONGLIT => curr.longVal
          .fold(getInvalid(curr, _), Token.Constant.Long(input, dialect, curr.offset, curr.endOffset, _))
      case FLOATLIT => curr.floatVal.fold(
          getInvalid(curr, _),
          Token.Constant.Float(input, dialect, curr.offset, curr.endOffset, _)
        )
      case DOUBLELIT => curr.doubleVal.fold(
          getInvalid(curr, _),
          Token.Constant.Double(input, dialect, curr.offset, curr.endOffset, _)
        )
      case CHARLIT => Token.Constant.Char(input, dialect, curr.offset, curr.endOffset, curr.charVal)
      case SYMBOLLIT => Token.Constant
          .Symbol(input, dialect, curr.offset, curr.endOffset, scala.Symbol(curr.strVal))
      case STRINGLIT => Token.Constant
          .String(input, dialect, curr.offset, curr.endOffset, curr.strVal)
      case TRUE => Token.KwTrue(input, dialect, curr.offset)
      case FALSE => Token.KwFalse(input, dialect, curr.offset)
      case NULL => Token.KwNull(input, dialect, curr.offset)

      case INTERPOLATIONID => Token.Interpolation
          .Id(input, dialect, curr.offset, curr.endOffset, curr.strVal)
      case XMLLIT => getXmlPart(curr)

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
      case OBJECT => Token.KwObject(input, dialect, curr.offset)
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
      case ARROW => Token.RightArrow(input, dialect, curr.offset, curr.endOffset)
      case LARROW => Token.LeftArrow(input, dialect, curr.offset, curr.endOffset)
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
      case WHITESPACE_CRLF => Token.CRLF(input, dialect, curr.offset)
      case WHITESPACE_FF => Token.FF(input, dialect, curr.offset)

      case COMMENT =>
        var value = new String(input.chars, curr.offset, curr.endOffset - curr.offset)
        if (value.startsWith("//")) value = value.stripPrefix("//")
        if (value.startsWith("/*")) value = value.stripPrefix("/*").stripSuffix("*/")
        Token.Comment(input, dialect, curr.offset, curr.endOffset, value)

      case ELLIPSIS => Token.Ellipsis(input, dialect, curr.offset, curr.endOffset, curr.base)
      case UNQUOTE => Token.Unquote(input, dialect, curr.offset, curr.endOffset)

      case EOF => new Token.EOF(input, dialect)
      case SHEBANG => new Token.Shebang(input, dialect, curr.offset, curr.endOffset, curr.strVal)
      case INVALID => getInvalid(curr, curr.strVal)
      case _ => getInvalid(curr, s"Unexpected token id ${curr.token}, contents:\n" + curr.strVal)
    }
  }

  private def getInvalid(curr: LegacyTokenData, error: String): Token.Invalid =
    new Token.Invalid(input, dialect, curr.offset, curr.endOffset, error)

}

object ScalametaTokenizer {
  def toTokenize: Tokenize = new Tokenize {
    def apply(input: Input, dialect: Dialect): Tokenized = {
      implicit val options: TokenizerOptions = input.tokenizerOptions
      val tokenizer = new ScalametaTokenizer(input.withoutTokenizerOptions, dialect)
      try Tokenized.Success(tokenizer.tokenize())
      catch {
        case details @ TokenizeException(pos, message) => Tokenized.Error(pos, message, details)
      }
    }
  }
}
