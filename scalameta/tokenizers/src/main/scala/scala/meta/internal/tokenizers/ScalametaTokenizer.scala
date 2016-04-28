package scala.meta
package internal
package tokenizers

import scala.collection.{immutable, mutable}
import org.scalameta._
import org.scalameta.invariants._
import Chars.{CR, LF, FF}
import LegacyToken._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokenizers._

private[meta] class ScalametaTokenizer(val content: Content)(implicit val dialect: Dialect) {
  def tokenize(): Tokens.Tokenized = {
    def legacyTokenToToken(curr: LegacyTokenData): Token = {
      (curr.token: @scala.annotation.switch) match {
        case IDENTIFIER       => Token.Ident(content, dialect, curr.offset, curr.endOffset + 1, curr.name)
        case BACKQUOTED_IDENT => Token.Ident(content, dialect, curr.offset, curr.endOffset + 1, "`" + curr.name + "`")

        case INTLIT          => Token.Constant.Int(content, dialect, curr.offset, curr.endOffset + 1, curr.intVal)
        case LONGLIT         => Token.Constant.Long(content, dialect, curr.offset, curr.endOffset + 1, curr.longVal)
        case FLOATLIT        => Token.Constant.Float(content, dialect, curr.offset, curr.endOffset + 1, curr.floatVal)
        case DOUBLELIT       => Token.Constant.Double(content, dialect, curr.offset, curr.endOffset + 1, curr.doubleVal)
        case CHARLIT         => Token.Constant.Char(content, dialect, curr.offset, curr.endOffset + 1, curr.charVal)
        case SYMBOLLIT       => Token.Constant.Symbol(content, dialect, curr.offset, curr.endOffset + 1, scala.Symbol(curr.strVal))
        case STRINGLIT       => Token.Constant.String(content, dialect, curr.offset, curr.endOffset + 1, curr.strVal)
        case TRUE            => Token.KwTrue(content, dialect, curr.offset)
        case FALSE           => Token.KwFalse(content, dialect, curr.offset)
        case NULL            => Token.KwNull(content, dialect, curr.offset)

        case INTERPOLATIONID => Token.Interpolation.Id(content, dialect, curr.offset, curr.endOffset + 1)
        case XMLSTART        => Token.Xml.Start(content, dialect, curr.offset, curr.offset)

        case NEW   => Token.KwNew(content, dialect, curr.offset)
        case THIS  => Token.KwThis(content, dialect, curr.offset)
        case SUPER => Token.KwSuper(content, dialect, curr.offset)

        case IMPLICIT  => Token.KwImplicit(content, dialect, curr.offset)
        case OVERRIDE  => Token.KwOverride(content, dialect, curr.offset)
        case PROTECTED => Token.KwProtected(content, dialect, curr.offset)
        case PRIVATE   => Token.KwPrivate(content, dialect, curr.offset)
        case ABSTRACT  => Token.KwAbstract(content, dialect, curr.offset)
        case FINAL     => Token.KwFinal(content, dialect, curr.offset)
        case SEALED    => Token.KwSealed(content, dialect, curr.offset)
        case LAZY      => Token.KwLazy(content, dialect, curr.offset)
        case MACRO     => Token.KwMacro(content, dialect, curr.offset)

        case PACKAGE    => Token.KwPackage(content, dialect, curr.offset)
        case IMPORT     => Token.KwImport(content, dialect, curr.offset)
        case CLASS      => Token.KwClass(content, dialect, curr.offset)
        case CASECLASS  => unreachable
        case OBJECT     => Token.KwObject(content, dialect, curr.offset)
        case CASEOBJECT => unreachable
        case TRAIT      => Token.KwTrait(content, dialect, curr.offset)
        case EXTENDS    => Token.KwExtends(content, dialect, curr.offset)
        case WITH       => Token.KwWith(content, dialect, curr.offset)
        case TYPE       => Token.KwType(content, dialect, curr.offset)
        case FORSOME    => Token.KwForsome(content, dialect, curr.offset)
        case DEF        => Token.KwDef(content, dialect, curr.offset)
        case VAL        => Token.KwVal(content, dialect, curr.offset)
        case VAR        => Token.KwVar(content, dialect, curr.offset)

        case IF      => Token.KwIf(content, dialect, curr.offset)
        case THEN    => unreachable
        case ELSE    => Token.KwElse(content, dialect, curr.offset)
        case WHILE   => Token.KwWhile(content, dialect, curr.offset)
        case DO      => Token.KwDo(content, dialect, curr.offset)
        case FOR     => Token.KwFor(content, dialect, curr.offset)
        case YIELD   => Token.KwYield(content, dialect, curr.offset)
        case THROW   => Token.KwThrow(content, dialect, curr.offset)
        case TRY     => Token.KwTry(content, dialect, curr.offset)
        case CATCH   => Token.KwCatch(content, dialect, curr.offset)
        case FINALLY => Token.KwFinally(content, dialect, curr.offset)
        case CASE    => Token.KwCase(content, dialect, curr.offset)
        case RETURN  => Token.KwReturn(content, dialect, curr.offset)
        case MATCH   => Token.KwMatch(content, dialect, curr.offset)

        case LPAREN   => Token.LeftParen(content, dialect, curr.offset)
        case RPAREN   => Token.RightParen(content, dialect, curr.offset)
        case LBRACKET => Token.LeftBracket(content, dialect, curr.offset)
        case RBRACKET => Token.RightBracket(content, dialect, curr.offset)
        case LBRACE   => Token.LeftBrace(content, dialect, curr.offset)
        case RBRACE   => Token.RightBrace(content, dialect, curr.offset)

        case COMMA     => Token.Comma(content, dialect, curr.offset)
        case SEMI      => Token.Semicolon(content, dialect, curr.offset)
        case DOT       => Token.Dot(content, dialect, curr.offset)
        case COLON     => Token.Colon(content, dialect, curr.offset)
        case EQUALS    => Token.Equals(content, dialect, curr.offset)
        case AT        => Token.At(content, dialect, curr.offset)
        case HASH      => Token.Hash(content, dialect, curr.offset)
        case USCORE    => Token.Underscore(content, dialect, curr.offset)
        case ARROW     => Token.RightArrow(content, dialect, curr.offset, curr.endOffset + 1)
        case LARROW    => Token.LeftArrow(content, dialect, curr.offset, curr.endOffset + 1)
        case SUBTYPE   => Token.Subtype(content, dialect, curr.offset)
        case SUPERTYPE => Token.Supertype(content, dialect, curr.offset)
        case VIEWBOUND => Token.Viewbound(content, dialect, curr.offset)

        case WHITESPACE =>
          if (curr.strVal == " ") Token.Space(content, dialect, curr.offset)
          else if (curr.strVal == "\t") Token.Tab(content, dialect, curr.offset)
          else if (curr.strVal == "\r") Token.CR(content, dialect, curr.offset)
          else if (curr.strVal == "\n") Token.LF(content, dialect, curr.offset)
          else if (curr.strVal == "\f") Token.FF(content, dialect, curr.offset)
          else unreachable(debug(curr.strVal))

        case COMMENT   => Token.Comment(content, dialect, curr.offset, curr.endOffset + 1)

        case ELLIPSIS  => Token.Ellipsis(content, dialect, curr.offset, curr.endOffset + 1, curr.base)

        case EOF       => Token.EOF(content, dialect)

        case EMPTY    => unreachable
        case UNDEF    => unreachable
        case ERROR    => unreachable
      }
    }
    val scanner = new LegacyScanner(content)
    val buf = scanner.reader.buf

    var legacyTokenBuf = mutable.ArrayBuilder.make[LegacyTokenData]()
    var xmlLiteralBuf = new mutable.ListBuffer[String]
    scanner.foreach(curr => {
      val currCopy = new LegacyTokenData{}.copyFrom(curr)
      if (currCopy.token == XMLSTART) {
        // TODO: replace this with honest XML support via #356
        import fastparse.core.Parsed
        import scalaparse.Scala.XmlExpr
        val start = currCopy.offset
        val result = XmlExpr.parse(new String(content.chars), index = start)
        result match {
          case Parsed.Success(_, end) =>
            val length = end - start
            xmlLiteralBuf += new String(content.chars, start, length)
            scanner.reader.charOffset = scanner.curr.offset + length
            scanner.reader.nextChar()
          case Parsed.Failure(_, _, _) =>
            scanner.reporter.syntaxError("malformed xml literal", at = currCopy.offset)
        }
      }
      if (currCopy.token == EOF) {
        // NOTE: sometimes EOF's offset is `buf.length - 1`, and that might mess things up
        currCopy.offset = buf.length
      }
      legacyTokenBuf += currCopy
    })
    val legacyTokens = legacyTokenBuf.result

    var tokens = new immutable.VectorBuilder[Token]
    tokens += Token.BOF(content, dialect)

    def loop(startingFrom: Int, braceBalance: Int = 0, returnWhenBraceBalanceHitsZero: Boolean = false): Int = {
      var legacyIndex = startingFrom
      def prev = legacyTokens(legacyIndex - 1)
      def curr = legacyTokens(legacyIndex)
      def emitToken() = tokens += legacyTokenToToken(curr)
      def nextToken() = legacyIndex += 1
      if (legacyIndex >= legacyTokens.length) return legacyIndex

      emitToken()
      nextToken()

      // NOTE: need to track this in order to correctly emit SpliceEnd tokens after splices end
      var braceBalance1 = braceBalance
      if (prev.token == LBRACE) braceBalance1 += 1
      if (prev.token == RBRACE) braceBalance1 -= 1
      if (braceBalance1 == 0 && returnWhenBraceBalanceHitsZero) return legacyIndex

      if (prev.token == INTERPOLATIONID) {
        // NOTE: funnily enough, messing with interpolation tokens is what I've been doing roughly 3 years ago, on New Year's Eve of 2011/2012
        // I vividly remember spending 2 or 3 days making scanner emit detailed tokens for string interpolations, and that was tedious.
        // Now we need to do the same for our new token stream, but I don't really feel like going through the pain again.
        // Therefore, I'm giving up the 1-to-1 legacy-to-new token correspondence and will be trying to reverse engineer sane tokens here rather than in scanner.
        var startEnd = prev.endOffset + 1
        while (startEnd < buf.length && buf(startEnd) == '\"') startEnd += 1
        val numStartQuotes = startEnd - prev.endOffset - 1
        val numQuotes = if (numStartQuotes <= 2) 1 else 3
        def emitStart(offset: Offset) = tokens += Token.Interpolation.Start(content, dialect, offset, offset + numQuotes)
        def emitEnd(offset: Offset) = tokens += Token.Interpolation.End(content, dialect, offset, offset + numQuotes)
        def emitContents(): Unit = {
          require(curr.token == STRINGPART || curr.token == STRINGLIT)
          if (curr.token == STRINGPART) {
            tokens += Token.Interpolation.Part(content, dialect, curr.offset, curr.endOffset + 1, curr.strVal)
            require(buf(curr.endOffset + 1) == '$')
            val dollarOffset = curr.endOffset + 1
            def emitSpliceStart(offset: Offset) = tokens += Token.Interpolation.SpliceStart(content, dialect, offset, offset + 1)
            def emitSpliceEnd(offset: Offset) = tokens += Token.Interpolation.SpliceEnd(content, dialect, offset, offset)
            def requireExpectedToken(expected: LegacyToken) = { require(curr.token == expected) }
            def emitExpectedToken(expected: LegacyToken) = { require(curr.token == expected); emitToken() }
            if (buf(dollarOffset + 1) == '{') {
              emitSpliceStart(dollarOffset)
              nextToken()
              legacyIndex = loop(legacyIndex, braceBalance = 0, returnWhenBraceBalanceHitsZero = true)
              emitSpliceEnd(curr.offset)
              emitContents()
            } else if (buf(dollarOffset + 1) == '_') {
              emitSpliceStart(dollarOffset)
              nextToken()
              emitExpectedToken(USCORE)
              nextToken()
              emitSpliceEnd(curr.offset)
              emitContents()
            } else {
              emitSpliceStart(dollarOffset)
              nextToken()
              require(curr.token == IDENTIFIER || curr.token == THIS)
              emitToken()
              nextToken()
              emitSpliceEnd(curr.offset)
              emitContents()
            }
          } else {
            curr.endOffset -= numQuotes
            tokens += Token.Interpolation.Part(content, dialect, curr.offset, curr.endOffset + 1, curr.strVal)
            require(buf(curr.endOffset + 1) == '\"')
            nextToken()
          }
        }
        // NOTE: before emitStart, curr is the first token that follows INTERPOLATIONID
        // i.e. STRINGLIT (if the interpolation is empty) or STRINGPART (if it's not)
        // NOTE: before emitEnd, curr is the first token that follows the concluding STRINGLIT of the interpolation
        // for example, EOF in the case of `q""` or `q"$foobar"`
        numStartQuotes match {
          case 1 => emitStart(curr.offset - 1); emitContents(); emitEnd(curr.offset - 1)
          case 2 => emitStart(curr.offset); curr.offset += 1; emitContents(); emitEnd(curr.offset - 1)
          case n if 3 <= n && n < 6 => emitStart(curr.offset - 3); emitContents(); emitEnd(curr.offset - 3)
          case 6 => emitStart(curr.offset - 3); emitContents(); emitEnd(curr.offset - 3)
        }
      }

      if (prev.token == XMLSTART) {
        val raw = xmlLiteralBuf.remove(0)
        tokens += Token.Xml.Part(content, dialect, prev.offset, curr.offset, raw)
        tokens += Token.Xml.End(content, dialect, curr.offset, curr.offset)
      }

      loop(legacyIndex, braceBalance1, returnWhenBraceBalanceHitsZero)
    }

    loop(startingFrom = 0)
    Tokens.Tokenized(content, dialect, tokens.result: _*)
  }
}

object ScalametaTokenizer {
  def toTokenize: Tokenize = new Tokenize {
    def apply(content: Content)(implicit dialect: Dialect): Tokenized = {
      try {
        val tokenizer = new ScalametaTokenizer(content)(dialect)
        Tokenized.Success(tokenizer.tokenize())
      } catch {
        case details @ TokenizeException(pos, message) => Tokenized.Error(pos, message, details)
      }
    }
  }
}
