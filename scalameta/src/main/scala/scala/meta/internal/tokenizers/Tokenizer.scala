package scala.meta
package internal
package tokenizers

import scala.collection.{immutable, mutable}
import org.scalameta.unreachable
import org.scalameta.invariants._
import Chars.{CR, LF, FF}
import LegacyToken._

private[meta] object tokenize {
  def apply(input: Input)(implicit dialect: Dialect): Vector[Token] = {
    def legacyTokenToToken(curr: LegacyTokenData, index: Int): Token = {
      (curr.token: @scala.annotation.switch) match {
        case CHARLIT         => Token.Literal.Char(input, dialect, index, curr.offset, curr.endOffset, curr.charVal)
        case INTLIT          => Token.Literal.Int(input, dialect, index, curr.offset, curr.endOffset, isNegated => curr.intVal(isNegated).map(_.toInt).get)
        case LONGLIT         => Token.Literal.Long(input, dialect, index, curr.offset, curr.endOffset, isNegated => curr.intVal(isNegated).get)
        case FLOATLIT        => Token.Literal.Float(input, dialect, index, curr.offset, curr.endOffset, isNegated => curr.floatVal(isNegated).map(_.toFloat).get)
        case DOUBLELIT       => Token.Literal.Double(input, dialect, index, curr.offset, curr.endOffset, isNegated => curr.floatVal(isNegated).get)
        case STRINGLIT       => Token.Literal.String(input, dialect, index, curr.offset, curr.endOffset, curr.strVal)
        case SYMBOLLIT       => Token.Literal.Symbol(input, dialect, index, curr.offset, curr.endOffset, scala.Symbol(curr.strVal))
        case NULL            => Token.Literal.`null`(input, dialect, index, curr.offset)
        case TRUE            => Token.Literal.`true`(input, dialect, index, curr.offset)
        case FALSE           => Token.Literal.`false`(input, dialect, index, curr.offset)

        case IDENTIFIER       => Token.Ident(input, dialect, index, curr.offset, curr.endOffset)
        case BACKQUOTED_IDENT => Token.Ident(input, dialect, index, curr.offset, curr.endOffset)
        case INTERPOLATIONID  => Token.Interpolation.Id(input, dialect, index, curr.offset, curr.endOffset)

        case NEW   => Token.`new`(input, dialect, index, curr.offset)
        case THIS  => Token.`this`(input, dialect, index, curr.offset)
        case SUPER => Token.`super`(input, dialect, index, curr.offset)

        case IMPLICIT  => Token.`implicit`(input, dialect, index, curr.offset)
        case OVERRIDE  => Token.`override`(input, dialect, index, curr.offset)
        case PROTECTED => Token.`protected`(input, dialect, index, curr.offset)
        case PRIVATE   => Token.`private`(input, dialect, index, curr.offset)
        case ABSTRACT  => Token.`abstract`(input, dialect, index, curr.offset)
        case FINAL     => Token.`final`(input, dialect, index, curr.offset)
        case SEALED    => Token.`sealed`(input, dialect, index, curr.offset)
        case LAZY      => Token.`lazy`(input, dialect, index, curr.offset)
        case MACRO     => Token.`macro`(input, dialect, index, curr.offset)

        case PACKAGE    => Token.`package `(input, dialect, index, curr.offset)
        case IMPORT     => Token.`import`(input, dialect, index, curr.offset)
        case CLASS      => Token.`class `(input, dialect, index, curr.offset)
        case CASECLASS  => unreachable
        case OBJECT     => Token.`object`(input, dialect, index, curr.offset)
        case CASEOBJECT => unreachable
        case TRAIT      => Token.`trait`(input, dialect, index, curr.offset)
        case EXTENDS    => Token.`extends`(input, dialect, index, curr.offset)
        case WITH       => Token.`with`(input, dialect, index, curr.offset)
        case TYPE       => Token.`type`(input, dialect, index, curr.offset)
        case FORSOME    => Token.`forSome`(input, dialect, index, curr.offset)
        case DEF        => Token.`def`(input, dialect, index, curr.offset)
        case VAL        => Token.`val`(input, dialect, index, curr.offset)
        case VAR        => Token.`var`(input, dialect, index, curr.offset)

        case IF      => Token.`if`(input, dialect, index, curr.offset)
        case THEN    => unreachable
        case ELSE    => Token.`else`(input, dialect, index, curr.offset)
        case WHILE   => Token.`while`(input, dialect, index, curr.offset)
        case DO      => Token.`do`(input, dialect, index, curr.offset)
        case FOR     => Token.`for`(input, dialect, index, curr.offset)
        case YIELD   => Token.`yield`(input, dialect, index, curr.offset)
        case THROW   => Token.`throw`(input, dialect, index, curr.offset)
        case TRY     => Token.`try`(input, dialect, index, curr.offset)
        case CATCH   => Token.`catch`(input, dialect, index, curr.offset)
        case FINALLY => Token.`finally`(input, dialect, index, curr.offset)
        case CASE    => Token.`case`(input, dialect, index, curr.offset)
        case RETURN  => Token.`return`(input, dialect, index, curr.offset)
        case MATCH   => Token.`match`(input, dialect, index, curr.offset)

        case LPAREN   => Token.`(`(input, dialect, index, curr.offset)
        case RPAREN   => Token.`)`(input, dialect, index, curr.offset)
        case LBRACKET => Token.`[`(input, dialect, index, curr.offset)
        case RBRACKET => Token.`]`(input, dialect, index, curr.offset)
        case LBRACE   => Token.`{`(input, dialect, index, curr.offset)
        case RBRACE   => Token.`}`(input, dialect, index, curr.offset)

        case COMMA     => Token.`,`(input, dialect, index, curr.offset)
        case SEMI      => Token.`;`(input, dialect, index, curr.offset)
        case DOT       => Token.`.`(input, dialect, index, curr.offset)
        case COLON     => Token.`:`(input, dialect, index, curr.offset)
        case EQUALS    => Token.`=`(input, dialect, index, curr.offset)
        case AT        => Token.`@`(input, dialect, index, curr.offset)
        case HASH      => Token.`#`(input, dialect, index, curr.offset)
        case USCORE    => Token.`_ `(input, dialect, index, curr.offset)
        case ARROW     => Token.`=>`(input, dialect, index, curr.offset, curr.endOffset)
        case LARROW    => Token.`<-`(input, dialect, index, curr.offset, curr.endOffset)
        case SUBTYPE   => Token.`<:`(input, dialect, index, curr.offset)
        case SUPERTYPE => Token.`>:`(input, dialect, index, curr.offset)
        case VIEWBOUND => Token.`<%`(input, dialect, index, curr.offset)

        case WHITESPACE =>
          if (curr.strVal == " ") Token.` `(input, dialect, index, curr.offset)
          else if (curr.strVal == "\t") Token.`\t`(input, dialect, index, curr.offset)
          else if (curr.strVal == "\r") Token.`\r`(input, dialect, index, curr.offset)
          else if (curr.strVal == "\n") Token.`\n`(input, dialect, index, curr.offset)
          else if (curr.strVal == "\f") Token.`\f`(input, dialect, index, curr.offset)
          else unreachable(debug(curr.strVal))

        case COMMENT   => Token.Comment(input, dialect, index, curr.offset, curr.endOffset)

        case ELLIPSIS  => Token.Ellipsis(input, dialect, index, curr.offset, curr.endOffset, curr.base)

        case EOF       => Token.EOF(input, dialect, index)
        case XMLSTART  => Token.XMLStart(input, dialect, index, curr.offset, curr.endOffset)

        case EMPTY    => unreachable
        case UNDEF    => unreachable
        case ERROR    => unreachable
      }
    }
    val scanner = new LegacyScanner(input)
    val buf = scanner.reader.buf

    var legacyTokenBuf = new mutable.UnrolledBuffer[LegacyTokenData]
    scanner.foreach(curr => {
      // TODO: reinstate error handling
      // try legacyTokenBuf += curr
      // catch { case e: Exception => scanner.report.error(e.getMessage) }
      // val tokenGetters = Tokens.getClass.getMethods.filter(_.getParameterTypes().length == 0)
      // println(tokenGetters.find(m => m.invoke(Tokens) == curr).get.getName)
      val currCopy = new LegacyTokenData{}.copyFrom(curr)
      if (currCopy.token == EOF) currCopy.offset = buf.length // NOTE: sometimes EOF's offset is `buf.length - 1`, and that might mess things up
      legacyTokenBuf += currCopy
    })
    val legacyTokens = legacyTokenBuf.toVector

    var _index = -1
    def nextIndex() = { _index += 1; _index }
    var tokens = new mutable.UnrolledBuffer[Token]
    tokens += Token.BOF(input, dialect, nextIndex())

    def loop(startingFrom: Int, braceBalance: Int = 0, returnWhenBraceBalanceHitsZero: Boolean = false): Int = {
      var legacyIndex = startingFrom
      def prev = legacyTokens(legacyIndex - 1)
      def curr = legacyTokens(legacyIndex)
      def emitToken() = tokens += legacyTokenToToken(curr, nextIndex())
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
        def emitStart(offset: Offset) = tokens += Token.Interpolation.Start(input, dialect, nextIndex(), offset, offset + numQuotes - 1)
        def emitEnd(offset: Offset) = tokens += Token.Interpolation.End(input, dialect, nextIndex(), offset, offset + numQuotes - 1)
        def emitContents(): Unit = {
          require(curr.token == STRINGPART || curr.token == STRINGLIT)
          if (curr.token == STRINGPART) {
            tokens += Token.Interpolation.Part(input, dialect, nextIndex(), curr.offset, curr.endOffset)
            require(buf(curr.endOffset + 1) == '$')
            val dollarOffset = curr.endOffset + 1
            def emitSpliceStart(offset: Offset) = tokens += Token.Interpolation.SpliceStart(input, dialect, nextIndex(), offset)
            def emitSpliceEnd(offset: Offset) = tokens += Token.Interpolation.SpliceEnd(input, dialect, nextIndex(), offset)
            def requireExpectedToken(expected: LegacyToken) = { require(curr.token == expected) }
            def emitExpectedToken(expected: LegacyToken) = { require(curr.token == expected); emitToken() }
            if (buf(dollarOffset + 1) == '{') {
              emitSpliceStart(dollarOffset)
              nextToken()
              legacyIndex = loop(legacyIndex, braceBalance = 0, returnWhenBraceBalanceHitsZero = true)
              emitSpliceEnd(curr.offset - 1)
              emitContents()
            } else if (buf(dollarOffset + 1) == '_') {
              emitSpliceStart(dollarOffset)
              nextToken()
              emitExpectedToken(USCORE)
              nextToken()
              emitSpliceEnd(curr.offset - 1)
              emitContents()
            } else {
              emitSpliceStart(dollarOffset)
              nextToken()
              require(curr.token == IDENTIFIER || curr.token == THIS)
              emitToken()
              nextToken()
              emitSpliceEnd(curr.offset - 1)
              emitContents()
            }
          } else {
            curr.endOffset -= numQuotes
            tokens += Token.Interpolation.Part(input, dialect, nextIndex(), curr.offset, curr.endOffset)
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

      loop(legacyIndex, braceBalance1, returnWhenBraceBalanceHitsZero)
    }

    loop(startingFrom = 0)
    tokens.toVector
  }
}