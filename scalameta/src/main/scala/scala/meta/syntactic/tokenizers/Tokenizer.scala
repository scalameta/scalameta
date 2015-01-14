package scala.meta
package syntactic
package tokenizers

import scala.collection.{immutable, mutable}
import org.scalameta.unreachable
import org.scalameta.invariants._
import Chars.{CR, LF, FF}
import LegacyToken._

object tokenize {
  def apply(origin: Origin)(implicit dialect: Dialect): Vector[Token] = {
    def legacyTokenToToken(curr: LegacyTokenData): Token = {
      (curr.token: @scala.annotation.switch) match {
        case CHARLIT         => Token.Literal.Char(origin, curr.offset, curr.endOffset, curr.charVal)
        case INTLIT          => Token.Literal.Int(origin, curr.offset, curr.endOffset, isNegated => curr.intVal(isNegated).map(_.toInt).get)
        case LONGLIT         => Token.Literal.Long(origin, curr.offset, curr.endOffset, isNegated => curr.intVal(isNegated).get)
        case FLOATLIT        => Token.Literal.Float(origin, curr.offset, curr.endOffset, isNegated => curr.floatVal(isNegated).map(_.toFloat).get)
        case DOUBLELIT       => Token.Literal.Double(origin, curr.offset, curr.endOffset, isNegated => curr.floatVal(isNegated).get)
        case STRINGLIT       => Token.Literal.String(origin, curr.offset, curr.endOffset, curr.strVal)
        case SYMBOLLIT       => Token.Literal.Symbol(origin, curr.offset, curr.endOffset, scala.Symbol(curr.strVal))

        case IDENTIFIER       => Token.Ident(origin, curr.offset, curr.endOffset)
        case BACKQUOTED_IDENT => Token.Ident(origin, curr.offset, curr.endOffset)
        case INTERPOLATIONID  => Token.Interpolation.Id(origin, curr.offset, curr.endOffset)

        case NEW   => Token.`new`(origin, curr.offset)
        case THIS  => Token.`this`(origin, curr.offset)
        case SUPER => Token.`super`(origin, curr.offset)
        case NULL  => Token.`null`(origin, curr.offset)
        case TRUE  => Token.`true`(origin, curr.offset)
        case FALSE => Token.`false`(origin, curr.offset)

        case IMPLICIT  => Token.`implicit`(origin, curr.offset)
        case OVERRIDE  => Token.`override`(origin, curr.offset)
        case PROTECTED => Token.`protected`(origin, curr.offset)
        case PRIVATE   => Token.`private`(origin, curr.offset)
        case ABSTRACT  => Token.`abstract`(origin, curr.offset)
        case FINAL     => Token.`final`(origin, curr.offset)
        case SEALED    => Token.`sealed`(origin, curr.offset)
        case LAZY      => Token.`lazy`(origin, curr.offset)
        case MACRO     => Token.`macro`(origin, curr.offset)

        case PACKAGE    => Token.`package `(origin, curr.offset)
        case IMPORT     => Token.`import`(origin, curr.offset)
        case CLASS      => Token.`class `(origin, curr.offset)
        case CASECLASS  => unreachable
        case OBJECT     => Token.`object`(origin, curr.offset)
        case CASEOBJECT => unreachable
        case TRAIT      => Token.`trait`(origin, curr.offset)
        case EXTENDS    => Token.`extends`(origin, curr.offset)
        case WITH       => Token.`with`(origin, curr.offset)
        case TYPE       => Token.`type`(origin, curr.offset)
        case FORSOME    => Token.`forSome`(origin, curr.offset)
        case DEF        => Token.`def`(origin, curr.offset)
        case VAL        => Token.`val`(origin, curr.offset)
        case VAR        => Token.`var`(origin, curr.offset)

        case IF      => Token.`if`(origin, curr.offset)
        case THEN    => unreachable
        case ELSE    => Token.`else`(origin, curr.offset)
        case WHILE   => Token.`while`(origin, curr.offset)
        case DO      => Token.`do`(origin, curr.offset)
        case FOR     => Token.`for`(origin, curr.offset)
        case YIELD   => Token.`yield`(origin, curr.offset)
        case THROW   => Token.`throw`(origin, curr.offset)
        case TRY     => Token.`try`(origin, curr.offset)
        case CATCH   => Token.`catch`(origin, curr.offset)
        case FINALLY => Token.`finally`(origin, curr.offset)
        case CASE    => Token.`case`(origin, curr.offset)
        case RETURN  => Token.`return`(origin, curr.offset)
        case MATCH   => Token.`match`(origin, curr.offset)

        case LPAREN   => Token.`(`(origin, curr.offset)
        case RPAREN   => Token.`)`(origin, curr.offset)
        case LBRACKET => Token.`[`(origin, curr.offset)
        case RBRACKET => Token.`]`(origin, curr.offset)
        case LBRACE   => Token.`{`(origin, curr.offset)
        case RBRACE   => Token.`}`(origin, curr.offset)

        case COMMA     => Token.`,`(origin, curr.offset)
        case SEMI      => Token.`;`(origin, curr.offset)
        case DOT       => Token.`.`(origin, curr.offset)
        case COLON     => Token.`:`(origin, curr.offset)
        case EQUALS    => Token.`=`(origin, curr.offset)
        case AT        => Token.`@`(origin, curr.offset)
        case HASH      => Token.`#`(origin, curr.offset)
        case USCORE    => Token.`_ `(origin, curr.offset)
        case ARROW     => Token.`=>`(origin, curr.offset, curr.endOffset)
        case LARROW    => Token.`<-`(origin, curr.offset, curr.endOffset)
        case SUBTYPE   => Token.`<:`(origin, curr.offset)
        case SUPERTYPE => Token.`>:`(origin, curr.offset)
        case VIEWBOUND => Token.`<%`(origin, curr.offset)

        case WHITESPACE =>
          if (curr.strVal == " ") Token.` `(origin, curr.offset)
          else if (curr.strVal == "\t") Token.`\t`(origin, curr.offset)
          else if (curr.strVal == "\r") Token.`\r`(origin, curr.offset)
          else if (curr.strVal == "\n") Token.`\n`(origin, curr.offset)
          else if (curr.strVal == "\f") Token.`\f`(origin, curr.offset)
          else unreachable

        case COMMENT   => Token.Comment(origin, curr.offset, curr.endOffset)

        case EOF       => Token.EOF(origin)
        case XMLSTART  => Token.XMLStart(origin, curr.offset, curr.endOffset)

        case EMPTY    => unreachable
        case UNDEF    => unreachable
        case ERROR    => unreachable
      }
    }
    val scanner = new LegacyScanner(origin)
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

    var tokens = new mutable.UnrolledBuffer[Token]
    def loop(startingFrom: Int, braceBalance: Int = 0, returnWhenBraceBalanceHitsZero: Boolean = false): Int = {
      var i = startingFrom
      def prev = legacyTokens(i - 1)
      def curr = legacyTokens(i)
      def emitToken() = tokens += legacyTokenToToken(curr)
      def nextToken() = i += 1
      if (i >= legacyTokens.length) return i

      emitToken()
      nextToken()

      // NOTE: need to track this in order to correctly emit SpliceEnd tokens after splices end
      var braceBalance1 = braceBalance
      if (prev.token == LBRACE) braceBalance1 += 1
      if (prev.token == RBRACE) braceBalance1 -= 1
      if (braceBalance1 == 0 && returnWhenBraceBalanceHitsZero) return i

      if (prev.token == INTERPOLATIONID) {
        // NOTE: funnily enough, messing with interpolation tokens is what I've been doing roughly 3 years ago, on New Year's Eve of 2011/2012
        // I vividly remember spending 2 or 3 days making scanner emit detailed tokens for string interpolations, and that was tedious.
        // Now we need to do the same for our new token stream, but I don't really feel like going through the pain again.
        // Therefore, I'm giving up the 1-to-1 legacy-to-new token correspondence and will be trying to reverse engineer sane tokens here rather than in scanner.
        var startEnd = prev.endOffset + 1
        while (startEnd < buf.length && buf(startEnd) == '\"') startEnd += 1
        val numStartQuotes = startEnd - prev.endOffset - 1
        val numQuotes = if (numStartQuotes <= 2) 1 else 3
        def emitStart(offset: Offset) = tokens += Token.Interpolation.Start(origin, offset, offset + numQuotes - 1)
        def emitEnd(offset: Offset) = tokens += Token.Interpolation.End(origin, offset, offset + numQuotes - 1)
        def emitContents(): Unit = {
          require(curr.token == STRINGPART || curr.token == STRINGLIT)
          if (curr.token == STRINGPART) {
            tokens += Token.Interpolation.Part(origin, curr.offset, curr.endOffset)
            require(buf(curr.endOffset + 1) == '$')
            val dollarOffset = curr.endOffset + 1
            def emitSpliceStart(offset: Offset) = tokens += Token.Interpolation.SpliceStart(origin, offset)
            def emitSpliceEnd(offset: Offset) = tokens += Token.Interpolation.SpliceEnd(origin, offset)
            def requireExpectedToken(expected: LegacyToken) = { require(curr.token == expected) }
            def emitExpectedToken(expected: LegacyToken) = { require(curr.token == expected); emitToken() }
            if (buf(dollarOffset + 1) == '{') {
              emitSpliceStart(dollarOffset)
              nextToken()
              i = loop(i, braceBalance = 0, returnWhenBraceBalanceHitsZero = true)
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
            tokens += Token.Interpolation.Part(origin, curr.offset, curr.endOffset)
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

      loop(i, braceBalance1, returnWhenBraceBalanceHitsZero)
    }

    loop(startingFrom = 0)
    tokens.toVector
  }
}