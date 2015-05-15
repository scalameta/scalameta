package scala.meta
package internal
package tokenizers

import scala.collection.{immutable, mutable}
import org.scalameta.unreachable
import org.scalameta.invariants._
import Chars.{CR, LF, FF}
import LegacyToken._

private[meta] object tokenize {
  def apply(content: Content)(implicit dialect: Dialect): Tokens = {
    def legacyTokenToToken(curr: LegacyTokenData): Token = {
      (curr.token: @scala.annotation.switch) match {
        case IDENTIFIER       => Token.Ident(content, dialect, curr.offset, curr.endOffset + 1)
        case BACKQUOTED_IDENT => Token.Ident(content, dialect, curr.offset, curr.endOffset + 1)

        case CHARLIT         => Token.Literal.Char(content, dialect, curr.offset, curr.endOffset + 1, curr.charVal)
        case INTLIT          => Token.Literal.Int(content, dialect, curr.offset, curr.endOffset + 1, isNegated => curr.intVal(isNegated).map(_.toInt).get)
        case LONGLIT         => Token.Literal.Long(content, dialect, curr.offset, curr.endOffset + 1, isNegated => curr.intVal(isNegated).get)
        case FLOATLIT        => Token.Literal.Float(content, dialect, curr.offset, curr.endOffset + 1, isNegated => curr.floatVal(isNegated).map(_.toFloat).get)
        case DOUBLELIT       => Token.Literal.Double(content, dialect, curr.offset, curr.endOffset + 1, isNegated => curr.floatVal(isNegated).get)
        case STRINGLIT       => Token.Literal.String(content, dialect, curr.offset, curr.endOffset + 1, curr.strVal)
        case SYMBOLLIT       => Token.Literal.Symbol(content, dialect, curr.offset, curr.endOffset + 1, scala.Symbol(curr.strVal))
        case NULL            => Token.Literal.`null`(content, dialect, curr.offset)
        case TRUE            => Token.Literal.`true`(content, dialect, curr.offset)
        case FALSE           => Token.Literal.`false`(content, dialect, curr.offset)

        case INTERPOLATIONID => Token.Interpolation.Id(content, dialect, curr.offset, curr.endOffset + 1)
        case XMLSTART        => Token.Xml.Start(content, dialect, curr.offset)

        case NEW   => Token.`new`(content, dialect, curr.offset)
        case THIS  => Token.`this`(content, dialect, curr.offset)
        case SUPER => Token.`super`(content, dialect, curr.offset)

        case IMPLICIT  => Token.`implicit`(content, dialect, curr.offset)
        case OVERRIDE  => Token.`override`(content, dialect, curr.offset)
        case PROTECTED => Token.`protected`(content, dialect, curr.offset)
        case PRIVATE   => Token.`private`(content, dialect, curr.offset)
        case ABSTRACT  => Token.`abstract`(content, dialect, curr.offset)
        case FINAL     => Token.`final`(content, dialect, curr.offset)
        case SEALED    => Token.`sealed`(content, dialect, curr.offset)
        case LAZY      => Token.`lazy`(content, dialect, curr.offset)
        case MACRO     => Token.`macro`(content, dialect, curr.offset)

        case PACKAGE    => Token.`package `(content, dialect, curr.offset)
        case IMPORT     => Token.`import`(content, dialect, curr.offset)
        case CLASS      => Token.`class `(content, dialect, curr.offset)
        case CASECLASS  => unreachable
        case OBJECT     => Token.`object`(content, dialect, curr.offset)
        case CASEOBJECT => unreachable
        case TRAIT      => Token.`trait`(content, dialect, curr.offset)
        case EXTENDS    => Token.`extends`(content, dialect, curr.offset)
        case WITH       => Token.`with`(content, dialect, curr.offset)
        case TYPE       => Token.`type`(content, dialect, curr.offset)
        case FORSOME    => Token.`forSome`(content, dialect, curr.offset)
        case DEF        => Token.`def`(content, dialect, curr.offset)
        case VAL        => Token.`val`(content, dialect, curr.offset)
        case VAR        => Token.`var`(content, dialect, curr.offset)

        case IF      => Token.`if`(content, dialect, curr.offset)
        case THEN    => unreachable
        case ELSE    => Token.`else`(content, dialect, curr.offset)
        case WHILE   => Token.`while`(content, dialect, curr.offset)
        case DO      => Token.`do`(content, dialect, curr.offset)
        case FOR     => Token.`for`(content, dialect, curr.offset)
        case YIELD   => Token.`yield`(content, dialect, curr.offset)
        case THROW   => Token.`throw`(content, dialect, curr.offset)
        case TRY     => Token.`try`(content, dialect, curr.offset)
        case CATCH   => Token.`catch`(content, dialect, curr.offset)
        case FINALLY => Token.`finally`(content, dialect, curr.offset)
        case CASE    => Token.`case`(content, dialect, curr.offset)
        case RETURN  => Token.`return`(content, dialect, curr.offset)
        case MATCH   => Token.`match`(content, dialect, curr.offset)

        case LPAREN   => Token.`(`(content, dialect, curr.offset)
        case RPAREN   => Token.`)`(content, dialect, curr.offset)
        case LBRACKET => Token.`[`(content, dialect, curr.offset)
        case RBRACKET => Token.`]`(content, dialect, curr.offset)
        case LBRACE   => Token.`{`(content, dialect, curr.offset)
        case RBRACE   => Token.`}`(content, dialect, curr.offset)

        case COMMA     => Token.`,`(content, dialect, curr.offset)
        case SEMI      => Token.`;`(content, dialect, curr.offset)
        case DOT       => Token.`.`(content, dialect, curr.offset)
        case COLON     => Token.`:`(content, dialect, curr.offset)
        case EQUALS    => Token.`=`(content, dialect, curr.offset)
        case AT        => Token.`@`(content, dialect, curr.offset)
        case HASH      => Token.`#`(content, dialect, curr.offset)
        case USCORE    => Token.`_ `(content, dialect, curr.offset)
        case ARROW     => Token.`=>`(content, dialect, curr.offset, curr.endOffset + 1)
        case LARROW    => Token.`<-`(content, dialect, curr.offset, curr.endOffset + 1)
        case SUBTYPE   => Token.`<:`(content, dialect, curr.offset)
        case SUPERTYPE => Token.`>:`(content, dialect, curr.offset)
        case VIEWBOUND => Token.`<%`(content, dialect, curr.offset)

        case WHITESPACE =>
          if (curr.strVal == " ") Token.` `(content, dialect, curr.offset)
          else if (curr.strVal == "\t") Token.`\t`(content, dialect, curr.offset)
          else if (curr.strVal == "\r") Token.`\r`(content, dialect, curr.offset)
          else if (curr.strVal == "\n") Token.`\n`(content, dialect, curr.offset)
          else if (curr.strVal == "\f") Token.`\f`(content, dialect, curr.offset)
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
    lazy val xmlLiteralGlobal = {
      import scala.tools.reflect._
      import scala.tools.nsc._
      import scala.tools.nsc.settings._
      import scala.tools.nsc.reporters._
      val settings = new Settings(msg => sys.error(s"fatal error parsing xml literal: $msg"))
      settings.Yrangepos.value = true
      val reporter = new StoreReporter()
      val result = new ReflectGlobal(settings, reporter, classOf[List[_]].getClassLoader)
      result.globalPhase = new result.Run().parserPhase
      result
    }
    scanner.foreach(curr => {
      val currCopy = new LegacyTokenData{}.copyFrom(curr)
      if (currCopy.token == XMLSTART) {
        val slice = content.chars.drop(Math.max(currCopy.offset, 0))
        def probe(): Either[String, Int] = {
          import scala.reflect.io._
          import scala.reflect.internal.util._
          import xmlLiteralGlobal._
          val abstractFile = new VirtualFile("<scalameta-tokenizing-xmlliteral>")
          val sourceFile = new BatchSourceFile(abstractFile, slice)
          val unit = new CompilationUnit(sourceFile)
          def tryParse(action: syntaxAnalyzer.Parser => Tree): Either[(Int, String), Int] = {
            reporter.reset()
            val parser = newUnitParser(unit)
            val result = action(parser)
            if (reporter.hasErrors) {
              import scala.language.existentials
              val infos = reporter.asInstanceOf[scala.tools.nsc.reporters.StoreReporter].infos
              val error = infos.filter(_.severity == reporter.ERROR).toList.head
              Left((error.pos.point, error.msg))
            } else {
              Right(result.pos.end)
            }
          }
          tryParse(_.xmlLiteralPattern()).fold(_ => tryParse(_.xmlLiteral()).left.map(_._2), result => Right(result))
        }
        probe() match {
          case Left(error) =>
            scanner.reporter.syntaxError("unexpected shape of xml literal", at = currCopy.offset)
          case Right(length) =>
            xmlLiteralBuf += new String(slice.take(length))
            scanner.reader.charOffset = scanner.curr.offset + length + 1
            if (scanner.reader.charOffset >= content.chars.length) scanner.next.token = EOF
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
            tokens += Token.Interpolation.Part(content, dialect, curr.offset, curr.endOffset + 1)
            require(buf(curr.endOffset + 1) == '$')
            val dollarOffset = curr.endOffset + 1
            def emitSpliceStart(offset: Offset) = tokens += Token.Interpolation.SpliceStart(content, dialect, offset)
            def emitSpliceEnd(offset: Offset) = tokens += Token.Interpolation.SpliceEnd(content, dialect, offset)
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
            tokens += Token.Interpolation.Part(content, dialect, curr.offset, curr.endOffset + 1)
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
        tokens += Token.Xml.Part(content, dialect, prev.offset, curr.offset)
        tokens += Token.Xml.End(content, dialect, curr.offset - 1)
      }

      loop(legacyIndex, braceBalance1, returnWhenBraceBalanceHitsZero)
    }

    loop(startingFrom = 0)
    Tokens.Tokenized(content, dialect, tokens.result: _*)
  }
}