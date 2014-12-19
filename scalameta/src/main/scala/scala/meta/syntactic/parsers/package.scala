package scala.meta.syntactic

import scala.collection.{immutable, mutable}
import scala.annotation.switch
import org.scalameta.convert._
import org.scalameta.unreachable
import parsers.Tokens._
import scala.meta._
import scala.meta.syntactic.parsers.Chars.{CR, LF, FF}
import org.scalameta.invariants._

package parsers {
  // TODO: when I grow up I want to become a monad, just like my daddy
  trait Report {
    def currentOffset: Offset
    def warning(msg: String, at: Offset = currentOffset): Unit                 = ()
    def deprecationWarning(msg: String, at: Offset = currentOffset): Unit      = ()
    def error(msg: String, at: Offset = currentOffset): Nothing                = throw Report.Error(msg, at)
    def syntaxError(msg: String, at: Offset = currentOffset): Nothing          = throw Report.SyntaxError(msg, at)
    def incompleteInputError(msg: String, at: Offset = currentOffset): Nothing = throw Report.IncompleteInputError(msg, at)
  }
  object Report {
    def apply(current: () => Offset) = new Report { def currentOffset = current() }
    sealed abstract class Exception(msg: String) extends scala.Exception(msg)
    final case class Error(msg: String, at: Offset) extends Exception(s"error $msg at $at: $msg")
    final case class SyntaxError(msg: String, at: Offset) extends Exception(s"syntax error at $at: $msg")
    final case class IncompleteInputError(msg: String, at: Offset) extends Exception("incomplete input at $at: $msg")
  }
}

package object parsers {
  type Offset = Int

  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )

  trait Parse[T] extends Convert[Origin, T]
  object Parse {
    def apply[T](f: Origin => T): Parse[T] = new Parse[T] { def apply(origin: Origin): T = f(origin) }
    implicit val parseSource: Parse[Source] = apply(origin => new Parser(origin).parseSource())
    implicit val parseTerm: Parse[Term] = apply(origin => new Parser(origin).parseTerm())
    implicit val parseType: Parse[Type.Arg] = apply(origin => new Parser(origin).parseType())
    implicit val parsePat: Parse[Pat.Arg] = apply(origin => new Parser(origin).parsePat())
    implicit val parseStat: Parse[Stat] = apply(origin => new Parser(origin).parseStat())
    implicit val parseStats: Parse[List[Stat]] = apply(origin => new Parser(origin).parseStats())
    implicit val parseParam: Parse[Templ.Param] = apply(origin => new Parser(origin).parseParam())
    implicit val parseTparam: Parse[Type.Param] = apply(origin => new Parser(origin).parseTparam())
    implicit val parseTermArg: Parse[Term.Arg] = apply(origin => new Parser(origin).parseTermArg())
    implicit val parseEnum: Parse[Enum] = apply(origin => new Parser(origin).parseEnum())
    implicit val parseMod: Parse[Mod] = apply(origin => new Parser(origin).parseMod())
    implicit val parseTempl: Parse[Templ] = apply(origin => new Parser(origin).parseTempl())
    implicit val parseCtorRef: Parse[Ctor.Ref] = apply(origin => new Parser(origin).parseCtorRef())
    implicit val parseSelector: Parse[Selector] = apply(origin => new Parser(origin).parseSelector())
    implicit val parseCase: Parse[Case] = apply(origin => new Parser(origin).parseCase())
  }

  implicit class RichOrigin[T](val originLike: T)(implicit ev: Convert[T, Origin]) {
    private val origin: Origin = ev(originLike)
    def parse[T](implicit ev: Parse[T]): T = ev(origin)
    def tokens: immutable.IndexedSeq[Tok] = {
      implicit class TokenDataExtensions(tokenData: TokenData) {
        def code = origin.content.mkString.substring(tokenData.offset, tokenData.endOffset + 1)
      }
      def td2tok(curr: TokenData): Tok = {
        (curr.token: @switch) match {
          case CHARLIT         => Tok.Literal.Char(curr.code, curr.charVal, curr.offset)
          case INTLIT          => Tok.Literal.Int(curr.code, curr.intVal(false).map(_.toInt).get, curr.offset)
          case LONGLIT         => Tok.Literal.Long(curr.code, curr.intVal(false).get, curr.offset)
          case FLOATLIT        => Tok.Literal.Float(curr.code, curr.floatVal(false).map(_.toFloat).get, curr.offset)
          case DOUBLELIT       => Tok.Literal.Double(curr.code, curr.floatVal(false).get, curr.offset)
          case STRINGLIT       => Tok.Literal.String(curr.code, curr.strVal, curr.offset)
          case SYMBOLLIT       => Tok.Literal.Symbol(curr.code, scala.Symbol(curr.strVal), curr.offset)

          case IDENTIFIER       => Tok.Ident(curr.code, curr.name, isBackquoted = false, curr.offset)
          case BACKQUOTED_IDENT => Tok.Ident(curr.code, curr.name, isBackquoted = true, curr.offset)
          case INTERPOLATIONID  => Tok.Interpolation.Id(curr.code, curr.name, curr.offset)

          case NEW   => Tok.`new`(curr.offset)
          case THIS  => Tok.`this`(curr.offset)
          case SUPER => Tok.`super`(curr.offset)
          case NULL  => Tok.`null`(curr.offset)
          case TRUE  => Tok.`true`(curr.offset)
          case FALSE => Tok.`false`(curr.offset)

          case IMPLICIT  => Tok.`implicit`(curr.offset)
          case OVERRIDE  => Tok.`override`(curr.offset)
          case PROTECTED => Tok.`protected`(curr.offset)
          case PRIVATE   => Tok.`private`(curr.offset)
          case ABSTRACT  => Tok.`abstract`(curr.offset)
          case FINAL     => Tok.`final`(curr.offset)
          case SEALED    => Tok.`sealed`(curr.offset)
          case LAZY      => Tok.`lazy`(curr.offset)
          case MACRO     => Tok.`macro`(curr.offset)

          case PACKAGE    => Tok.`package `(curr.offset)
          case IMPORT     => Tok.`import`(curr.offset)
          case CLASS      => Tok.`class `(curr.offset)
          case CASECLASS  => unreachable
          case OBJECT     => Tok.`object`(curr.offset)
          case CASEOBJECT => unreachable
          case TRAIT      => Tok.`trait`(curr.offset)
          case EXTENDS    => Tok.`extends`(curr.offset)
          case WITH       => Tok.`with`(curr.offset)
          case TYPE       => Tok.`type`(curr.offset)
          case FORSOME    => Tok.`forSome`(curr.offset)
          case DEF        => Tok.`def`(curr.offset)
          case VAL        => Tok.`val`(curr.offset)
          case VAR        => Tok.`var`(curr.offset)

          case IF      => Tok.`if`(curr.offset)
          case THEN    => unreachable
          case ELSE    => Tok.`else`(curr.offset)
          case WHILE   => Tok.`while`(curr.offset)
          case DO      => Tok.`do`(curr.offset)
          case FOR     => Tok.`for`(curr.offset)
          case YIELD   => Tok.`yield`(curr.offset)
          case THROW   => Tok.`throw`(curr.offset)
          case TRY     => Tok.`try`(curr.offset)
          case CATCH   => Tok.`catch`(curr.offset)
          case FINALLY => Tok.`finally`(curr.offset)
          case CASE    => Tok.`case`(curr.offset)
          case RETURN  => Tok.`return`(curr.offset)
          case MATCH   => Tok.`match`(curr.offset)

          case LPAREN   => Tok.`(`(curr.offset)
          case RPAREN   => Tok.`)`(curr.offset)
          case LBRACKET => Tok.`[`(curr.offset)
          case RBRACKET => Tok.`]`(curr.offset)
          case LBRACE   => Tok.`{`(curr.offset)
          case RBRACE   => Tok.`}`(curr.offset)

          case COMMA     => Tok.`,`(curr.offset)
          case SEMI      => Tok.`;`(curr.offset)
          case DOT       => Tok.`.`(curr.offset)
          case COLON     => Tok.`:`(curr.offset)
          case EQUALS    => Tok.`=`(curr.offset)
          case AT        => Tok.`@`(curr.offset)
          case HASH      => Tok.`#`(curr.offset)
          case USCORE    => Tok.`_ `(curr.offset)
          case ARROW     => Tok.`=>`(curr.offset)
          case LARROW    => Tok.`<-`(curr.offset)
          case SUBTYPE   => Tok.`<:`(curr.offset)
          case SUPERTYPE => Tok.`>:`(curr.offset)
          case VIEWBOUND => Tok.`<%`(curr.offset)

          case WHITESPACE =>
            if (curr.strVal == " ") Tok.` `(curr.offset)
            else if (curr.strVal == "\t") Tok.`\t`(curr.offset)
            else if (curr.strVal == "\r") Tok.`\r`(curr.offset)
            else if (curr.strVal == "\n") Tok.`\n`(curr.offset)
            else if (curr.strVal == "\f") Tok.`\f`(curr.offset)
            else unreachable

          case EOF       => Tok.EOF(curr.offset + 1)
          case XMLSTART  => Tok.XMLStart(curr.offset)

          case COMMENT  => unreachable
          case IGNORE   => unreachable
          case ESCAPE   => unreachable
          case EMPTY    => unreachable
          case UNDEF    => unreachable
          case ERROR    => unreachable
        }
      }
      val scanner = new Scanner(origin)
      val buf = scanner.reader.buf

      var oldTokenBuf = new mutable.UnrolledBuffer[TokenData]
      scanner.foreach(curr => {
        // TODO: reinstate error handling
        // try oldTokenBuf += curr
        // catch { case e: Exception => scanner.report.error(e.getMessage) }
        // val tokenGetters = Tokens.getClass.getMethods.filter(_.getParameterTypes().length == 0)
        // println(tokenGetters.find(m => m.invoke(Tokens) == curr).get.getName)
        oldTokenBuf += new TokenData{}.copyFrom(curr)
      })
      val oldTokens = oldTokenBuf.toVector

      var newTokens = new mutable.UnrolledBuffer[Tok]
      def loop(startingFrom: Int, braceBalance: Int = 0, returnWhenBraceBalanceHitsZero: Boolean = false): Int = {
        var i = startingFrom
        def prev = oldTokens(i - 1)
        def curr = oldTokens(i)
        def emitToken() = newTokens += td2tok(curr)
        def nextToken() = i += 1
        if (i >= oldTokens.length) return i

        // NOTE: need to track this in order to correctly emit SpliceEnd tokens after splices end
        var braceBalance1 = braceBalance
        if (curr.token == LBRACE) braceBalance1 += 1
        if (curr.token == RBRACE) braceBalance1 -= 1
        if (braceBalance1 == 0 && returnWhenBraceBalanceHitsZero) return i

        emitToken()
        nextToken()

        if (prev.token == INTERPOLATIONID) {
          // NOTE: funnily enough, messing with interpolation tokens is what I've been doing roughly 3 years ago, on New Year's Eve of 2011/2012
          // I vividly remember spending 2 or 3 days making scanner emit detailed tokens for string interpolations, and that was tedious.
          // Now we need to do the same for our new token stream, but I don't really feel like going through the pain again.
          // Therefore, I'm giving up the 1-to-1 old-to-new token correspondence and will be trying to reverse engineer sane tokens here rather than in scanner.
          var startEnd = prev.endOffset + 1
          while (startEnd < buf.length && buf(startEnd) == '\"') startEnd += 1
          val numStartQuotes = startEnd - prev.endOffset - 1
          val numQuotes = if (numStartQuotes <= 2) 1 else 3
          def emitStart(offset: Int) = newTokens += Tok.Interpolation.Start("\"" * numQuotes, offset)
          def emitEnd(offset: Int) = newTokens += Tok.Interpolation.End("\"" * numQuotes, offset)
          def emitContents(): Unit = {
            require(curr.token == STRINGPART || curr.token == STRINGLIT)
            if (curr.token == STRINGPART) {
              newTokens += Tok.Interpolation.Part(curr.code, curr.name, curr.offset)
              require(buf(curr.endOffset + 1) == '$')
              val dollarOffset = curr.endOffset + 1
              def emitSpliceStart(code: String, offset: Int) = newTokens += Tok.Interpolation.SpliceStart(code, offset)
              def emitSpliceEnd(code: String, offset: Int) = newTokens += Tok.Interpolation.SpliceEnd(code, offset)
              def requireExpectedToken(expected: Token) = { require(curr.token == expected) }
              def emitExpectedToken(expected: Token) = { require(curr.token == expected); emitToken() }
              if (buf(dollarOffset + 1) == '{') {
                emitSpliceStart("${", dollarOffset)
                nextToken()
                requireExpectedToken(LBRACE)
                nextToken()
                i = loop(i, braceBalance = 1, returnWhenBraceBalanceHitsZero = true)
                requireExpectedToken(RBRACE)
                nextToken()
                emitSpliceEnd("}", curr.offset - 1)
                emitContents()
              } else if (buf(dollarOffset + 1) == '_') {
                emitSpliceStart("$_", dollarOffset)
                nextToken()
                emitExpectedToken(USCORE)
                nextToken()
                emitSpliceEnd("", curr.offset - 1)
                emitContents()
              } else {
                emitSpliceStart("$", dollarOffset)
                nextToken()
                emitExpectedToken(IDENTIFIER)
                nextToken()
                emitSpliceEnd("", curr.offset - 1)
                emitContents()
              }
            } else {
              curr.endOffset -= numQuotes
              newTokens += Tok.Interpolation.Part(curr.code, curr.name, curr.offset)
              require(buf(curr.endOffset + 1) == '\"')
              nextToken()
            }
          }
          numStartQuotes match {
            case 1 => emitStart(curr.offset - 1); emitContents(); emitEnd(curr.endOffset + 1)
            case 2 => emitStart(curr.offset); curr.endOffset -= 1; emitContents(); emitEnd(curr.endOffset + 1)
            case n if 3 <= n && n < 6 => emitStart(curr.offset - 3); emitContents(); emitEnd(curr.endOffset + 1)
            case 6 => emitStart(curr.offset - 3); emitContents(); emitEnd(curr.endOffset + 1)
          }
        }

        loop(i, braceBalance1, returnWhenBraceBalanceHitsZero)
      }

      loop(startingFrom = 0)
      newTokens.toVector
    }
  }
}
