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
      def td2tok(curr: TokenData): Tok = {
        (curr.token: @switch) match {
          case CHARLIT         => Tok.Literal.Char(origin, curr.offset, curr.endOffset, curr.charVal)
          case INTLIT          => Tok.Literal.Int(origin, curr.offset, curr.endOffset, curr.intVal(false).map(_.toInt).get)
          case LONGLIT         => Tok.Literal.Long(origin, curr.offset, curr.endOffset, curr.intVal(false).get)
          case FLOATLIT        => Tok.Literal.Float(origin, curr.offset, curr.endOffset, curr.floatVal(false).map(_.toFloat).get)
          case DOUBLELIT       => Tok.Literal.Double(origin, curr.offset, curr.endOffset, curr.floatVal(false).get)
          case STRINGLIT       => Tok.Literal.String(origin, curr.offset, curr.endOffset, curr.strVal)
          case SYMBOLLIT       => Tok.Literal.Symbol(origin, curr.offset, curr.endOffset, scala.Symbol(curr.strVal))

          case IDENTIFIER       => Tok.Ident(origin, curr.offset, curr.endOffset)
          case BACKQUOTED_IDENT => Tok.Ident(origin, curr.offset, curr.endOffset)
          case INTERPOLATIONID  => Tok.Interpolation.Id(origin, curr.offset, curr.endOffset)

          case NEW   => Tok.`new`(origin, curr.offset)
          case THIS  => Tok.`this`(origin, curr.offset)
          case SUPER => Tok.`super`(origin, curr.offset)
          case NULL  => Tok.`null`(origin, curr.offset)
          case TRUE  => Tok.`true`(origin, curr.offset)
          case FALSE => Tok.`false`(origin, curr.offset)

          case IMPLICIT  => Tok.`implicit`(origin, curr.offset)
          case OVERRIDE  => Tok.`override`(origin, curr.offset)
          case PROTECTED => Tok.`protected`(origin, curr.offset)
          case PRIVATE   => Tok.`private`(origin, curr.offset)
          case ABSTRACT  => Tok.`abstract`(origin, curr.offset)
          case FINAL     => Tok.`final`(origin, curr.offset)
          case SEALED    => Tok.`sealed`(origin, curr.offset)
          case LAZY      => Tok.`lazy`(origin, curr.offset)
          case MACRO     => Tok.`macro`(origin, curr.offset)

          case PACKAGE    => Tok.`package `(origin, curr.offset)
          case IMPORT     => Tok.`import`(origin, curr.offset)
          case CLASS      => Tok.`class `(origin, curr.offset)
          case CASECLASS  => unreachable
          case OBJECT     => Tok.`object`(origin, curr.offset)
          case CASEOBJECT => unreachable
          case TRAIT      => Tok.`trait`(origin, curr.offset)
          case EXTENDS    => Tok.`extends`(origin, curr.offset)
          case WITH       => Tok.`with`(origin, curr.offset)
          case TYPE       => Tok.`type`(origin, curr.offset)
          case FORSOME    => Tok.`forSome`(origin, curr.offset)
          case DEF        => Tok.`def`(origin, curr.offset)
          case VAL        => Tok.`val`(origin, curr.offset)
          case VAR        => Tok.`var`(origin, curr.offset)

          case IF      => Tok.`if`(origin, curr.offset)
          case THEN    => unreachable
          case ELSE    => Tok.`else`(origin, curr.offset)
          case WHILE   => Tok.`while`(origin, curr.offset)
          case DO      => Tok.`do`(origin, curr.offset)
          case FOR     => Tok.`for`(origin, curr.offset)
          case YIELD   => Tok.`yield`(origin, curr.offset)
          case THROW   => Tok.`throw`(origin, curr.offset)
          case TRY     => Tok.`try`(origin, curr.offset)
          case CATCH   => Tok.`catch`(origin, curr.offset)
          case FINALLY => Tok.`finally`(origin, curr.offset)
          case CASE    => Tok.`case`(origin, curr.offset)
          case RETURN  => Tok.`return`(origin, curr.offset)
          case MATCH   => Tok.`match`(origin, curr.offset)

          case LPAREN   => Tok.`(`(origin, curr.offset)
          case RPAREN   => Tok.`)`(origin, curr.offset)
          case LBRACKET => Tok.`[`(origin, curr.offset)
          case RBRACKET => Tok.`]`(origin, curr.offset)
          case LBRACE   => Tok.`{`(origin, curr.offset)
          case RBRACE   => Tok.`}`(origin, curr.offset)

          case COMMA     => Tok.`,`(origin, curr.offset)
          case SEMI      => Tok.`;`(origin, curr.offset)
          case DOT       => Tok.`.`(origin, curr.offset)
          case COLON     => Tok.`:`(origin, curr.offset)
          case EQUALS    => Tok.`=`(origin, curr.offset)
          case AT        => Tok.`@`(origin, curr.offset)
          case HASH      => Tok.`#`(origin, curr.offset)
          case USCORE    => Tok.`_ `(origin, curr.offset)
          case ARROW     => Tok.`=>`(origin, curr.offset)
          case LARROW    => Tok.`<-`(origin, curr.offset)
          case SUBTYPE   => Tok.`<:`(origin, curr.offset)
          case SUPERTYPE => Tok.`>:`(origin, curr.offset)
          case VIEWBOUND => Tok.`<%`(origin, curr.offset)

          case WHITESPACE =>
            if (curr.strVal == " ") Tok.` `(origin, curr.offset)
            else if (curr.strVal == "\t") Tok.`\t`(origin, curr.offset)
            else if (curr.strVal == "\r") Tok.`\r`(origin, curr.offset)
            else if (curr.strVal == "\n") Tok.`\n`(origin, curr.offset)
            else if (curr.strVal == "\f") Tok.`\f`(origin, curr.offset)
            else unreachable

          case COMMENT   => Tok.Comment(origin, curr.offset, curr.endOffset)

          case EOF       => Tok.EOF(origin)
          case XMLSTART  => Tok.XMLStart(origin, curr.offset, curr.endOffset)

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
        val currCopy = new TokenData{}.copyFrom(curr)
        if (currCopy.token == EOF) currCopy.offset = buf.length // NOTE: sometimes EOF's offset is `buf.length - 1`, and that might mess things up
        oldTokenBuf += currCopy
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
          // Therefore, I'm giving up the 1-to-1 old-to-new token correspondence and will be trying to reverse engineer sane tokens here rather than in scanner.
          var startEnd = prev.endOffset + 1
          while (startEnd < buf.length && buf(startEnd) == '\"') startEnd += 1
          val numStartQuotes = startEnd - prev.endOffset - 1
          val numQuotes = if (numStartQuotes <= 2) 1 else 3
          def emitStart(offset: Int) = newTokens += Tok.Interpolation.Start(origin, offset, offset + numQuotes - 1)
          def emitEnd(offset: Int) = newTokens += Tok.Interpolation.End(origin, offset, offset + numQuotes - 1)
          def emitContents(): Unit = {
            require(curr.token == STRINGPART || curr.token == STRINGLIT)
            if (curr.token == STRINGPART) {
              newTokens += Tok.Interpolation.Part(origin, curr.offset, curr.endOffset)
              require(buf(curr.endOffset + 1) == '$')
              val dollarOffset = curr.endOffset + 1
              def emitSpliceStart(offset: Int) = newTokens += Tok.Interpolation.SpliceStart(origin, offset)
              def emitSpliceEnd(offset: Int) = newTokens += Tok.Interpolation.SpliceEnd(origin, offset)
              def requireExpectedToken(expected: Token) = { require(curr.token == expected) }
              def emitExpectedToken(expected: Token) = { require(curr.token == expected); emitToken() }
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
                emitExpectedToken(IDENTIFIER)
                nextToken()
                emitSpliceEnd(curr.offset - 1)
                emitContents()
              }
            } else {
              curr.endOffset -= numQuotes
              newTokens += Tok.Interpolation.Part(origin, curr.offset, curr.endOffset)
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
      newTokens.toVector
    }
  }
}
