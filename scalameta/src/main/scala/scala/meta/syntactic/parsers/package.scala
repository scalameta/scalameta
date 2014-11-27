package scala.meta.syntactic

import scala.collection.{immutable, mutable}
import scala.annotation.switch
import org.scalameta.convert._
import org.scalameta.unreachable
import parsers.Tokens._
import scala.meta._
import scala.meta.syntactic.parsers.Chars.{CR, LF, FF}

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
        import curr.offset
        (curr.token: @switch) match {
          case CHARLIT         => Tok.Literal.Char(curr.charVal, offset)
          case INTLIT          => Tok.Literal.Int(curr.intVal(false).map(_.toInt).get, offset)
          case LONGLIT         => Tok.Literal.Long(curr.intVal(false).get, offset)
          case FLOATLIT        => Tok.Literal.Float(curr.floatVal(false).map(_.toFloat).get, offset)
          case DOUBLELIT       => Tok.Literal.Double(curr.floatVal(false).get, offset)
          case STRINGLIT       => Tok.Literal.String(curr.strVal, offset)
          case SYMBOLLIT       => Tok.Literal.Symbol(scala.Symbol(curr.strVal), offset)
          case INTERPOLATIONID => Tok.Interpolation.Id(curr.name, offset)
          case STRINGPART      => Tok.Interpolation.Part(curr.strVal, offset)

          case IDENTIFIER       => Tok.Ident(curr.name, isBackquoted = false, offset)
          case BACKQUOTED_IDENT => Tok.Ident(curr.name, isBackquoted = true, offset)

          case NEW   => Tok.`new`(offset)
          case THIS  => Tok.`this`(offset)
          case SUPER => Tok.`super`(offset)
          case NULL  => Tok.`null`(offset)
          case TRUE  => Tok.`true`(offset)
          case FALSE => Tok.`false`(offset)

          case IMPLICIT  => Tok.`implicit`(offset)
          case OVERRIDE  => Tok.`override`(offset)
          case PROTECTED => Tok.`protected`(offset)
          case PRIVATE   => Tok.`private`(offset)
          case ABSTRACT  => Tok.`abstract`(offset)
          case FINAL     => Tok.`final`(offset)
          case SEALED    => Tok.`sealed`(offset)
          case LAZY      => Tok.`lazy`(offset)
          case MACRO     => Tok.`macro`(offset)

          case PACKAGE    => Tok.`package `(offset)
          case IMPORT     => Tok.`import`(offset)
          case CLASS      => Tok.`class `(offset)
          case CASECLASS  => unreachable
          case OBJECT     => Tok.`object`(offset)
          case CASEOBJECT => unreachable
          case TRAIT      => Tok.`trait`(offset)
          case EXTENDS    => Tok.`extends`(offset)
          case WITH       => Tok.`with`(offset)
          case TYPE       => Tok.`type`(offset)
          case FORSOME    => Tok.`forSome`(offset)
          case DEF        => Tok.`def`(offset)
          case VAL        => Tok.`val`(offset)
          case VAR        => Tok.`var`(offset)

          case IF      => Tok.`if`(offset)
          case THEN    => unreachable
          case ELSE    => Tok.`else`(offset)
          case WHILE   => Tok.`while`(offset)
          case DO      => Tok.`do`(offset)
          case FOR     => Tok.`for`(offset)
          case YIELD   => Tok.`yield`(offset)
          case THROW   => Tok.`throw`(offset)
          case TRY     => Tok.`try`(offset)
          case CATCH   => Tok.`catch`(offset)
          case FINALLY => Tok.`finally`(offset)
          case CASE    => Tok.`case`(offset)
          case RETURN  => Tok.`return`(offset)
          case MATCH   => Tok.`match`(offset)

          case LPAREN   => Tok.`(`(offset)
          case RPAREN   => Tok.`)`(offset)
          case LBRACKET => Tok.`[`(offset)
          case RBRACKET => Tok.`]`(offset)
          case LBRACE   => Tok.`{`(offset)
          case RBRACE   => Tok.`}`(offset)

          case COMMA     => Tok.`,`(offset)
          case SEMI      => Tok.`;`(offset)
          case DOT       => Tok.`.`(offset)
          case COLON     => Tok.`:`(offset)
          case EQUALS    => Tok.`=`(offset)
          case AT        => Tok.`@`(offset)
          case HASH      => Tok.`#`(offset)
          case USCORE    => Tok.`_ `(offset)
          case ARROW     => Tok.`=>`(offset)
          case LARROW    => Tok.`<-`(offset)
          case SUBTYPE   => Tok.`<:`(offset)
          case SUPERTYPE => Tok.`>:`(offset)
          case VIEWBOUND => Tok.`<%`(offset)

          case NEWLINE                           => Tok.`\n`(offset)
          case WHITESPACE if curr.strVal == "\n" => Tok.`\n`(offset)
          case WHITESPACE if curr.strVal == "\t" => Tok.`\t`(offset)
          case WHITESPACE if curr.strVal == " "  => Tok.` `(offset)
          case WHITESPACE if curr.strVal == CR   => Tok.CarriageReturn(offset)
          case WHITESPACE if curr.strVal == LF   => Tok.LineFeed(offset)
          case WHITESPACE if curr.strVal == FF   => Tok.FormFeed(offset)
          case WHITESPACE                        => unreachable

          case EOF       => Tok.EOF(offset)
          case XMLSTART  => Tok.XMLStart(offset)

          case COMMENT  => unreachable
          case IGNORE   => unreachable
          case ESCAPE   => unreachable
          case EMPTY    => unreachable
          case UNDEF    => unreachable
          case ERROR    => unreachable
          case NEWLINES => unreachable
        }
      }

      var buf = new mutable.UnrolledBuffer[Tok]
      val scanner = new Scanner(origin)
      scanner.foreach { curr =>
        try {
          buf += td2tok(curr)
        } catch {
          case e: Exception =>
            scanner.report.error(e.getMessage)
        }
      }
      buf.toVector
    }
  }
}
