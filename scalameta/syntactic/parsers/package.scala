package scala.meta.syntactic

import scala.collection.{immutable, mutable}
import scala.annotation.switch
import org.scalameta.convert._
import parsers.Tokens._
import scala.meta._

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
    implicit val parseTopLevel: Parse[TopLevel] = apply(origin => new Parser(origin).parseTopLevel())
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
      val scanner = new Scanner(origin)
      scanner.init()
      var buf = new mutable.UnrolledBuffer[Tok]
      var tok: Tok = null
      do {
        tok = (scanner.token: @switch) match {
          case EMPTY => ???
          case UNDEF => ???
          case ERROR => ???
          case EOF   => Tok.EOF(scanner.offset)

          case CHARLIT         => Tok.Literal.Char(scanner.charVal, scanner.offset)
          case INTLIT          => Tok.Literal.Int(scanner.intVal(false).toInt, scanner.offset)
          case LONGLIT         => Tok.Literal.Long(scanner.intVal(false), scanner.offset)
          case FLOATLIT        => Tok.Literal.Float(scanner.floatVal(false).toFloat, scanner.offset)
          case DOUBLELIT       => Tok.Literal.Double(scanner.floatVal(false), scanner.offset)
          case STRINGLIT       => Tok.Literal.String(scanner.strVal, scanner.offset)
          case SYMBOLLIT       => Tok.Literal.Symbol(scala.Symbol(scanner.strVal), scanner.offset)
          case INTERPOLATIONID => Tok.Interpolation.Id(scanner.name, scanner.offset)
          case STRINGPART      => Tok.Interpolation.Part(scanner.strVal, scanner.offset)

          case IDENTIFIER       => Tok.Ident(scanner.name, isBackquoted = false, scanner.offset)
          case BACKQUOTED_IDENT => Tok.Ident(scanner.name, isBackquoted = true, scanner.offset)

          case NEW   => Tok.`new`(scanner.offset)
          case THIS  => Tok.`this`(scanner.offset)
          case SUPER => Tok.`super`(scanner.offset)
          case NULL  => Tok.`null`(scanner.offset)
          case TRUE  => Tok.`true`(scanner.offset)
          case FALSE => Tok.`false`(scanner.offset)

          case IMPLICIT  => Tok.`implicit`(scanner.offset)
          case OVERRIDE  => Tok.`override`(scanner.offset)
          case PROTECTED => Tok.`protected`(scanner.offset)
          case PRIVATE   => Tok.`private`(scanner.offset)
          case ABSTRACT  => Tok.`abstract`(scanner.offset)
          case FINAL     => Tok.`final`(scanner.offset)
          case SEALED    => Tok.`sealed`(scanner.offset)
          case LAZY      => Tok.`lazy`(scanner.offset)
          case MACRO     => Tok.`macro`(scanner.offset)

          case PACKAGE    => Tok.`package `(scanner.offset)
          case IMPORT     => Tok.`import`(scanner.offset)
          case CLASS      => Tok.`class `(scanner.offset)
          case CASECLASS  => Tok.`case class`(scanner.offset)
          case OBJECT     => Tok.`object`(scanner.offset)
          case CASEOBJECT => Tok.`case object`(scanner.offset)
          case TRAIT      => Tok.`trait`(scanner.offset)
          case EXTENDS    => Tok.`extends`(scanner.offset)
          case WITH       => Tok.`with`(scanner.offset)
          case TYPE       => Tok.`type`(scanner.offset)
          case FORSOME    => Tok.`forSome`(scanner.offset)
          case DEF        => Tok.`def`(scanner.offset)
          case VAL        => Tok.`val`(scanner.offset)
          case VAR        => Tok.`var`(scanner.offset)

          case IF      => Tok.`if`(scanner.offset)
          case THEN    => ???
          case ELSE    => Tok.`else`(scanner.offset)
          case WHILE   => Tok.`while`(scanner.offset)
          case DO      => Tok.`do`(scanner.offset)
          case FOR     => Tok.`for`(scanner.offset)
          case YIELD   => Tok.`yield`(scanner.offset)
          case THROW   => Tok.`throw`(scanner.offset)
          case TRY     => Tok.`try`(scanner.offset)
          case CATCH   => Tok.`catch`(scanner.offset)
          case FINALLY => Tok.`finally`(scanner.offset)
          case CASE    => Tok.`case`(scanner.offset)
          case RETURN  => Tok.`return`(scanner.offset)
          case MATCH   => Tok.`match`(scanner.offset)

          case LPAREN   => Tok.`(`(scanner.offset)
          case RPAREN   => Tok.`)`(scanner.offset)
          case LBRACKET => Tok.`[`(scanner.offset)
          case RBRACKET => Tok.`]`(scanner.offset)
          case LBRACE   => Tok.`{`(scanner.offset)
          case RBRACE   => Tok.`}`(scanner.offset)

          case COMMA     => Tok.`,`(scanner.offset)
          case SEMI      => Tok.`;`(scanner.offset)
          case DOT       => Tok.`.`(scanner.offset)
          case COLON     => Tok.`:`(scanner.offset)
          case EQUALS    => Tok.`=`(scanner.offset)
          case AT        => Tok.`@`(scanner.offset)
          case HASH      => Tok.`#`(scanner.offset)
          case USCORE    => Tok.`_ `(scanner.offset)
          case ARROW     => Tok.`=>`(scanner.offset)
          case LARROW    => Tok.`<-`(scanner.offset)
          case SUBTYPE   => Tok.`<:`(scanner.offset)
          case SUPERTYPE => Tok.`>:`(scanner.offset)
          case VIEWBOUND => Tok.`<%`(scanner.offset)
          case NEWLINE   => Tok.`\n`(scanner.offset)
          case NEWLINES  => Tok.`\n\n`(scanner.offset)
          case XMLSTART  => Tok.XMLStart(scanner.offset)

          case COMMENT    => ???
          case WHITESPACE => ???
          case IGNORE     => ???
          case ESCAPE     => ???
        }
        buf += tok
        scanner.nextToken()
      } while (tok.isNot[Tok.EOF])
      buf.toVector
    }
  }
}
