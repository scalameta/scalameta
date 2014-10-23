package scala.meta
package syntactic

import scala.collection.{immutable, mutable}
import scala.annotation.switch
import org.scalameta.convert._
import parsers.Tokens._

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

  trait Parse[T] extends Convert[Source, T]
  object Parse {
    def apply[T](f: Source => T): Parse[T] = new Parse[T] { def apply(source: Source): T = f(source) }
    implicit val parseCompUnit: Parse[Aux.CompUnit] = apply(source => new Parser(source).parseTopLevel())
    implicit val parseTerm: Parse[Term] = apply(source => new Parser(source).parseTerm())
    implicit val parseType: Parse[Type] = apply(source => new Parser(source).parseType())
    implicit val parseStats: Parse[List[Stat]] = apply(source => new Parser(source).parseStats())
    implicit val parseQ: Parse[Stat] = apply(source => new Parser(source).parseQ())
    implicit val parseT: Parse[Param.Type] = apply(source => new Parser(source).parseT())
    implicit val parseP: Parse[Pat] = apply(source => new Parser(source).parseP())
    implicit val parseParam: Parse[Param] = apply(source => new Parser(source).parseParam())
    implicit val parseTypeParam: Parse[TypeParam] = apply(source => new Parser(source).parseTypeParam())
    implicit val parseArg: Parse[Arg] = apply(source => new Parser(source).parseArg())
    implicit val parseEnum: Parse[Enum] = apply(source => new Parser(source).parseEnum())
    implicit val parseMod: Parse[Mod] = apply(source => new Parser(source).parseMod())
    implicit val parseCase: Parse[Aux.Case] = apply(source => new Parser(source).parseCase())
    implicit val parseParent: Parse[Aux.Parent] = apply(source => new Parser(source).parseParent())
    implicit val parseTemplate: Parse[Aux.Template] = apply(source => new Parser(source).parseTemplate())
    implicit val parseSelf: Parse[Aux.Self] = apply(source => new Parser(source).parseSelf())
  }

  implicit class RichSource[T](val sourceLike: T)(implicit ev: Convert[T, Source]) {
    private val source: Source = ev(sourceLike)
    def parse[T](implicit ev: Parse[T]): T = ev(source)
    def tokens: immutable.IndexedSeq[Tok] = {
      val scanner = new Scanner(source)
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
