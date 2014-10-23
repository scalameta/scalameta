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
    implicit val parseStats: Parse[List[Stmt.Template]] = apply(source => new Parser(source).parseStats())
    implicit val parseQ: Parse[Stmt] = apply(source => new Parser(source).parseQ())
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
    def tokens: immutable.IndexedSeq[(Tok, Offset)] = {
      val scanner = new Scanner(source)
      scanner.init()
      var buf = new mutable.UnrolledBuffer[(Tok, Offset)]
      while (scanner.token != EOF) {
        val tok = (scanner.token: @switch) match {
          case EMPTY => ???
          case UNDEF => ???
          case ERROR => ???
          case EOF   => Tok.EOF()

          case CHARLIT         => Tok.Literal.Char(scanner.charVal)
          case INTLIT          => Tok.Literal.Int(scanner.intVal(false).toInt)
          case LONGLIT         => Tok.Literal.Long(scanner.intVal(false))
          case FLOATLIT        => Tok.Literal.Float(scanner.floatVal(false).toFloat)
          case DOUBLELIT       => Tok.Literal.Double(scanner.floatVal(false))
          case STRINGLIT       => Tok.Literal.String(scanner.strVal)
          case SYMBOLLIT       => Tok.Literal.Symbol(scala.Symbol(scanner.strVal))
          case INTERPOLATIONID => Tok.Interpolation.Id(scanner.name)
          case STRINGPART      => Tok.Interpolation.Part(scanner.strVal)                    

          case IDENTIFIER       => Tok.Ident(scanner.name, isBackquoted = false)
          case BACKQUOTED_IDENT => Tok.Ident(scanner.name, isBackquoted = true)

          case NEW   => Tok.`new`()
          case THIS  => Tok.`this`()
          case SUPER => Tok.`super`()
          case NULL  => Tok.`null`()
          case TRUE  => Tok.`true`()
          case FALSE => Tok.`false`()

          case IMPLICIT  => Tok.`implicit`()
          case OVERRIDE  => Tok.`override`()
          case PROTECTED => Tok.`protected`()
          case PRIVATE   => Tok.`private`()
          case ABSTRACT  => Tok.`abstract`()
          case FINAL     => Tok.`final`()
          case SEALED    => Tok.`sealed`()
          case LAZY      => Tok.`lazy`()
          case MACRO     => Tok.`macro`()

          case PACKAGE    => Tok.`package `()
          case IMPORT     => Tok.`import`()
          case CLASS      => Tok.`class `()
          case CASECLASS  => Tok.`case class`()
          case OBJECT     => Tok.`object`()
          case CASEOBJECT => Tok.`case object`()
          case TRAIT      => Tok.`trait`()
          case EXTENDS    => Tok.`extends`()
          case WITH       => Tok.`with`()
          case TYPE       => Tok.`type`()
          case FORSOME    => Tok.`forSome`()
          case DEF        => Tok.`def`()
          case VAL        => Tok.`val`()
          case VAR        => Tok.`var`()

          case IF      => Tok.`if`()
          case THEN    => ???
          case ELSE    => Tok.`else`()
          case WHILE   => Tok.`while`()
          case DO      => Tok.`do`()
          case FOR     => Tok.`for`()
          case YIELD   => Tok.`yield`()
          case THROW   => Tok.`throw`()
          case TRY     => Tok.`try`()
          case CATCH   => Tok.`catch`()
          case FINALLY => Tok.`finally`()
          case CASE    => Tok.`case`()
          case RETURN  => Tok.`return`()
          case MATCH   => Tok.`match`()

          case LPAREN   => Tok.`(`()
          case RPAREN   => Tok.`)`()
          case LBRACKET => Tok.`[`()
          case RBRACKET => Tok.`]`()
          case LBRACE   => Tok.`{`()
          case RBRACE   => Tok.`}`()

          case COMMA     => Tok.`,`()
          case SEMI      => Tok.`;`()
          case DOT       => Tok.`.`()
          case COLON     => Tok.`:`()
          case EQUALS    => Tok.`=`()
          case AT        => Tok.`@`()
          case HASH      => Tok.`#`()
          case USCORE    => Tok.`_ `()
          case ARROW     => Tok.`=>`()
          case LARROW    => Tok.`<-`()
          case SUBTYPE   => Tok.`<:`()
          case SUPERTYPE => Tok.`>:`()
          case VIEWBOUND => Tok.`<%`()
          case NEWLINE   => Tok.`\n`()
          case NEWLINES  => Tok.`\n\n`()
          case XMLSTART  => Tok.XMLStart()

          case COMMENT    => ???
          case WHITESPACE => ???
          case IGNORE     => ???
          case ESCAPE     => ???
        }
        buf += ((tok, scanner.offset))
        scanner.nextToken()
      }
      buf += ((Tok.EOF(), scanner.offset))
      buf.toVector
    }    
  }
}
