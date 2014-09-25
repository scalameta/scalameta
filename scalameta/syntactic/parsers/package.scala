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
          case EMPTY => Tok.Empty
          case UNDEF => Tok.Undef
          case ERROR => Tok.Error
          case EOF   => Tok.EOF

          case CHARLIT         => Tok.Char(scanner.charVal)
          case INTLIT          => Tok.Int(scanner.intVal(false).toInt)
          case LONGLIT         => Tok.Long(scanner.intVal(false))
          case FLOATLIT        => Tok.Float(scanner.floatVal(false).toFloat)
          case DOUBLELIT       => Tok.Double(scanner.floatVal(false))
          case STRINGLIT       => Tok.String(scanner.strVal)
          case STRINGPART      => Tok.StringPart(scanner.strVal)
          case SYMBOLLIT       => Tok.Symbol(scala.Symbol(scanner.strVal))
          case INTERPOLATIONID => Tok.InterpolationId(scanner.name)

          case IDENTIFIER       => Tok.Ident(scanner.name, isBackquoted = false)
          case BACKQUOTED_IDENT => Tok.Ident(scanner.name, isBackquoted = true)

          case NEW   => Tok.New
          case THIS  => Tok.This
          case SUPER => Tok.Super
          case NULL  => Tok.Null
          case TRUE  => Tok.True
          case FALSE => Tok.False

          case IMPLICIT  => Tok.Implicit
          case OVERRIDE  => Tok.Override
          case PROTECTED => Tok.Protected
          case PRIVATE   => Tok.Private
          case ABSTRACT  => Tok.Abstract
          case FINAL     => Tok.Final
          case SEALED    => Tok.Sealed
          case LAZY      => Tok.Lazy
          case MACRO     => Tok.Macro

          case PACKAGE    => Tok.Package
          case IMPORT     => Tok.Import
          case CLASS      => Tok.Class
          case CASECLASS  => Tok.CaseClass
          case OBJECT     => Tok.Object
          case CASEOBJECT => Tok.CaseObject
          case TRAIT      => Tok.Trait
          case EXTENDS    => Tok.Extends
          case WITH       => Tok.With
          case TYPE       => Tok.Type
          case FORSOME    => Tok.ForSome
          case DEF        => Tok.Def
          case VAL        => Tok.Val
          case VAR        => Tok.Var

          case IF      => Tok.If
          case THEN    => ???
          case ELSE    => Tok.Else
          case WHILE   => Tok.While
          case DO      => Tok.Do
          case FOR     => Tok.For
          case YIELD   => Tok.Yield
          case THROW   => Tok.Throw
          case TRY     => Tok.Try
          case CATCH   => Tok.Catch
          case FINALLY => Tok.Finally
          case CASE    => Tok.Case
          case RETURN  => Tok.Return
          case MATCH   => Tok.Match

          case LPAREN   => Tok.`(`
          case RPAREN   => Tok.`)`
          case LBRACKET => Tok.`[`
          case RBRACKET => Tok.`]`
          case LBRACE   => Tok.`{`
          case RBRACE   => Tok.`}`

          case COMMA     => Tok.`,`
          case SEMI      => Tok.`;`
          case DOT       => Tok.`.`
          case COLON     => Tok.`:`
          case EQUALS    => Tok.`=`
          case AT        => Tok.`@`
          case HASH      => Tok.`#`
          case USCORE    => Tok.`_`
          case ARROW     => Tok.`=>`
          case LARROW    => Tok.`<-`
          case SUBTYPE   => Tok.`<:`
          case SUPERTYPE => Tok.`>:`
          case VIEWBOUND => Tok.`<%`
          case NEWLINE   => Tok.`\n`
          case NEWLINES  => Tok.`\n\n`
          case XMLSTART  => Tok.XMLStart

          case COMMENT    => ???
          case WHITESPACE => Tok.` `
          case IGNORE     => Tok.Ignore
          case ESCAPE     => Tok.Escape
        }
        buf += ((tok, scanner.offset))
        scanner.nextToken()
      }
      buf += ((Tok.EOF, scanner.offset))
      buf.toVector
    }    
  }
}
