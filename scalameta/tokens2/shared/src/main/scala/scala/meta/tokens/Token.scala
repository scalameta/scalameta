package scala.meta
package tokens

import scala.meta.classifiers._
import scala.meta.inputs._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.tokens._
import scala.meta.prettyprinters._

import scala.math.ScalaNumber

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Token.end can point to the last character plus one.
// Btw, Token.start can also point to the last character plus one if it's an EOF token.
@root
trait Token extends InternalToken with InputRange {
  def dialect: Dialect
  def pos: Position
  def text: String = pos.text

  def isEmpty: Boolean = start == end
  def len: Int = end - start
}

object Token {

  @branch
  trait MultiToken extends Token {
    def tokens: List[Token]
  }

  // Literals (include some keywords from above, constants, interpolations and xml)
  @branch
  trait Literal extends Token with ExprBegToken with StatEndToken
  @branch
  abstract class Constant[A] extends Literal {
    val value: A
  }
  @branch
  abstract class NumericConstant[A <: ScalaNumber] extends Constant[A]
  @branch
  abstract class BooleanConstant(val value: Boolean) extends Constant[Boolean]

  @branch
  trait Keyword extends Token
  @branch
  trait ModifierKeyword extends Keyword
  @branch
  private[meta] trait ContKeyword extends Token
  @branch
  private[meta] trait DeclBegKeyword extends Token
  @branch
  private[meta] trait TmplBegKeyword extends Token
  @branch
  private[meta] trait ExprBegToken extends Token
  @branch
  private[meta] trait StatDelim extends Token
  @branch
  private[meta] trait EndMarkerSpecifier extends Token
  @branch
  private[meta] trait StatEndToken extends Token

  @branch
  trait Trivia extends Token
  @branch
  trait HTrivia extends Trivia
  @branch
  trait Whitespace extends Trivia
  @branch
  trait HSpace extends Whitespace with HTrivia
  @branch
  trait AtEOLorF extends Token
  @branch
  trait AtEOL extends Whitespace with AtEOLorF with StatDelim {
    def newlines: Int = 1
  }
  @branch
  trait EOL extends AtEOL
  @branch
  trait MultiEOL extends AtEOL

  @branch
  trait Symbolic extends Token
  @branch
  trait SymbolicKeyword extends Symbolic
  @branch
  trait FunctionArrow extends SymbolicKeyword with ContKeyword
  @branch
  trait Punct extends Symbolic
  @branch
  trait OpenDelim extends Punct
  @branch
  trait CloseDelim extends Punct with StatEndToken

  // Identifiers
  @freeform("identifier")
  class Ident(value: String) extends Token with EndMarkerSpecifier with StatEndToken

  // Alphanumeric keywords
  @fixed("abstract")
  class KwAbstract extends ModifierKeyword
  @fixed("case")
  class KwCase extends Keyword
  @fixed("catch")
  class KwCatch extends Keyword with ContKeyword
  @fixed("class")
  class KwClass extends Keyword with TmplBegKeyword
  @fixed("def")
  class KwDef extends Keyword with DeclBegKeyword
  @fixed("do")
  class KwDo extends Keyword with ExprBegToken
  @fixed("else")
  class KwElse extends Keyword with ContKeyword
  @fixed("enum")
  class KwEnum extends Keyword with DeclBegKeyword
  @fixed("export")
  class KwExport extends Keyword
  @fixed("extends")
  class KwExtends extends Keyword with ContKeyword
  @fixed("false")
  class KwFalse extends BooleanConstant(false)
  @fixed("final")
  class KwFinal extends ModifierKeyword
  @fixed("finally")
  class KwFinally extends Keyword with ContKeyword
  @fixed("for")
  class KwFor extends Keyword with EndMarkerSpecifier with ExprBegToken
  @fixed("forSome")
  class KwForsome extends Keyword with ContKeyword
  @fixed("given")
  class KwGiven extends Keyword with EndMarkerSpecifier with DeclBegKeyword with StatEndToken
  @fixed("if")
  class KwIf extends Keyword with EndMarkerSpecifier with ExprBegToken
  @fixed("implicit")
  class KwImplicit extends ModifierKeyword
  @fixed("import")
  class KwImport extends Keyword
  @fixed("lazy")
  class KwLazy extends ModifierKeyword
  @fixed("match")
  class KwMatch extends Keyword with ContKeyword with EndMarkerSpecifier
  @fixed("macro")
  class KwMacro extends Keyword
  @fixed("new")
  class KwNew extends Keyword with EndMarkerSpecifier with ExprBegToken
  @fixed("null")
  class KwNull extends Literal
  @fixed("object")
  class KwObject extends Keyword with TmplBegKeyword
  @fixed("override")
  class KwOverride extends ModifierKeyword
  @fixed("package")
  class KwPackage extends Keyword
  @fixed("private")
  class KwPrivate extends ModifierKeyword
  @fixed("protected")
  class KwProtected extends ModifierKeyword
  @fixed("return")
  class KwReturn extends Keyword with ExprBegToken with StatEndToken
  @fixed("sealed")
  class KwSealed extends ModifierKeyword
  @fixed("super")
  class KwSuper extends Keyword with ExprBegToken
  @fixed("then")
  class KwThen extends Keyword
  @fixed("this")
  class KwThis extends Keyword with EndMarkerSpecifier with ExprBegToken with StatEndToken
  @fixed("throw")
  class KwThrow extends Keyword with ExprBegToken
  @fixed("trait")
  class KwTrait extends Keyword with TmplBegKeyword
  @fixed("true")
  class KwTrue extends BooleanConstant(true)
  @fixed("try")
  class KwTry extends Keyword with EndMarkerSpecifier with ExprBegToken
  @fixed("type")
  class KwType extends Keyword with DeclBegKeyword with StatEndToken
  @fixed("val")
  class KwVal extends Keyword with EndMarkerSpecifier with DeclBegKeyword
  @fixed("var")
  class KwVar extends Keyword with DeclBegKeyword
  @fixed("while")
  class KwWhile extends Keyword with EndMarkerSpecifier with ExprBegToken
  @fixed("with")
  class KwWith extends Keyword with ContKeyword
  @fixed("yield")
  class KwYield extends Keyword with ContKeyword

  // Symbolic keywords
  @fixed("#")
  class Hash extends SymbolicKeyword with ContKeyword
  @fixed(":")
  class Colon extends SymbolicKeyword with ContKeyword
  @fixed("<%")
  class Viewbound extends SymbolicKeyword with ContKeyword
  @freeform("<-")
  class LeftArrow extends SymbolicKeyword with ContKeyword
  @fixed("<:")
  class Subtype extends SymbolicKeyword with ContKeyword
  @fixed("=")
  class Equals extends SymbolicKeyword with ContKeyword
  @freeform("=>")
  class RightArrow extends FunctionArrow
  @fixed(">:")
  class Supertype extends SymbolicKeyword with ContKeyword
  @fixed("@")
  class At extends SymbolicKeyword
  @fixed("_")
  class Underscore extends SymbolicKeyword with ExprBegToken with StatEndToken
  @fixed("=>>")
  class TypeLambdaArrow extends SymbolicKeyword with ContKeyword
  @fixed("?=>")
  class ContextArrow extends FunctionArrow
  @fixed("'")
  class MacroQuote extends SymbolicKeyword with ExprBegToken
  @fixed("$") @deprecated("use Ident($) instead", "v4.14.5")
  private[meta] class MacroSplice extends SymbolicKeyword

  // Delimiters
  @fixed("(")
  class LeftParen extends OpenDelim with ExprBegToken
  @fixed(")")
  class RightParen extends CloseDelim
  @fixed(",")
  class Comma extends Punct
  @fixed(".")
  class Dot extends Punct
  @fixed(";")
  class Semicolon extends Punct with StatDelim
  @fixed("[")
  class LeftBracket extends OpenDelim
  @fixed("]")
  class RightBracket extends CloseDelim
  @fixed("{")
  class LeftBrace extends OpenDelim with ExprBegToken
  @fixed("}")
  class RightBrace extends CloseDelim

  object Constant {
    @freeform("integer constant")
    class Int(value: BigInt) extends NumericConstant[BigInt]
    @freeform("long constant")
    class Long(value: BigInt) extends NumericConstant[BigInt]
    @freeform("generic integer constant")
    class IntXL(value: BigInt) extends NumericConstant[BigInt]
    @freeform("float constant")
    class Float(value: BigDecimal) extends NumericConstant[BigDecimal]
    @freeform("double constant")
    class Double(value: BigDecimal) extends NumericConstant[BigDecimal]
    @freeform("generic floating-point constant")
    class FloatXL(value: AnyDecimal) extends Constant[AnyDecimal]
    @freeform("character constant")
    class Char(value: scala.Char) extends Constant[scala.Char]
    @freeform("symbol constant")
    class Symbol(value: scala.Symbol) extends Constant[scala.Symbol]
    @freeform("string constant")
    class String(value: Predef.String) extends Constant[Predef.String]
  }
  // NOTE: Here's example tokenization of q"${foo}bar".
  // BOF, Id(q)<"q">, Start<"\"">, Part("")<"">, SpliceStart<"$">, {, foo, }, SpliceEnd<"">, Part("bar")<"bar">, End("\""), EOF.
  // As you can see, SpliceEnd is always empty, but I still decided to expose it for consistency reasons.
  object Interpolation {
    @freeform("interpolation id")
    class Id(value: String) extends Token with ExprBegToken
    @freeform("interpolation start")
    class Start extends Token
    @freeform("interpolation part")
    class Part(value: String) extends Token
    @freeform("splice start")
    class SpliceStart extends Token
    @freeform("splice end")
    class SpliceEnd extends Token
    @freeform("interpolation end")
    class End extends Token with StatEndToken
  }
  object Xml {
    @freeform("xml start")
    class Start extends Token with ExprBegToken {
      require(dialect.allowXmlLiterals, s"$dialect doesn't support xml literals")
    }
    @freeform("xml part")
    class Part(value: String) extends Token {
      require(dialect.allowXmlLiterals, s"$dialect doesn't support xml literals")
    }
    @freeform("xml splice start")
    class SpliceStart extends Token {
      require(dialect.allowXmlLiterals, s"$dialect doesn't support xml literals")
    }
    @freeform("xml splice end")
    class SpliceEnd extends Token {
      require(dialect.allowXmlLiterals, s"$dialect doesn't support xml literals")
    }
    @freeform("xml end")
    class End extends Token with StatEndToken {
      require(dialect.allowXmlLiterals, s"$dialect doesn't support xml literals")
    }
  }

  @branch
  trait Indentation extends Whitespace
  object Indentation {
    @freeform("indent")
    class Indent extends Indentation with ExprBegToken
    @freeform("outdent")
    class Outdent extends Indentation
  }

  // Trivia
  @fixed(" ")
  class Space extends HSpace
  @fixed("\t")
  class Tab extends HSpace
  @fixed("\r")
  class CR extends EOL
  @fixed("\n")
  class LF extends EOL
  @fixed("\f")
  class FF extends EOL
  @fixed("\r\n")
  class CRLF extends EOL
  @freeform("multiple horizontal spaces")
  class MultiHS(tokens: List[HSpace]) extends HSpace
  @freeform("multiple newlines")
  class MultiNL(tokens: List[EOL]) extends MultiEOL with MultiToken {
    override def newlines: Int = tokens.length
  }
  @freeform("comment")
  class Comment(value: String) extends HTrivia
  @freeform("comment_start")
  class CommentStart(value: String) extends HTrivia
  @freeform("comment_part")
  class CommentPart(value: String) extends HTrivia
  @freeform("comment_end")
  class CommentEnd(value: String) extends HTrivia
  @freeform("comment_unquote")
  class CommentUnquote extends HTrivia
  @freeform("beginning of file")
  class BOF extends AtEOLorF {
    def this(input: Input, dialect: Dialect) = this(input, dialect, 0)
    def end = start
  }
  @freeform("`#!` line at the beginning of file`")
  class Shebang(value: String) extends Token
  @freeform("end of file")
  class EOF extends AtEOLorF {
    def this(input: Input, dialect: Dialect) = this(input, dialect, input.chars.length)
    def end = start
  }
  @freeform("\n\n")
  private[meta] class LFLF extends MultiEOL
  @freeform("\n")
  private[meta] class InfixLF(invalid: Option[String]) extends EOL

  // NOTE: in order to maintain conceptual compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  @freeform("ellipsis")
  private[meta] class Ellipsis(rank: Int) extends Token {
    require(dialect.allowUnquotes, s"$dialect doesn't support unquoting")
  }
  @freeform("unquote")
  private[meta] class Unquote extends Token {
    require(dialect.allowUnquotes, s"$dialect doesn't support unquoting")
  }

  @freeform("invalid token, tokenizer error")
  private[meta] class Invalid(error: String) extends Token

  implicit def classifiable[T <: Token]: Classifiable[T] = null
  implicit def showStructure[T <: Token]: Structure[T] = TokenStructure.apply[T]
  implicit def showSyntax[T <: Token](implicit dialect: Dialect): Syntax[T] = TokenSyntax.apply[T]

  val pureFunctionArrow = "->"
  val pureContextFunctionArrow = "?->"

}
