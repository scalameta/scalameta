package scala.meta
package tokens

import org.scalameta.adt.{Liftables => AdtLiftables}
import scala.meta.internal.tokens
import scala.meta.internal.tokens._
import scala.meta.inputs._
import scala.meta.classifiers._
import scala.meta.dialects.Metalevel
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Token.end can point to the last character plus one.
// Btw, Token.start can also point to the last character plus one if it's an EOF token.
@root trait Token {
  def input: Input
  def dialect: Dialect
  def start: Int
  def end: Int
  def pos: Position
}

object Token {
  // Identifiers
  @freeform("identifier") class Ident(value: String) extends Token

  // Alphanumeric keywords
  @fixed("abstract") class KwAbstract extends Token
  @fixed("case") class KwCase extends Token
  @fixed("catch") class KwCatch extends Token
  @fixed("class") class KwClass extends Token
  @fixed("def") class KwDef extends Token
  @fixed("do") class KwDo extends Token
  @fixed("else") class KwElse extends Token
  @fixed("extends") class KwExtends extends Token
  @fixed("false") class KwFalse extends Token
  @fixed("final") class KwFinal extends Token
  @fixed("finally") class KwFinally extends Token
  @fixed("for") class KwFor extends Token
  @fixed("forSome") class KwForsome extends Token
  @fixed("if") class KwIf extends Token
  @fixed("implicit") class KwImplicit extends Token
  @fixed("import") class KwImport extends Token
  @fixed("lazy") class KwLazy extends Token
  @fixed("match") class KwMatch extends Token
  @fixed("macro") class KwMacro extends Token
  @fixed("new") class KwNew extends Token
  @fixed("null") class KwNull extends Token
  @fixed("object") class KwObject extends Token
  @fixed("override") class KwOverride extends Token
  @fixed("package") class KwPackage extends Token
  @fixed("private") class KwPrivate extends Token
  @fixed("protected") class KwProtected extends Token
  @fixed("return") class KwReturn extends Token
  @fixed("sealed") class KwSealed extends Token
  @fixed("super") class KwSuper extends Token
  @fixed("this") class KwThis extends Token
  @fixed("throw") class KwThrow extends Token
  @fixed("trait") class KwTrait extends Token
  @fixed("true") class KwTrue extends Token
  @fixed("try") class KwTry extends Token
  @fixed("type") class KwType extends Token
  @fixed("val") class KwVal extends Token
  @fixed("var") class KwVar extends Token
  @fixed("while") class KwWhile extends Token
  @fixed("with") class KwWith extends Token
  @fixed("yield") class KwYield extends Token

  // Symbolic keywords
  @fixed("#") class Hash extends Token
  @fixed(":") class Colon extends Token
  @fixed("<%") class Viewbound extends Token
  @freeform("<-") class LeftArrow extends Token
  @fixed("<:") class Subtype extends Token
  @fixed("=") class Equals extends Token
  @freeform("=>") class RightArrow extends Token
  @fixed(">:") class Supertype extends Token
  @fixed("@") class At extends Token
  @fixed("_") class Underscore extends Token

  // Delimiters
  @fixed("(") class LeftParen extends Token
  @fixed(")") class RightParen extends Token
  @fixed(",") class Comma extends Token
  @fixed(".") class Dot extends Token
  @fixed(";") class Semicolon extends Token
  @fixed("[") class LeftBracket extends Token
  @fixed("]") class RightBracket extends Token
  @fixed("{") class LeftBrace extends Token
  @fixed("}") class RightBrace extends Token

  // Literals (include some keywords from above, constants, interpolations and xml)
  object Constant {
    @freeform("integer constant") class Int(value: scala.BigInt) extends Token
    @freeform("long constant") class Long(value: scala.BigInt) extends Token
    @freeform("float constant") class Float(value: scala.BigDecimal) extends Token
    @freeform("double constant") class Double(value: scala.BigDecimal) extends Token
    @freeform("character constant") class Char(value: scala.Char) extends Token
    @freeform("symbol constant") class Symbol(value: scala.Symbol) extends Token
    @freeform("string constant") class String(value: Predef.String) extends Token
  }
  // NOTE: Here's example tokenization of q"${foo}bar".
  // BOF, Id(q)<"q">, Start<"\"">, Part("")<"">, SpliceStart<"$">, {, foo, }, SpliceEnd<"">, Part("bar")<"bar">, End("\""), EOF.
  // As you can see, SpliceEnd is always empty, but I still decided to expose it for consistency reasons.
  object Interpolation {
    @freeform("interpolation id") class Id(value: String) extends Token
    @freeform("interpolation start") class Start extends Token
    @freeform("interpolation part") class Part(value: String) extends Token
    @freeform("splice start") class SpliceStart extends Token
    @freeform("splice end") class SpliceEnd extends Token
    @freeform("interpolation end") class End extends Token
  }
  object Xml {
    @freeform("xml start") class Start extends Token
    @freeform("xml part") class Part(value: String) extends Token
    @freeform("xml splice start") class SpliceStart extends Token
    @freeform("xml splice end") class SpliceEnd extends Token
    @freeform("xml end") class End extends Token
  }

  // Trivia
  @fixed(" ") class Space extends Token
  @fixed("\t") class Tab extends Token
  @fixed("\r") class CR extends Token
  @fixed("\n") class LF extends Token
  @fixed("\f") class FF extends Token
  @freeform("comment") class Comment(value: String) extends Token
  @freeform("beginning of file") class BOF extends Token { def start = 0; def end = 0 }
  @freeform("end of file") class EOF extends Token { def start = input.chars.length; def end = input.chars.length }

  // TODO: Rewrite the parser so that it doesn't need LFLF anymore.
  // NOTE: in order to maintain conceptual compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  @freeform("\n\n") private[meta] class LFLF extends Token
  @freeform("ellipsis") private[meta] class Ellipsis(rank: Int) extends Token
  @freeform("unquote") private[meta] class Unquote(metalevel: Metalevel) extends Token

  implicit def classifiable[T <: Token]: Classifiable[T] = null
  implicit def showStructure[T <: Token](implicit options: Options): Structure[T] = TokenStructure.apply[T](options)
  implicit def showSyntax[T <: Token](implicit dialect: Dialect, options: Options): Syntax[T] = TokenSyntax.apply[T](dialect, options)
}

// NOTE: We have this unrelated code here, because of how materializeAdt works.
// TODO: Revisit this since we now have split everything into separate projects.
//
// Due to an unfortunate limitation of knownDirectSubclasses,
// all macro applications that depend on that API
// must come after the classes that they call the API on.
//
// That's why if we put TokenLiftables elsewhere, we might run into troubles
// depending on how the file and its enclosing directories are called.
// To combat that, we have TokenLiftables right here, guaranteeing that there won't be problems
// if someone wants to refactor/rename something later.
private[meta] trait TokenLiftables extends AdtLiftables {
  val c: scala.reflect.macros.blackbox.Context
  override lazy val u: c.universe.type = c.universe

  import c.universe._
  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  implicit lazy val liftInput: Liftable[Input] = Liftable[Input] { input =>
    q"_root_.scala.meta.inputs.Input.String(${new String(input.chars)})"
  }

  implicit lazy val liftBigInt: Liftable[BigInt] = Liftable[BigInt] { v =>
    q"_root_.scala.math.BigInt(${v.bigInteger.toString})"
  }

  implicit lazy val liftBigDecimal: Liftable[BigDecimal] = Liftable[BigDecimal] { v =>
    q"_root_.scala.math.BigDecimal(${v.bigDecimal.toString})"
  }

  // TODO: this can't be `implicit val`, because then the materialization macro will crash in GenICode
  implicit def liftToken: Liftable[Token] = materializeAdt[Token]
}
