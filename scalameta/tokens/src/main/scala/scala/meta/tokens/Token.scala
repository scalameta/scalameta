package scala.meta
package tokens

import scala.meta.internal.tokens // NOTE: no underscore import!
import scala.meta.internal.tokens._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

@root trait Token {
  def name: String
  def code: String = new String(content.chars.slice(start, end))

  def content: Content
  def dialect: Dialect
  def start: Int
  def end: Int
  def position: Position = new Position.Range(content, Point.Offset(content, start), Point.Offset(content, start), Point.Offset(content, end))
  def adjust(
    content: Content = this.content,
    dialect: Dialect = this.dialect,
    start: Int = this.start,
    end: Int = this.end,
    delta: Int = 0): Token

  final override def toString = scala.meta.internal.prettyprinters.TokenToString(this)
}

object Token {
  @branch trait Static extends Token
  @branch trait Dynamic extends Token

  @token class Ident(start: Int, end: Int) extends Dynamic { def name = "identifier" }

  @branch trait Literal extends Token
  object Literal {
    @token class Int(start: scala.Int, end: scala.Int, value: scala.BigInt) extends Literal with Dynamic { def name = "integer literal" }
    @token class Long(start: scala.Int, end: scala.Int, value: scala.BigInt) extends Literal with Dynamic { def name = "long literal" }
    @token class Float(start: scala.Int, end: scala.Int, value: scala.BigDecimal) extends Literal with Dynamic { def name = "float literal" }
    @token class Double(start: scala.Int, end: scala.Int, value: scala.BigDecimal) extends Literal with Dynamic { def name = "double literal" }
    @token class Char(start: scala.Int, end: scala.Int, value: scala.Char) extends Literal with Dynamic { def name = "character literal" }
    @token class Symbol(start: scala.Int, end: scala.Int, value: scala.Symbol) extends Literal with Dynamic { def name = "symbol literal" }
    @token class String(start: scala.Int, end: scala.Int, value: Predef.String) extends Literal with Dynamic { def name = "string literal" }
    @token class `null`(start: scala.Int) extends Keyword with Literal with Static
    @token class `true`(start: scala.Int) extends Keyword with Literal with Static
    @token class `false`(start: scala.Int) extends Keyword with Literal with Static
  }

  object Interpolation {
    @token class Id(start: Int, end: Int) extends Dynamic { def name = "interpolation id" }
    @token class Start(start: Int, end: Int) extends Dynamic { def name = "interpolation start" }
    @token class Part(start: Int, end: Int, value: Predef.String) extends Dynamic { def name = "interpolation part" }
    @token class SpliceStart(start: Int) extends Static { def name = "splice start"; override def code = "$" }
    @token class SpliceEnd(start: Int) extends Static { def name = "splice end"; override def code = "" }
    @token class End(start: Int, end: Int) extends Dynamic { def name = "interpolation end" }
  }

  object Xml {
    @token class Start(start: Int) extends Static { def name = "xml start"; override def code = "" }
    @token class Part(start: Int, end: Int, value: Predef.String) extends Dynamic { def name = "xml part" }
    @token class SpliceStart(start: Int) extends Static { def name = "xml splice start"; override def code = "" }
    @token class SpliceEnd(start: Int) extends Static { def name = "xml splice end"; override def code = "" }
    @token class End(start: Int) extends Static { def name = "xml end"; override def code = "" }
  }

  @branch trait Keyword extends Static
  @token class `case`(start: Int) extends Keyword
  @token class `catch`(start: Int) extends Keyword
  @token class `class `(start: Int) extends Keyword
  @token class `def`(start: Int) extends Keyword
  @token class `do`(start: Int) extends Keyword
  @token class `else`(start: Int) extends Keyword
  @token class `extends`(start: Int) extends Keyword
  @token class `finally`(start: Int) extends Keyword
  @token class `for`(start: Int) extends Keyword
  @token class `forSome`(start: Int) extends Keyword
  @token class `if`(start: Int) extends Keyword
  @token class `import`(start: Int) extends Keyword
  @token class `match`(start: Int) extends Keyword
  @token class `macro`(start: Int) extends Keyword
  @token class `new`(start: Int) extends Keyword
  @token class `object`(start: Int) extends Keyword
  @token class `package `(start: Int) extends Keyword
  @token class `return`(start: Int) extends Keyword
  @token class `super`(start: Int) extends Keyword
  @token class `this`(start: Int) extends Keyword
  @token class `throw`(start: Int) extends Keyword
  @token class `trait`(start: Int) extends Keyword
  @token class `try`(start: Int) extends Keyword
  @token class `type`(start: Int) extends Keyword
  @token class `val`(start: Int) extends Keyword
  @token class `var`(start: Int) extends Keyword
  @token class `while`(start: Int) extends Keyword
  @token class `with`(start: Int) extends Keyword
  @token class `yield`(start: Int) extends Keyword

  @branch trait Modifier extends Keyword
  @token class `abstract`(start: Int) extends Modifier
  @token class `final`(start: Int) extends Modifier
  @token class `sealed`(start: Int) extends Modifier
  @token class `implicit`(start: Int) extends Modifier
  @token class `lazy`(start: Int) extends Modifier
  @token class `private`(start: Int) extends Modifier
  @token class `protected`(start: Int) extends Modifier
  @token class `override`(start: Int) extends Modifier

  @branch trait Delim extends Token
  @token class `(`(start: Int) extends Delim with Static
  @token class `)`(start: Int) extends Delim with Static
  @token class `[`(start: Int) extends Delim with Static
  @token class `]`(start: Int) extends Delim with Static
  @token class `{`(start: Int) extends Delim with Static
  @token class `}`(start: Int) extends Delim with Static
  @token class `,`(start: Int) extends Delim with Static
  @token class `;`(start: Int) extends Delim with Static
  @token class `:`(start: Int) extends Delim with Static
  @token class `.`(start: Int) extends Delim with Static
  @token class `=`(start: Int) extends Delim with Static
  @token class `@`(start: Int) extends Delim with Static
  @token class `#`(start: Int) extends Delim with Static
  @token class `_ `(start: Int) extends Delim with Static
  @token class `=>`(start: Int, end: Int) extends Delim with Dynamic { def name = "right arrow" }
  @token class `<-`(start: Int, end: Int) extends Delim with Dynamic { def name = "left arrow" }
  @token class `<:`(start: Int) extends Delim with Static
  @token class `>:`(start: Int) extends Delim with Static
  @token class `<%`(start: Int) extends Delim with Static

  @branch trait Trivia extends Token
  @branch trait Whitespace extends Trivia with Static
  @token class ` `(start: Int) extends Whitespace
  @token class `\t`(start: Int) extends Whitespace
  @token class `\r`(start: Int) extends Whitespace
  @token class `\n`(start: Int) extends Whitespace
  // TODO: \n\n is a virtual token emitted by TokIterator to appease the semi-ported scalac parser
  // it will never occur in a token stream produced by XtensionInputLike.tokens
  @token class `\n\n`(start: Int) extends Whitespace
  @token class `\f`(start: Int) extends Whitespace
  @token class Comment(start: Int, end: Int) extends Trivia with Dynamic { def name = "comment" }

  // NOTE: in order to maintain conceptual compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  // TODO: after we bootstrap, Unquote.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @token class Ellipsis(start: Int, end: Int, rank: Int) extends Dynamic { def name = "ellipsis" }
  @token class Unquote(start: Int, end: Int, tree: Any) extends Dynamic { def name = "unquote" }

  @token class BOF() extends Static {
    def name = "beginning of file"
    override def code = ""
    def start = 0
    def end = 0
  }

  @token class EOF() extends Static {
    def name = "end of file"
    override def code = ""
    def start = content.chars.length
    def end = content.chars.length
  }

  implicit def showStructure[T <: Token]: Structure[T] = TokenStructure.apply[T]
  implicit def showSyntax[T <: Token]: Syntax[T] = TokenSyntax.apply[T]
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
trait TokenLiftables extends tokens.Liftables {
  val c: scala.reflect.macros.blackbox.Context
  override lazy val u: c.universe.type = c.universe

  import c.universe._
  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  implicit lazy val liftContent: Liftable[Content] = Liftable[Content] { content =>
    q"_root_.scala.meta.inputs.Input.String(${new String(content.chars)})"
  }

  implicit lazy val liftDialect: Liftable[Dialect] = Liftable[Dialect] { dialect =>
    q"_root_.scala.meta.Dialect.forName(${dialect.name})"
  }

  implicit lazy val liftBigInt: Liftable[BigInt] = Liftable[BigInt] { v =>
    q"_root_.scala.math.BigInt(${v.bigInteger.toString})"
  }

  implicit lazy val liftBigDecimal: Liftable[BigDecimal] = Liftable[BigDecimal] { v =>
    q"_root_.scala.math.BigDecimal(${v.bigDecimal.toString})"
  }

  lazy implicit val liftUnquote: Liftable[Token.Unquote] = Liftable[Token.Unquote] { u =>
    c.abort(c.macroApplication.pos, "fatal error: this shouldn't have happened")
  }

  // TODO: this can't be `implicit val`, because then the materialization macro will crash in GenICode
  implicit def liftToken: Liftable[Token] = materializeToken[Token]

  implicit lazy val liftTokens: Liftable[Tokens] = Liftable[Tokens] { tokens =>
    def prepend(tokens: Tokens, t: Tree): Tree =
      (tokens foldRight t) { case (token, acc) => q"$token +: $acc" }

    def append(t: Tree, tokens: Tokens): Tree =
      // We call insert tokens again because there may be things that need to be spliced in it
      q"$t ++ ${insertTokens(tokens)}"

    def insertTokens(tokens: Tokens): Tree = {
      val (pre, middle) = tokens span (!_.isInstanceOf[Token.Unquote])
      middle match {
        case Tokens() =>
          prepend(pre, q"_root_.scala.meta.tokens.Tokens()")
        case Token.Unquote(_, _, _, _, tree: Tree) +: rest =>
          // If we are splicing only a single token we need to wrap it in a Tokens
          // to be able to append and prepend other tokens to it easily.
          val quoted = if (tree.tpe <:< typeOf[Token]) q"_root_.scala.meta.tokens.Tokens($tree)" else tree
          append(prepend(pre, quoted), Tokens(rest: _*))
      }
    }

    insertTokens(tokens)
  }
}
