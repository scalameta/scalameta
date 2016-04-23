package scala.meta
package tokens

import org.scalameta.adt
import org.scalameta.adt.{Liftables => AdtLiftables}
import scala.meta.internal.tokens
import scala.meta.internal.tokens._
import scala.meta.inputs._
import scala.meta.classifiers._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Token.end can point to the last character plus one.
// Btw, Token.start can also point to the last character plus one if it's an EOF token.
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

  def is[U](implicit classifier: Classifier[Token, U]): Boolean = classifier.apply(this)
  def isNot[U](implicit classifier: Classifier[Token, U]): Boolean = !this.is[U]

  final override def toString = scala.meta.internal.prettyprinters.TokenToString(this)
}

object Token {
  // Identifiers
  @freeform("identifier") class Ident(value: String) extends Token

  // Keywords
  @fixed("abstract") class `abstract` extends Token
  @fixed("case") class `case` extends Token
  @fixed("catch") class `catch` extends Token
  @fixed("class") class `class ` extends Token
  @fixed("def") class `def` extends Token
  @fixed("do") class `do` extends Token
  @fixed("else") class `else` extends Token
  @fixed("extends") class `extends` extends Token
  @fixed("final") class `final` extends Token
  @fixed("finally") class `finally` extends Token
  @fixed("for") class `for` extends Token
  @fixed("forSome") class `forSome` extends Token
  @fixed("if") class `if` extends Token
  @fixed("implicit") class `implicit` extends Token
  @fixed("import") class `import` extends Token
  @fixed("lazy") class `lazy` extends Token
  @fixed("match") class `match` extends Token
  @fixed("macro") class `macro` extends Token
  @fixed("new") class `new` extends Token
  @fixed("object") class `object` extends Token
  @fixed("override") class `override` extends Token
  @fixed("package") class `package ` extends Token
  @fixed("private") class `private` extends Token
  @fixed("protected") class `protected` extends Token
  @fixed("return") class `return` extends Token
  @fixed("sealed") class `sealed` extends Token
  @fixed("super") class `super` extends Token
  @fixed("this") class `this` extends Token
  @fixed("throw") class `throw` extends Token
  @fixed("trait") class `trait` extends Token
  @fixed("try") class `try` extends Token
  @fixed("type") class `type` extends Token
  @fixed("val") class `val` extends Token
  @fixed("var") class `var` extends Token
  @fixed("while") class `while` extends Token
  @fixed("with") class `with` extends Token
  @fixed("yield") class `yield` extends Token
  @fixed("#") class `#` extends Token
  @fixed(":") class `:` extends Token
  @fixed("<%") class `<%` extends Token
  @freeform("<-") class `<-` extends Token
  @fixed("<:") class `<:` extends Token
  @fixed("=") class `=` extends Token
  @freeform("=>") class `=>` extends Token
  @fixed(">:") class `>:` extends Token
  @fixed("@") class `@` extends Token
  @fixed("_") class `_ ` extends Token

  // Delimiters
  @fixed("(") class `(` extends Token
  @fixed(")") class `)` extends Token
  @fixed(",") class `,` extends Token
  @fixed(".") class `.` extends Token
  @fixed(";") class `;` extends Token
  @fixed("[") class `[` extends Token
  @fixed("]") class `]` extends Token
  @fixed("{") class `{` extends Token
  @fixed("}") class `}` extends Token

  // Literals
  @freeform("literal") class Literal(constant: Constant) extends Token
  object Interpolation {
    @freeform("interpolation id") class Id extends Token
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
  @fixed(" ") class ` ` extends Token
  @fixed("\t") class `\t` extends Token
  @fixed("\r") class `\r` extends Token
  @fixed("\n") class `\n` extends Token
  @fixed("\f") class `\f` extends Token
  @freeform("comment") class Comment extends Token
  @freeform("beginning of file") class BOF extends Token { def start = 0; def end = 0 }
  @freeform("end of file") class EOF extends Token { def start = content.chars.length; def end = content.chars.length }

  // TODO: Rewrite the parser so that it doesn't need `\n\n` anymore.
  // NOTE: in order to maintain conceptual compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  // TODO: after we bootstrap, Unquote.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @freeform("\n\n") private[meta] class `\n\n` extends Token
  @freeform("ellipsis") private[meta] class Ellipsis(rank: Int) extends Token
  @freeform("unquote") private[meta] class Unquote(tree: Any) extends Token

  implicit def classifiable[T <: Token]: Classifiable[T] = null
  implicit def showStructure[T <: Token]: Structure[T] = TokenStructure.apply[T]
  implicit def showSyntax[T <: Token]: Syntax[T] = TokenSyntax.apply[T]
}

@adt.root trait Constant { def value: Any }
object Constant {
  @adt.leaf class Int(value: scala.BigInt) extends Constant
  @adt.leaf class Long(value: scala.BigInt) extends Constant
  @adt.leaf class Float(value: scala.BigDecimal) extends Constant
  @adt.leaf class Double(value: scala.BigDecimal) extends Constant
  @adt.leaf class Char(value: scala.Char) extends Constant
  @adt.leaf class Symbol(value: scala.Symbol) extends Constant
  @adt.leaf class String(value: Predef.String) extends Constant
  @adt.leaf class Boolean(value: scala.Boolean) extends Constant
  @adt.leaf object Null extends Constant { def value = null }
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
trait TokenLiftables extends AdtLiftables with tokens.Liftables {
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

  implicit lazy val liftConstant: Liftable[scala.meta.tokens.Constant] = materializeAdt[scala.meta.tokens.Constant]

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
        case Token.Unquote(tree: Tree) +: rest =>
          // If we are splicing only a single token we need to wrap it in a Tokens
          // to be able to append and prepend other tokens to it easily.
          val quoted = if (tree.tpe <:< typeOf[Token]) q"_root_.scala.meta.tokens.Tokens($tree)" else tree
          append(prepend(pre, quoted), Tokens(rest: _*))
      }
    }

    insertTokens(tokens)
  }
}
