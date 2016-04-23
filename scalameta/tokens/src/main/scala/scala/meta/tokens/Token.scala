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
  @token("identifier") class Ident(start: Int, end: Int) extends Token

  @token("literal") class Literal(start: Int, end: Int, constant: Constant) extends Token

  object Interpolation {
    @token("interpolation id") class Id(start: Int, end: Int) extends Token
    @token("interpolation start") class Start(start: Int, end: Int) extends Token
    @token("interpolation part") class Part(start: Int, end: Int, value: String) extends Token
    @token("splice start") class SpliceStart(start: Int, end: Int) extends Token
    @token("splice end") class SpliceEnd(start: Int, end: Int) extends Token
    @token("interpolation end") class End(start: Int, end: Int) extends Token
  }

  object Xml {
    @token("xml start") class Start(start: Int, end: Int) extends Token
    @token("xml part") class Part(start: Int, end: Int, value: String) extends Token
    @token("xml splice start") class SpliceStart(start: Int, end: Int) extends Token
    @token("xml splice end") class SpliceEnd(start: Int, end: Int) extends Token
    @token("xml end") class End(start: Int, end: Int) extends Token
  }

  @token("case") class `case`(start: Int) extends Token
  @token("catch") class `catch`(start: Int) extends Token
  @token("class") class `class `(start: Int) extends Token
  @token("def") class `def`(start: Int) extends Token
  @token("do") class `do`(start: Int) extends Token
  @token("else") class `else`(start: Int) extends Token
  @token("extends") class `extends`(start: Int) extends Token
  @token("finally") class `finally`(start: Int) extends Token
  @token("for") class `for`(start: Int) extends Token
  @token("forSome") class `forSome`(start: Int) extends Token
  @token("if") class `if`(start: Int) extends Token
  @token("import") class `import`(start: Int) extends Token
  @token("match") class `match`(start: Int) extends Token
  @token("macro") class `macro`(start: Int) extends Token
  @token("new") class `new`(start: Int) extends Token
  @token("object") class `object`(start: Int) extends Token
  @token("package") class `package `(start: Int) extends Token
  @token("return") class `return`(start: Int) extends Token
  @token("super") class `super`(start: Int) extends Token
  @token("this") class `this`(start: Int) extends Token
  @token("throw") class `throw`(start: Int) extends Token
  @token("trait") class `trait`(start: Int) extends Token
  @token("try") class `try`(start: Int) extends Token
  @token("type") class `type`(start: Int) extends Token
  @token("val") class `val`(start: Int) extends Token
  @token("var") class `var`(start: Int) extends Token
  @token("while") class `while`(start: Int) extends Token
  @token("with") class `with`(start: Int) extends Token
  @token("yield") class `yield`(start: Int) extends Token

  @token("abstract") class `abstract`(start: Int) extends Token
  @token("final") class `final`(start: Int) extends Token
  @token("sealed") class `sealed`(start: Int) extends Token
  @token("implicit") class `implicit`(start: Int) extends Token
  @token("lazy") class `lazy`(start: Int) extends Token
  @token("private") class `private`(start: Int) extends Token
  @token("protected") class `protected`(start: Int) extends Token
  @token("override") class `override`(start: Int) extends Token

  @token("(") class `(`(start: Int) extends Token
  @token(")") class `)`(start: Int) extends Token
  @token("[") class `[`(start: Int) extends Token
  @token("]") class `]`(start: Int) extends Token
  @token("{") class `{`(start: Int) extends Token
  @token("}") class `}`(start: Int) extends Token
  @token(",") class `,`(start: Int) extends Token
  @token(";") class `;`(start: Int) extends Token
  @token(":") class `:`(start: Int) extends Token
  @token(".") class `.`(start: Int) extends Token
  @token("=") class `=`(start: Int) extends Token
  @token("@") class `@`(start: Int) extends Token
  @token("#") class `#`(start: Int) extends Token
  @token("_") class `_ `(start: Int) extends Token
  @token("=>") class `=>`(start: Int, end: Int) extends Token
  @token("<-") class `<-`(start: Int, end: Int) extends Token
  @token("<:") class `<:`(start: Int) extends Token
  @token(">:") class `>:`(start: Int) extends Token
  @token("<%") class `<%`(start: Int) extends Token

  @token(" ") class ` `(start: Int) extends Token
  @token("\t") class `\t`(start: Int) extends Token
  @token("\r") class `\r`(start: Int) extends Token
  @token("\n") class `\n`(start: Int) extends Token
  @token("\n\n") private[meta] class `\n\n`(start: Int) extends Token // TODO: rewrite the parser so that it doesn't need this
  @token("\f") class `\f`(start: Int) extends Token
  @token("comment") class Comment(start: Int, end: Int) extends Token

  // NOTE: in order to maintain conceptual compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  // TODO: after we bootstrap, Unquote.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @token("ellipsis") class Ellipsis(start: Int, end: Int, rank: Int) extends Token
  @token("unquote") class Unquote(start: Int, end: Int, tree: Any) extends Token

  @token("beginning of file") class BOF() extends Token { def start = 0; def end = 0 }
  @token("end of file") class EOF() extends Token { def start = content.chars.length; def end = content.chars.length }

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
