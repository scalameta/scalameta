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
  @token class Ident(start: Int, end: Int) extends Token { def name = "identifier" }

  @token class Literal(start: Int, end: Int, constant: Constant) extends Token { def name = constant.name + " literal" }

  object Interpolation {
    @token class Id(start: Int, end: Int) extends Token { def name = "interpolation id" }
    @token class Start(start: Int, end: Int) extends Token { def name = "interpolation start" }
    @token class Part(start: Int, end: Int, value: Predef.String) extends Token { def name = "interpolation part" }
    @token class SpliceStart(start: Int) extends Token { def name = "splice start"; override def code = "$" }
    @token class SpliceEnd(start: Int) extends Token { def name = "splice end"; override def code = "" }
    @token class End(start: Int, end: Int) extends Token { def name = "interpolation end" }
  }

  object Xml {
    @token class Start(start: Int) extends Token { def name = "xml start"; override def code = "" }
    @token class Part(start: Int, end: Int, value: Predef.String) extends Token { def name = "xml part" }
    @token class SpliceStart(start: Int) extends Token { def name = "xml splice start"; override def code = "" }
    @token class SpliceEnd(start: Int) extends Token { def name = "xml splice end"; override def code = "" }
    @token class End(start: Int) extends Token { def name = "xml end"; override def code = "" }
  }

  @token class `case`(start: Int) extends Token
  @token class `catch`(start: Int) extends Token
  @token class `class `(start: Int) extends Token
  @token class `def`(start: Int) extends Token
  @token class `do`(start: Int) extends Token
  @token class `else`(start: Int) extends Token
  @token class `extends`(start: Int) extends Token
  @token class `finally`(start: Int) extends Token
  @token class `for`(start: Int) extends Token
  @token class `forSome`(start: Int) extends Token
  @token class `if`(start: Int) extends Token
  @token class `import`(start: Int) extends Token
  @token class `match`(start: Int) extends Token
  @token class `macro`(start: Int) extends Token
  @token class `new`(start: Int) extends Token
  @token class `object`(start: Int) extends Token
  @token class `package `(start: Int) extends Token
  @token class `return`(start: Int) extends Token
  @token class `super`(start: Int) extends Token
  @token class `this`(start: Int) extends Token
  @token class `throw`(start: Int) extends Token
  @token class `trait`(start: Int) extends Token
  @token class `try`(start: Int) extends Token
  @token class `type`(start: Int) extends Token
  @token class `val`(start: Int) extends Token
  @token class `var`(start: Int) extends Token
  @token class `while`(start: Int) extends Token
  @token class `with`(start: Int) extends Token
  @token class `yield`(start: Int) extends Token

  @token class `abstract`(start: Int) extends Token
  @token class `final`(start: Int) extends Token
  @token class `sealed`(start: Int) extends Token
  @token class `implicit`(start: Int) extends Token
  @token class `lazy`(start: Int) extends Token
  @token class `private`(start: Int) extends Token
  @token class `protected`(start: Int) extends Token
  @token class `override`(start: Int) extends Token

  @token class `(`(start: Int) extends Token
  @token class `)`(start: Int) extends Token
  @token class `[`(start: Int) extends Token
  @token class `]`(start: Int) extends Token
  @token class `{`(start: Int) extends Token
  @token class `}`(start: Int) extends Token
  @token class `,`(start: Int) extends Token
  @token class `;`(start: Int) extends Token
  @token class `:`(start: Int) extends Token
  @token class `.`(start: Int) extends Token
  @token class `=`(start: Int) extends Token
  @token class `@`(start: Int) extends Token
  @token class `#`(start: Int) extends Token
  @token class `_ `(start: Int) extends Token
  @token class `=>`(start: Int, end: Int) extends Token { def name = "right arrow" }
  @token class `<-`(start: Int, end: Int) extends Token { def name = "left arrow" }
  @token class `<:`(start: Int) extends Token
  @token class `>:`(start: Int) extends Token
  @token class `<%`(start: Int) extends Token

  @token class ` `(start: Int) extends Token
  @token class `\t`(start: Int) extends Token
  @token class `\r`(start: Int) extends Token
  @token class `\n`(start: Int) extends Token
  // TODO: \n\n is a virtual token emitted by TokIterator to appease the semi-ported scalac parser
  // it will never occur in a token stream produced by XtensionInputLike.tokens
  @token class `\n\n`(start: Int) extends Token
  @token class `\f`(start: Int) extends Token
  @token class Comment(start: Int, end: Int) extends Token { def name = "comment" }

  // NOTE: in order to maintain conceptual compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  // TODO: after we bootstrap, Unquote.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @token class Ellipsis(start: Int, end: Int, rank: Int) extends Token { def name = "ellipsis" }
  @token class Unquote(start: Int, end: Int, tree: Any) extends Token { def name = "unquote" }

  @token class BOF() extends Token {
    def name = "beginning of file"
    override def code = ""
    def start = 0
    def end = 0
  }

  @token class EOF() extends Token {
    def name = "end of file"
    override def code = ""
    def start = content.chars.length
    def end = content.chars.length
  }

  implicit def classifiable[T <: Token]: Classifiable[T] = null
  implicit def showStructure[T <: Token]: Structure[T] = TokenStructure.apply[T]
  implicit def showSyntax[T <: Token]: Syntax[T] = TokenSyntax.apply[T]
}

@adt.root trait Constant {
  def name: String
  def value: Any
}
object Constant {
  @adt.leaf class Int(value: scala.BigInt) extends Constant { def name = "integer" }
  @adt.leaf class Long(value: scala.BigInt) extends Constant { def name = "long" }
  @adt.leaf class Float(value: scala.BigDecimal) extends Constant { def name = "float" }
  @adt.leaf class Double(value: scala.BigDecimal) extends Constant { def name = "double" }
  @adt.leaf class Char(value: scala.Char) extends Constant { def name = "character" }
  @adt.leaf class Symbol(value: scala.Symbol) extends Constant { def name = "symbol" }
  @adt.leaf class String(value: Predef.String) extends Constant { def name = "string" }
  @adt.leaf class Boolean(value: scala.Boolean) extends Constant { def name = "boolean" }
  @adt.leaf object Null extends Constant { def value = null; def name = "null" }
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
