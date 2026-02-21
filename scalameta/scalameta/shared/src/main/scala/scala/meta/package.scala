package scala

import scala.reflect.ClassTag

package object meta
    extends classifiers.Api
    with parsers.Api
    with prettyprinters.Api
    with tokenizers.Api
    with tokens.Api
    with trees.Api
    with VersionSpecificApis {

  /*
   * aliases
   */

  // inputs

  type Input = inputs.Input
  val Input = inputs.Input

  type Position = inputs.Position
  val Position = inputs.Position

  // io

  type AbsolutePath = io.AbsolutePath
  val AbsolutePath = io.AbsolutePath

  type RelativePath = io.RelativePath
  val RelativePath = io.RelativePath

  type Classpath = io.Classpath
  val Classpath = io.Classpath

  // parsers

  type Parsed[+T] = parsers.Parsed[T]
  val Parsed = parsers.Parsed

  type ParseException = parsers.ParseException
  val ParseException = parsers.ParseException

  // prettyprinters

  type Structure[T] = prettyprinters.Structure[T]
  type Syntax[T] = prettyprinters.Syntax[T]

  // quasiquotes

  type Lift[O, I] = quasiquotes.Lift[O, I]
  val Lift = quasiquotes.Lift

  type Unlift[I, O] = quasiquotes.Unlift[I, O]
  val Unlift = quasiquotes.Unlift

  // tokenizers

  type Tokenized = tokenizers.Tokenized
  val Tokenized = tokenizers.Tokenized

  type TokenizeException = tokenizers.TokenizeException
  val TokenizeException = tokenizers.TokenizeException

  // tokens

  type Token = tokens.Token
  val Token = tokens.Token

  type Tokens = tokens.Tokens
  val Tokens = tokens.Tokens

  /*
   * extensions
   */

  implicit class XtensionDialectApply(private val dialect: Dialect) extends AnyVal {
    def apply[T](value: T)(implicit
        convert: common.Convert[T, inputs.Input]
    ): (Dialect, inputs.Input) = (dialect, convert(value))
    def apply(value: tokens.Token): (Dialect, tokens.Token) = (dialect, value)
    def apply(value: tokens.Tokens): (Dialect, tokens.Tokens) = (dialect, value)
    def apply(value: Tree): (Dialect, Tree) = (dialect, value)
  }
  implicit class XtensionDialectTokenSyntax(private val dialectToken: (Dialect, tokens.Token))
      extends AnyVal {
    def syntax: String = {
      implicit val (dialect, token) = dialectToken
      token.syntax
    }
  }
  implicit class XtensionDialectTokensSyntax(private val dialectTokens: (Dialect, tokens.Tokens))
      extends AnyVal {
    def syntax: String = {
      implicit val (dialect, tokens) = dialectTokens
      tokens.syntax
    }
    def tokenize(implicit tokenize: tokenizers.Tokenize): tokenizers.Tokenized = {
      val (dialect, tokens) = dialectTokens
      val input = Tokens.tokensToInput(tokens)
      tokenize.apply(input, dialect)
    }
    def parse[U](implicit parse: parsers.Parse[U]): parsers.Parsed[U] = {
      val (dialect, tokens) = dialectTokens
      val input = Tokens.tokensToInput(tokens)
      parse.apply(input, dialect)
    }
  }
  implicit class XtensionDialectTreeSyntax(private val dialectTree: (Dialect, Tree))
      extends AnyVal {
    def syntax: String = {
      implicit val (dialect, tree) = dialectTree
      tree.syntax
    }
  }

  implicit class XtensionTree(private val tree: Tree) extends AnyVal {
    def maybeParseAs[A <: Tree: ClassTag](implicit
        dialect: Dialect,
        parse: parsers.Parse[A]
    ): Parsed[A] = tree match {
      case t: A => t.maybeParse
      case _ => reparseAs[A]
    }

    def reparseAs[A <: Tree](implicit dialect: Dialect, parse: parsers.Parse[A]): Parsed[A] =
      parse(tree.textAsInput, dialect)
  }

  implicit class XtensionTreeT[A <: Tree](private val tree: A) extends AnyVal {
    def maybeParse(implicit dialect: Dialect, parse: parsers.Parse[A]): Parsed[A] =
      tree.origin match {
        case o: trees.Origin.ParsedPartial if o.dialect.isEquivalentTo(dialect) =>
          Parsed.Success(tree)
        case _ => tree.reparseAs[A]
      }
  }

}
