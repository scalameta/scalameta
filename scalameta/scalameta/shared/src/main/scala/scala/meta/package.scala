package scala

import scala.reflect.ClassTag

package object meta
    extends classifiers.Api
    with classifiers.Aliases
    with dialects.Api
    with dialects.Aliases
    with parsers.Api
    with parsers.Aliases
    with prettyprinters.Api
    with prettyprinters.Aliases
    with quasiquotes.Api
    with quasiquotes.Aliases
    with io.Api
    with io.Aliases
    with inputs.Api
    with inputs.Aliases
    with tokenizers.Api
    with tokenizers.Aliases
    with tokens.Api
    with tokens.Aliases
    with transversers.Api
    with transversers.Aliases
    with trees.Api
    with trees.Aliases {

  type XtensionParsersDialectApply // shadow conflicting implicit class
  type XtensionTokenizersDialectApply // shadow conflicting implicit class
  implicit class XtensionDialectApply(private val dialect: scala.meta.Dialect) extends AnyVal {
    def apply[T](inputLike: T)(implicit
        convert: scala.meta.common.Convert[T, scala.meta.inputs.Input]
    ): (scala.meta.Dialect, scala.meta.inputs.Input) = (dialect, convert(inputLike))
    def apply(token: scala.meta.tokens.Token): (scala.meta.Dialect, scala.meta.tokens.Token) =
      (dialect, token)
    def apply(tokens: scala.meta.tokens.Tokens): (scala.meta.Dialect, scala.meta.tokens.Tokens) =
      (dialect, tokens)
    def apply(tree: scala.meta.Tree): (scala.meta.Dialect, scala.meta.Tree) = (dialect, tree)
  }
  implicit class XtensionDialectTokenSyntax(
      private val dialectToken: (scala.meta.Dialect, scala.meta.tokens.Token)
  ) extends AnyVal {
    def syntax: String = {
      implicit val (dialect, token) = dialectToken
      token.syntax
    }
  }
  implicit class XtensionDialectTokensSyntax(
      private val dialectTokens: (scala.meta.Dialect, scala.meta.tokens.Tokens)
  ) extends AnyVal {
    def syntax: String = {
      implicit val (dialect, tokens) = dialectTokens
      tokens.syntax
    }
    def tokenize(implicit
        tokenize: scala.meta.tokenizers.Tokenize
    ): scala.meta.tokenizers.Tokenized = {
      val (dialect, tokens) = dialectTokens
      val input = Tokens.tokensToInput(tokens)
      tokenize.apply(input, dialect)
    }
    def parse[U](implicit parse: scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U] = {
      val (dialect, tokens) = dialectTokens
      val input = Tokens.tokensToInput(tokens)
      parse.apply(input, dialect)
    }
  }
  implicit class XtensionDialectTreeSyntax(
      private val dialectTree: (scala.meta.Dialect, scala.meta.Tree)
  ) extends AnyVal {
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
        case o: trees.Origin.Parsed if o.dialect.isEquivalentTo(dialect) => Parsed.Success(tree)
        case _ => tree.reparseAs[A]
      }
  }
}
