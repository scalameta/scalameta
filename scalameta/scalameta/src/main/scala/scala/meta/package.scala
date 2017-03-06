package scala

package object meta extends classifiers.Api with classifiers.Aliases
                       with dialects.Api with dialects.Aliases
                       with parsers.Api with parsers.Aliases
                       with prettyprinters.Api with prettyprinters.Aliases
                       with quasiquotes.Api with quasiquotes.Aliases
                       with inline.Api with inline.Aliases
                       with inputs.Api with inputs.Aliases
                       with semantic.Api with semantic.Aliases
                       with tokenizers.Api with tokenizers.Aliases
                       with tokens.Api with tokens.Aliases
                       with transversers.Api with transversers.Aliases {

  // Note. This is an optimization. We inline this implicit class in this package
  // in order to make it extend AnyVal, and thus making it a value class.
  // JMH  benchmarks show this optimization yields a ~30% speedup in code that
  // uses `.is` a lot.
  final implicit class XtensionClassifiable[T](val x: T) extends AnyVal {
    def is[U](implicit classifier: classifiers.Classifier[T, U]): Boolean = {
      classifier.apply(x)
    }
    def isNot[U](implicit classifier: classifiers.Classifier[T, U]): Boolean = {
      !this.is(classifier)
    }
  }

  // TODO: The necessity of scalameta/package.scala being non-empty is unsatisfying.
  // We seriously need to come up with a better way of achieving similar functionality.
  type XtensionParsersDialectApply // shadow conflicting implicit class
  type XtensionTokenizersDialectApply // shadow conflicting implicit class
  implicit class XtensionDialectApply(dialect: scala.meta.Dialect) {
    def apply[T](inputLike: T)(implicit convert: scala.meta.common.Convert[T, scala.meta.inputs.Input]): (scala.meta.Dialect, scala.meta.inputs.Input) = {
      (dialect, convert(inputLike))
    }
    def apply(token: scala.meta.tokens.Token): (scala.meta.Dialect, scala.meta.tokens.Token) = {
      (dialect, token)
    }
    def apply(tokens: scala.meta.tokens.Tokens): (scala.meta.Dialect, scala.meta.tokens.Tokens) = {
      (dialect, tokens)
    }
    def apply(tree: scala.meta.Tree): (scala.meta.Dialect, scala.meta.Tree) = {
      (dialect, tree)
    }
  }
  implicit class XtensionDialectTokenSyntax(dialectToken: (scala.meta.Dialect, scala.meta.tokens.Token)) {
    def syntax(implicit options: scala.meta.prettyprinters.Options): String = {
      implicit val (dialect, token) = dialectToken
      token.syntax
    }
  }
  implicit class XtensionDialectTokensSyntax(dialectTokens: (scala.meta.Dialect, scala.meta.tokens.Tokens)) {
    def syntax(implicit options: scala.meta.prettyprinters.Options): String = {
      implicit val (dialect, tokens) = dialectTokens
      tokens.syntax
    }
    def tokenize(implicit tokenize: scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized = {
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
  implicit class XtensionDialectTreeSyntax(dialectTree: (scala.meta.Dialect, scala.meta.Tree)) {
    def syntax(implicit options: scala.meta.prettyprinters.Options): String = {
      implicit val (dialect, tree) = dialectTree
      tree.syntax
    }
  }
}
