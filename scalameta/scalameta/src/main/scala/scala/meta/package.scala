package scala

package object meta extends classifiers.Api with classifiers.Aliases
                       with dialects.Api with dialects.Aliases
                       with parsers.Api with parsers.Aliases
                       with prettyprinters.Api with prettyprinters.Aliases
                       with quasiquotes.Api with quasiquotes.Aliases
                       with inputs.Api with inputs.Aliases
                       with tokenizers.Api with tokenizers.Aliases
                       with tokens.Api with tokens.Aliases
                       with transversers.Api with transversers.Aliases {

  type XtensionDialectTokenizeInputLike // shadow conflicting implicit class
  type XtensionDialectParseInputLike // shadow conflicting implicit class

  class InputWithDialect(input: scala.meta.inputs.Input, dialect: scala.meta.Dialect) {
    def parse[U](implicit parse: scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U] = {
      new scala.meta.parsers.InputWithDialect(input, dialect).parse[U]
    }
    def tokenize(implicit tokenize: scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized = {
      new scala.meta.tokenizers.InputWithDialect(input, dialect).tokenize
    }
    override def toString = s"$dialect($input)"
  }

  implicit class XtensionDialectApply(dialect: scala.meta.Dialect) {
    def apply[T](inputLike: T)(implicit convert: scala.meta.convert.Convert[T, scala.meta.inputs.Input]): InputWithDialect = {
      val input = convert(inputLike)
      new InputWithDialect(input, dialect)
    }
  }
}
