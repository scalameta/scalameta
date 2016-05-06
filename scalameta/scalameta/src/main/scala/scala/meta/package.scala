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

  type XtensionParsersDialectApply // shadow conflicting implicit class
  type XtensionTokenizersDialectApply // shadow conflicting implicit class
  implicit class XtensionDialectApply(dialect: scala.meta.Dialect) {
    def apply[T](inputLike: T)(implicit convert: scala.meta.common.Convert[T, scala.meta.inputs.Input]): (scala.meta.Dialect, scala.meta.inputs.Input) = {
      (dialect, convert(inputLike))
    }
  }
}
