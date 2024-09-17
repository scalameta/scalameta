package scala.meta.internal.parsers

import scala.meta.Dialect
import scala.meta.Mod

class SoftKeywords(dialect: Dialect) {

  import Keywords._

  object KwAs extends IsWithName(dialect.allowAsForImportRename, "as")
  object KwUsing extends IsWithName(dialect.allowGivenUsing, "using")
  object KwInline extends IsWithName(dialect.allowInlineMods, "inline")
  object KwOpaque extends IsWithName(dialect.allowOpaqueTypes, "opaque")
  object KwOpen extends IsWithName(dialect.allowOpenClass, "open")
  object KwTransparent extends IsWithName(dialect.allowInlineMods, "transparent")
  object KwDerives extends IsWithName(dialect.allowDerives, "derives")
  object KwEnd extends IsWithName(dialect.allowEndMarker, "end")
  object KwInfix extends IsWithName(dialect.allowInfixMods, "infix")
  object KwExtension extends IsWithName(dialect.allowExtensionMethods, "extension")
  object KwErased extends IsWithName(dialect.allowErasedDefs, "erased")
  object KwTracked extends IsWithName(dialect.allowTrackedParameters, "tracked")

  object StarSplice extends IsWithName(dialect.allowPostfixStarVarargSplices, "*")
  object StarAsTypePlaceholder
      extends AsWithFunc(
        dialect.allowStarAsTypePlaceholder,
        {
          case "*" => Some(None)
          case "+*" => Some(Some(Mod.Covariant()))
          case "-*" => Some(Some(Mod.Contravariant()))
          case _ => None
        }
      )
  object QuestionMarkAsTypeWildcard extends IsWithName(dialect.allowQuestionMarkAsTypeWildcard, "?")

}
