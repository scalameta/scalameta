package scala.meta.internal.parsers

import scala.meta.Dialect

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

  object StarAsTypePlaceholder
      extends AsWithFunc(
        dialect.allowStarAsTypePlaceholder,
        { (x: String) =>
          val last = x.length - 1
          if (last >= 0 && x.charAt(last) == '*') Some(x.substring(0, last)) else None
        }
      )

}
