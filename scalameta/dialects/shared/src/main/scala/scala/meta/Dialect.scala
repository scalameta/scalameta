package scala.meta

import scala.meta.Dialect.UnquoteType
import scala.meta.dialects._
import scala.meta.internal.dialects._

/**
 * A dialect is used to configure what Scala syntax is allowed during tokenization and parsing.
 */
final class Dialect private[meta] (
    private[meta] val unquoteParentDialect: Dialect,
    // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
    // If yes, they will be parsed as patterns or terms.
    private[meta] val unquoteType: UnquoteType,
    // Are `&` intersection types supported by this dialect?
    @deprecated("allowAndTypes unneeded, infix types are supported", "4.5.1")
    val allowAndTypes: Boolean,
    // Are extractor varargs specified using ats, i.e. is `case Extractor(xs @ _*)` legal or not?
    val allowAtForExtractorVarargs: Boolean,
    // Can case classes be declared without a parameter list?
    // Deprecated in 2.10, not supported in 2.11 and newer.
    val allowCaseClassWithoutParameterList: Boolean,
    // Are extractor varargs specified using colons, i.e. is `case Extractor(xs: _*)` legal or not?
    val allowColonForExtractorVarargs: Boolean,
    // Are enums allowed?
    // They are in Dotty, but not in Scala 2.13 or older.
    val allowEnums: Boolean,
    // Are implicit by name parameters supported?
    // They are in Dotty, but not in Scala 2.12 or older.
    val allowImplicitByNameParameters: Boolean,
    // Are `inline` identifiers supported by this dialect?
    val allowInlineIdents: Boolean,
    // Are inline vals and defs supported by this dialect?
    val allowInlineMods: Boolean,
    // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
    val allowLiteralTypes: Boolean,
    // Are `|` (union types) supported by this dialect?
    @deprecated("allowOrTypes unneeded, infix types are supported", "4.5.1")
    val allowOrTypes: Boolean,
    // Are naked underscores allowed after $ in pattern interpolators, i.e. is `case q"$_ + $_" =>` legal or not?
    val allowSpliceUnderscores: Boolean,
    // Are terms on the top level supported by this dialect?
    // Necessary to support popular script-like DSLs.
    val allowToplevelTerms: Boolean,
    // Are trailing commas allowed? SIP-27.
    val allowTrailingCommas: Boolean,
    // Are trait allowed to have parameters?
    // They are in Dotty, but not in Scala 2.13 or older.
    val allowTraitParameters: Boolean,
    // Are type lambdas allowed, i.e. is `[T] => (T, T)` legal or not?
    val allowTypeLambdas: Boolean,
    // Are view bounds supported by this dialect?
    // def f[A <% Int](a: A)
    // Removed in Dotty.
    val allowViewBounds: Boolean,
    // Are XML literals supported by this dialect?
    // We plan to deprecate XML literal syntax, and some dialects
    // might go ahead and drop support completely.
    val allowXmlLiterals: Boolean,
    @deprecated("toplevelSeparator has never been used", ">4.4.35")
    val toplevelSeparator: String,
    // Are numeric literal underscore separators, i.e. `1_000_000` legal or not?
    val allowNumericLiteralUnderscoreSeparators: Boolean,
    // Can try body contain any expression? (2.13.1 https://github.com/scala/scala/pull/8071)
    val allowTryWithAnyExpr: Boolean,
    // Given/using introduced in dotty
    val allowGivenUsing: Boolean,
    // https://dotty.epfl.ch/docs/reference/experimental/erased-defs.html
    val allowErasedDefs: Boolean,
    // Extension methods introduced in dotty
    val allowExtensionMethods: Boolean,
    // Open modifier for classes introduced in dotty
    val allowOpenClass: Boolean,
    // Top level statements introduced in dotty.
    // differs from ToplevelTerms because here you can define packages
    val allowToplevelStatements: Boolean,
    // Opaque types introduced in dotty
    val allowOpaqueTypes: Boolean,
    // Export selected members of an object introduced in dotty
    val allowExportClause: Boolean,
    // Extended classes separated by ',' introduced in dotty
    val allowCommaSeparatedExtend: Boolean,
    // end marker introduced in dotty
    val allowEndMarker: Boolean,
    // Support for escaping `"` in interpolated strings using $ - "$""
    val allowInterpolationDolarQuoteEscape: Boolean,
    // Significant identation introduced in dotty
    val allowSignificantIndentation: Boolean,
    // Dotty changed wildcard for types from `_` to `?`
    val allowQuestionMarkAsTypeWildcard: Boolean,
    // Dotty rejects placeholder as Type parameter
    val allowTypeParamUnderscore: Boolean,
    // Dotty allows by-name repeated parameters
    val allowByNameRepeatedParameters: Boolean,
    // Dotty allows lazy val abstract values
    val allowLazyValAbstractValues: Boolean,
    // Dotty allows capital pattern vars in `case A @ _ =>`
    val allowUpperCasePatternVarBinding: Boolean,
    // Dotty allows to use derives to automatically generate given instances for type classes
    val allowDerives: Boolean,
    // Dotty allows to specify `type T` inside blocks
    val allowTypeInBlock: Boolean,
    // Dotty allows to define function like `[T] => (ts: List[T]) => ts.headOption`
    val allowPolymorphicFunctions: Boolean,
    // Dotty allows `.match` expressions and chaining matches
    val allowMatchAsOperator: Boolean,
    // Dotty allows `match` on type
    val allowTypeMatch: Boolean,
    // Dotty allows to define types and methods with an `infix` soft keyword modifier
    val allowInfixMods: Boolean,
    // Scala 3 splices/quotes
    val allowSpliceAndQuote: Boolean,
    // Scala 3 disallowed symbol literals
    val allowSymbolLiterals: Boolean,
    // Scala 3 disallowed symbol literals
    val allowDependentFunctionTypes: Boolean,
    /* Scala 3 added possibility to use simpler splices such as:
     * val arr = Array(1, 2, 3)
     * val lst = List(0, arr*)                  // vararg splice argument
     *  lst match
     *    case List(0, 1, xs*) => println(xs)   // binds xs to Seq(2, 3)
     *    case List(1, _*) =>                   // wildcard pattern
     */
    val allowPostfixStarVarargSplices: Boolean,
    /* Scala 3 allows us to specify:
     * `case tp @ OrNull(tp1): OrType`
     * the last section after : was not allowed previously.
     */
    val allowAllTypedPatterns: Boolean,
    // Scala 3 import renames can use as soft keyword `import a.b.C as D`
    val allowAsForImportRename: Boolean,
    // Scala 3 wildcard imports can be specified as `import a.b.*`
    val allowStarWildcardImport: Boolean,
    // Scala 3 no longer allows def hello(){} - `=` is always needed
    val allowProcedureSyntax: Boolean,
    // Scala 3 no longer allows `do {...} while(...)`
    val allowDoWhile: Boolean,
    /* Kind-project support
     * works under -Xsource3 flag
     * https://github.com/scala/scala/pull/9605
     */
    val allowPlusMinusUnderscoreAsIdent: Boolean,
    // Dotty uses `_` for placeholder for types since 3.2
    val allowUnderscoreAsTypePlaceholder: Boolean,
    // Dotty uses `*` for placeholder for types in 3.0-3.2
    val allowStarAsTypePlaceholder: Boolean,
    // import a.b.c.{ given, _} used for -X:source3
    val allowGivenImports: Boolean,
    // Scala 3 uses proper precedence rules for infix types, unlike Scala 2
    val useInfixTypePrecedence: Boolean,
    // Scala213Source3 and Scala3 allow infix operator being placed after nl
    val allowInfixOperatorAfterNL: Boolean,
    // Scala 3 allows `def f[X](x: X)[Y](y: Y)`
    val allowParamClauseInterleaving: Boolean,
    /* Scala 3 allows dropping braces for block arguments such as `list.map: a =>`
     * It wasn't available in Scala 3.0 and got introduced later.
     */
    val allowFewerBraces: Boolean
) extends Product with Serializable {

  // NOTE(olafur) checklist for adding a new dialect field in a binary compatible way:
  // - add new field to primary constructor.
  // - add new parameter to `privateCopy()` method.
  // - add new `withX()` method to allow overriding that specific field.
  // - add new `x = FIELD_DEFAULT_VALUE` in the `def this()` constructor below.

  def this(
      allowAndTypes: Boolean,
      allowAtForExtractorVarargs: Boolean,
      allowCaseClassWithoutParameterList: Boolean,
      allowColonForExtractorVarargs: Boolean,
      allowEnums: Boolean,
      allowImplicitByNameParameters: Boolean,
      allowInlineIdents: Boolean,
      allowInlineMods: Boolean,
      allowLiteralTypes: Boolean,
      allowMultilinePrograms: Boolean = true, // unused
      allowOrTypes: Boolean,
      allowPatUnquotes: Boolean = false, // unused
      allowSpliceUnderscores: Boolean,
      allowTermUnquotes: Boolean = false, // unused
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      allowViewBounds: Boolean,
      allowXmlLiterals: Boolean,
      toplevelSeparator: String
  ) = {
    this(
      unquoteParentDialect = null,
      unquoteType = UnquoteType.None,
      allowAndTypes = allowAndTypes,
      allowAtForExtractorVarargs = allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs = allowColonForExtractorVarargs,
      allowEnums = allowEnums,
      allowImplicitByNameParameters = allowImplicitByNameParameters,
      allowInlineIdents = allowInlineIdents,
      allowInlineMods = allowInlineMods,
      allowLiteralTypes = allowLiteralTypes,
      allowOrTypes = allowOrTypes,
      allowSpliceUnderscores = allowSpliceUnderscores,
      allowToplevelTerms = allowToplevelTerms,
      allowTrailingCommas = allowTrailingCommas,
      allowTraitParameters = allowTraitParameters,
      allowTypeLambdas = allowTypeLambdas,
      allowViewBounds = allowViewBounds,
      allowXmlLiterals = allowXmlLiterals,
      toplevelSeparator = toplevelSeparator,
      allowNumericLiteralUnderscoreSeparators = false,
      allowTryWithAnyExpr = false,
      allowGivenUsing = false,
      allowErasedDefs = false,
      allowExtensionMethods = false,
      allowOpenClass = false,
      allowToplevelStatements = false,
      allowOpaqueTypes = false,
      allowExportClause = false,
      allowCommaSeparatedExtend = false,
      allowEndMarker = false,
      allowInterpolationDolarQuoteEscape = false,
      allowSignificantIndentation = false,
      allowQuestionMarkAsTypeWildcard = false,
      allowTypeParamUnderscore = true,
      allowByNameRepeatedParameters = false,
      allowLazyValAbstractValues = false,
      allowUpperCasePatternVarBinding = false,
      allowDerives = false,
      allowTypeInBlock = false,
      allowPolymorphicFunctions = false,
      allowMatchAsOperator = false,
      allowTypeMatch = false,
      allowInfixMods = false,
      allowSpliceAndQuote = false,
      allowSymbolLiterals = true,
      allowDependentFunctionTypes = false,
      allowPostfixStarVarargSplices = false,
      allowAllTypedPatterns = false,
      allowAsForImportRename = false,
      allowStarWildcardImport = false,
      allowProcedureSyntax = true,
      allowDoWhile = true,
      allowPlusMinusUnderscoreAsIdent = false,
      allowUnderscoreAsTypePlaceholder = false,
      allowStarAsTypePlaceholder = false,
      allowGivenImports = false,
      useInfixTypePrecedence = false,
      allowInfixOperatorAfterNL = false,
      allowParamClauseInterleaving = false,
      allowFewerBraces = false
      // NOTE(olafur): declare the default value for new fields above this comment.
    )
  }

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  def allowUnquotes: Boolean = null ne unquoteParentDialect
  def allowPatUnquotes: Boolean = unquoteType.isInstanceOf[UnquoteType.Pat]
  def allowTermUnquotes: Boolean = unquoteType.isInstanceOf[UnquoteType.Term]
  def allowMultilinePrograms: Boolean = unquoteType.isMultiline

  @deprecated("allowAndTypes unneeded, infix types are supported", "4.5.1")
  def withAllowAndTypes(newValue: Boolean): Dialect = {
    privateCopy(allowAndTypes = newValue)
  }
  def withAllowAtForExtractorVarargs(newValue: Boolean): Dialect = {
    privateCopy(allowAtForExtractorVarargs = newValue)
  }
  def withAllowCaseClassWithoutParameterList(
      newValue: Boolean
  ): Dialect = {
    privateCopy(allowCaseClassWithoutParameterList = newValue)
  }
  def withAllowColonForExtractorVarargs(newValue: Boolean): Dialect = {
    privateCopy(allowColonForExtractorVarargs = newValue)
  }
  def withAllowEnums(newValue: Boolean): Dialect = {
    privateCopy(allowEnums = newValue)
  }
  def withAllowImplicitByNameParameters(newValue: Boolean): Dialect = {
    privateCopy(allowImplicitByNameParameters = newValue)
  }
  def withAllowInlineIdents(newValue: Boolean): Dialect = {
    privateCopy(allowInlineIdents = newValue)
  }
  def withAllowInlineMods(newValue: Boolean): Dialect = {
    privateCopy(allowInlineMods = newValue)
  }
  def withAllowLiteralTypes(newValue: Boolean): Dialect = {
    privateCopy(allowLiteralTypes = newValue)
  }
  def withAllowMultilinePrograms(newValue: Boolean): Dialect = ???
  @deprecated("allowOrTypes unneeded, infix types are supported", "4.5.1")
  def withAllowOrTypes(newValue: Boolean): Dialect = {
    privateCopy(allowOrTypes = newValue)
  }
  def withAllowPatUnquotes(newValue: Boolean): Dialect = ???
  def withAllowSpliceUnderscores(newValue: Boolean): Dialect = {
    privateCopy(allowSpliceUnderscores = newValue)
  }
  def withAllowTermUnquotes(newValue: Boolean): Dialect = ???
  def withAllowToplevelTerms(newValue: Boolean): Dialect = {
    privateCopy(allowToplevelTerms = newValue)
  }
  def withAllowTrailingCommas(newValue: Boolean): Dialect = {
    privateCopy(allowTrailingCommas = newValue)
  }
  def withAllowTraitParameters(newValue: Boolean): Dialect = {
    privateCopy(allowTraitParameters = newValue)
  }
  def withAllowTypeLambdas(newValue: Boolean): Dialect = {
    privateCopy(allowTypeLambdas = newValue)
  }
  def withAllowViewBounds(newValue: Boolean): Dialect = {
    privateCopy(allowViewBounds = newValue)
  }
  def withAllowXmlLiterals(newValue: Boolean): Dialect = {
    privateCopy(allowXmlLiterals = newValue)
  }
  @deprecated("toplevelSeparator has never been used", ">4.4.35")
  def withToplevelSeparator(newValue: String): Dialect = {
    privateCopy(toplevelSeparator = newValue)
  }
  def withAllowNumericLiteralUnderscoreSeparators(newValue: Boolean): Dialect = {
    privateCopy(allowNumericLiteralUnderscoreSeparators = newValue)
  }
  def withAllowTryWithAnyExpr(newValue: Boolean): Dialect = {
    privateCopy(allowTryWithAnyExpr = newValue)
  }
  def withAllowGivenUsing(newValue: Boolean): Dialect = {
    privateCopy(allowGivenUsing = newValue)
  }
  def withAllowErasedDefs(newValue: Boolean): Dialect = {
    privateCopy(allowErasedDefs = newValue)
  }
  def withAllowExtensionMethods(newValue: Boolean): Dialect = {
    privateCopy(allowExtensionMethods = newValue)
  }
  def withAllowOpenClass(newValue: Boolean): Dialect = {
    privateCopy(allowOpenClass = newValue)
  }
  def withAllowToplevelStatements(newValue: Boolean): Dialect = {
    privateCopy(allowToplevelStatements = newValue)
  }
  def withAllowOpaqueTypes(newValue: Boolean): Dialect = {
    privateCopy(allowOpaqueTypes = newValue)
  }

  def withAllowInterpolationDolarQuoteEscape(newValue: Boolean): Dialect = {
    privateCopy(allowInterpolationDolarQuoteEscape = newValue)
  }
  def withAllowExportClause(newValue: Boolean): Dialect = {
    privateCopy(allowExportClause = newValue)
  }
  def withAllowCommaSeparatedExtend(newValue: Boolean): Dialect = {
    privateCopy(allowCommaSeparatedExtend = newValue)
  }
  def withAllowEndMarker(newValue: Boolean): Dialect = {
    privateCopy(allowEndMarker = newValue)
  }
  def withAllowSignificantIndentation(newValue: Boolean): Dialect = {
    privateCopy(allowSignificantIndentation = newValue)
  }
  def withAllowQuestionMarkAsTypeWildcard(newValue: Boolean): Dialect = {
    privateCopy(allowQuestionMarkAsTypeWildcard = newValue)
  }
  @deprecated("use allowQuestionMarkAsTypeWildcard", ">4.5.13")
  def allowQuestionMarkPlaceholder: Boolean = {
    allowQuestionMarkAsTypeWildcard
  }
  @deprecated("use withAllowQuestionMarkAsTypeWildcard", ">4.5.13")
  def withAllowQuestionMarkPlaceholder(newValue: Boolean): Dialect = {
    withAllowQuestionMarkAsTypeWildcard(newValue)
  }

  def withAllowTypeParamUnderscore(newValue: Boolean): Dialect = {
    privateCopy(allowTypeParamUnderscore = newValue)
  }
  def withAllowByNameRepeatedParameters(newValue: Boolean): Dialect = {
    privateCopy(allowByNameRepeatedParameters = newValue)
  }
  def withAllowLazyValAbstractValues(newValue: Boolean): Dialect = {
    privateCopy(allowLazyValAbstractValues = newValue)
  }
  def withAllowMatchAsOperator(newValue: Boolean): Dialect = {
    privateCopy(allowMatchAsOperator = newValue)
  }
  def withAllowUpperCasePatternVarBinding(newValue: Boolean): Dialect = {
    privateCopy(allowUpperCasePatternVarBinding = newValue)
  }
  def withAllowDerives(newValue: Boolean): Dialect = {
    privateCopy(allowDerives = newValue)
  }
  def withAllowTypeInBlock(newValue: Boolean): Dialect = {
    privateCopy(allowTypeInBlock = newValue)
  }
  def withAllowPolymorphicFunctions(newValue: Boolean): Dialect = {
    privateCopy(allowPolymorphicFunctions = newValue)
  }
  def withAllowTypeMatch(newValue: Boolean): Dialect = {
    privateCopy(allowTypeMatch = newValue)
  }
  def withAllowInfixMods(newValue: Boolean): Dialect = {
    privateCopy(allowInfixMods = newValue)
  }
  def withAllowSpliceAndQuote(newValue: Boolean): Dialect = {
    privateCopy(allowSpliceAndQuote = newValue)
  }
  def withAllowSymbolLiterals(newValue: Boolean): Dialect = {
    privateCopy(allowSymbolLiterals = newValue)
  }
  def withAllowDependentFunctionTypes(newValue: Boolean): Dialect = {
    privateCopy(allowDependentFunctionTypes = newValue)
  }
  def withAllowPostfixStarVarargSplices(newValue: Boolean): Dialect = {
    privateCopy(allowPostfixStarVarargSplices = newValue)
  }
  def withAllowAllTypedPatterns(newValue: Boolean): Dialect = {
    privateCopy(allowAllTypedPatterns = newValue)
  }
  def withAllowAsForImportRename(newValue: Boolean): Dialect = {
    privateCopy(allowAsForImportRename = newValue)
  }
  def withAllowStarWildcardImport(newValue: Boolean): Dialect = {
    privateCopy(allowStarWildcardImport = newValue)
  }
  def withAllowProcedureSyntax(newValue: Boolean): Dialect = {
    privateCopy(allowProcedureSyntax = newValue)
  }
  def withAllowDoWhile(newValue: Boolean): Dialect = {
    privateCopy(allowDoWhile = newValue)
  }

  def withAllowPlusMinusUnderscoreAsIdent(newValue: Boolean): Dialect = {
    privateCopy(allowPlusMinusUnderscoreAsIdent = newValue)
  }

  def withAllowUnderscoreAsTypePlaceholder(newValue: Boolean): Dialect = {
    privateCopy(allowUnderscoreAsTypePlaceholder = newValue)
  }
  def withAllowStarAsTypePlaceholder(newValue: Boolean): Dialect = {
    privateCopy(allowStarAsTypePlaceholder = newValue)
  }
  @deprecated("use withAllowUnderscoreAsTypePlaceholder", ">4.5.13")
  def withAllowPlusMinusUnderscoreAsPlaceholder(newValue: Boolean): Dialect = {
    withAllowUnderscoreAsTypePlaceholder(newValue)
  }
  @deprecated("use allowUnderscoreAsTypePlaceholder", ">4.5.13")
  def allowPlusMinusUnderscoreAsPlaceholder: Boolean = {
    allowUnderscoreAsTypePlaceholder
  }

  def withAllowGivenImports(newValue: Boolean): Dialect = {
    privateCopy(allowGivenImports = newValue)
  }

  def withUseInfixTypePrecedence(newValue: Boolean): Dialect = {
    privateCopy(useInfixTypePrecedence = newValue)
  }

  def withAllowInfixOperatorAfterNL(newValue: Boolean): Dialect = {
    privateCopy(allowInfixOperatorAfterNL = newValue)
  }

  def withAllowParamClauseInterleaving(newValue: Boolean): Dialect = {
    privateCopy(allowParamClauseInterleaving = newValue)
  }

  def withAllowFewerBraces(newValue: Boolean): Dialect = {
    privateCopy(allowFewerBraces = newValue)
  }

  // NOTE(olafur): add the next `withX()` method above this comment. Please try
  // to use consistent formatting, use `newValue` as the parameter name and wrap
  // the body inside curly braces.

  private[this] def privateCopy(
      unquoteParentDialect: Dialect = null,
      unquoteType: UnquoteType = UnquoteType.None,
      allowAndTypes: Boolean = this.allowAndTypes,
      allowAtForExtractorVarargs: Boolean = this.allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList: Boolean = this.allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs: Boolean = this.allowColonForExtractorVarargs,
      allowEnums: Boolean = this.allowEnums,
      allowImplicitByNameParameters: Boolean = this.allowImplicitByNameParameters,
      allowInlineIdents: Boolean = this.allowInlineIdents,
      allowInlineMods: Boolean = this.allowInlineMods,
      allowLiteralTypes: Boolean = this.allowLiteralTypes,
      allowOrTypes: Boolean = this.allowOrTypes,
      allowSpliceUnderscores: Boolean = this.allowSpliceUnderscores,
      allowToplevelTerms: Boolean = this.allowToplevelTerms,
      allowTrailingCommas: Boolean = this.allowTrailingCommas,
      allowTraitParameters: Boolean = this.allowTraitParameters,
      allowTypeLambdas: Boolean = this.allowTypeLambdas,
      allowViewBounds: Boolean = this.allowViewBounds,
      allowXmlLiterals: Boolean = this.allowXmlLiterals,
      toplevelSeparator: String = this.toplevelSeparator,
      allowNumericLiteralUnderscoreSeparators: Boolean =
        this.allowNumericLiteralUnderscoreSeparators,
      allowTryWithAnyExpr: Boolean = this.allowTryWithAnyExpr,
      allowGivenUsing: Boolean = this.allowGivenUsing,
      allowErasedDefs: Boolean = this.allowErasedDefs,
      allowExtensionMethods: Boolean = this.allowExtensionMethods,
      allowOpenClass: Boolean = this.allowOpenClass,
      allowToplevelStatements: Boolean = this.allowToplevelStatements,
      allowOpaqueTypes: Boolean = this.allowOpaqueTypes,
      allowExportClause: Boolean = this.allowExportClause,
      allowCommaSeparatedExtend: Boolean = this.allowCommaSeparatedExtend,
      allowEndMarker: Boolean = this.allowEndMarker,
      allowInterpolationDolarQuoteEscape: Boolean = this.allowInterpolationDolarQuoteEscape,
      allowSignificantIndentation: Boolean = this.allowSignificantIndentation,
      allowQuestionMarkAsTypeWildcard: Boolean = this.allowQuestionMarkAsTypeWildcard,
      allowTypeParamUnderscore: Boolean = this.allowTypeParamUnderscore,
      allowByNameRepeatedParameters: Boolean = this.allowByNameRepeatedParameters,
      allowLazyValAbstractValues: Boolean = this.allowLazyValAbstractValues,
      allowUpperCasePatternVarBinding: Boolean = this.allowUpperCasePatternVarBinding,
      allowDerives: Boolean = this.allowDerives,
      allowTypeInBlock: Boolean = this.allowTypeInBlock,
      allowPolymorphicFunctions: Boolean = this.allowPolymorphicFunctions,
      allowMatchAsOperator: Boolean = this.allowMatchAsOperator,
      allowTypeMatch: Boolean = this.allowTypeMatch,
      allowInfixMods: Boolean = this.allowInfixMods,
      allowSpliceAndQuote: Boolean = this.allowSpliceAndQuote,
      allowSymbolLiterals: Boolean = this.allowSymbolLiterals,
      allowDependentFunctionTypes: Boolean = this.allowDependentFunctionTypes,
      allowPostfixStarVarargSplices: Boolean = this.allowPostfixStarVarargSplices,
      allowAllTypedPatterns: Boolean = this.allowAllTypedPatterns,
      allowAsForImportRename: Boolean = this.allowAsForImportRename,
      allowStarWildcardImport: Boolean = this.allowStarWildcardImport,
      allowProcedureSyntax: Boolean = this.allowProcedureSyntax,
      allowDoWhile: Boolean = this.allowDoWhile,
      allowPlusMinusUnderscoreAsIdent: Boolean = this.allowPlusMinusUnderscoreAsIdent,
      allowUnderscoreAsTypePlaceholder: Boolean = this.allowUnderscoreAsTypePlaceholder,
      allowStarAsTypePlaceholder: Boolean = this.allowStarAsTypePlaceholder,
      allowGivenImports: Boolean = this.allowGivenImports,
      useInfixTypePrecedence: Boolean = this.useInfixTypePrecedence,
      allowInfixOperatorAfterNL: Boolean = this.allowInfixOperatorAfterNL,
      allowParamClauseInterleaving: Boolean = this.allowParamClauseInterleaving,
      allowFewerBraces: Boolean = this.allowFewerBraces
      // NOTE(olafur): add the next parameter above this comment.
  ): Dialect = {
    new Dialect(
      unquoteParentDialect,
      unquoteType,
      allowAndTypes,
      allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs,
      allowEnums,
      allowImplicitByNameParameters,
      allowInlineIdents,
      allowInlineMods,
      allowLiteralTypes,
      allowOrTypes,
      allowSpliceUnderscores,
      allowToplevelTerms,
      allowTrailingCommas,
      allowTraitParameters,
      allowTypeLambdas,
      allowViewBounds,
      allowXmlLiterals,
      toplevelSeparator,
      allowNumericLiteralUnderscoreSeparators,
      allowTryWithAnyExpr,
      allowGivenUsing,
      allowErasedDefs,
      allowExtensionMethods,
      allowOpenClass,
      allowToplevelStatements,
      allowOpaqueTypes,
      allowExportClause,
      allowCommaSeparatedExtend,
      allowEndMarker,
      allowInterpolationDolarQuoteEscape,
      allowSignificantIndentation,
      allowQuestionMarkAsTypeWildcard,
      allowTypeParamUnderscore,
      allowByNameRepeatedParameters,
      allowLazyValAbstractValues,
      allowUpperCasePatternVarBinding,
      allowDerives,
      allowTypeInBlock,
      allowPolymorphicFunctions,
      allowMatchAsOperator,
      allowTypeMatch,
      allowInfixMods,
      allowSpliceAndQuote,
      allowSymbolLiterals,
      allowDependentFunctionTypes,
      allowPostfixStarVarargSplices,
      allowAllTypedPatterns,
      allowAsForImportRename,
      allowStarWildcardImport,
      allowProcedureSyntax,
      allowDoWhile,
      allowPlusMinusUnderscoreAsIdent,
      allowUnderscoreAsTypePlaceholder,
      allowStarAsTypePlaceholder,
      allowGivenImports,
      useInfixTypePrecedence,
      allowInfixOperatorAfterNL,
      allowParamClauseInterleaving,
      allowFewerBraces
      // NOTE(olafur): add the next argument above this comment.
    )
  }

  // NOTE(olafur): Do not edit below here, these methods can remain unchanged.

  @deprecated("Dialect should not be a Product", "4.3.11")
  override def productPrefix: String = "Dialect"
  @deprecated("Dialect should not be a Product", "4.3.11")
  def productArity: Int = 0
  @deprecated("Dialect should not be a Product", "4.3.11")
  def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)

  // Dialects have reference equality semantics,
  // because sometimes dialects representing distinct Scala versions
  // can be structurally equal to each other.
  // As Dialect is no longer a case class, no need to override equals/hashCode
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Dialect]

  // Smart prettyprinting that knows about standard dialects.
  override def toString = {
    Dialect.inverseStandards.getOrElse(this, "Dialect()")
  }

  def isEquivalentTo(that: Dialect): Boolean =
    unquoteParentOrThis().isEquivalentToInternal(that.unquoteParentOrThis())

  @inline
  private def isEquivalentToInternal(
      allowAtForExtractorVarargs: Boolean,
      allowCaseClassWithoutParameterList: Boolean,
      allowColonForExtractorVarargs: Boolean,
      allowEnums: Boolean,
      allowImplicitByNameParameters: Boolean,
      allowInlineIdents: Boolean,
      allowInlineMods: Boolean,
      allowLiteralTypes: Boolean,
      allowSpliceUnderscores: Boolean,
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      allowViewBounds: Boolean,
      allowXmlLiterals: Boolean,
      allowNumericLiteralUnderscoreSeparators: Boolean,
      allowTryWithAnyExpr: Boolean,
      allowGivenUsing: Boolean,
      allowErasedDefs: Boolean,
      allowExtensionMethods: Boolean,
      allowOpenClass: Boolean,
      allowToplevelStatements: Boolean,
      allowOpaqueTypes: Boolean,
      allowExportClause: Boolean,
      allowCommaSeparatedExtend: Boolean,
      allowEndMarker: Boolean,
      allowInterpolationDolarQuoteEscape: Boolean,
      allowSignificantIndentation: Boolean,
      allowQuestionMarkAsTypeWildcard: Boolean,
      allowTypeParamUnderscore: Boolean,
      allowByNameRepeatedParameters: Boolean,
      allowLazyValAbstractValues: Boolean,
      allowUpperCasePatternVarBinding: Boolean,
      allowDerives: Boolean,
      allowTypeInBlock: Boolean,
      allowPolymorphicFunctions: Boolean,
      allowMatchAsOperator: Boolean,
      allowTypeMatch: Boolean,
      allowInfixMods: Boolean,
      allowSpliceAndQuote: Boolean,
      allowSymbolLiterals: Boolean,
      allowDependentFunctionTypes: Boolean,
      allowPostfixStarVarargSplices: Boolean,
      allowAllTypedPatterns: Boolean,
      allowAsForImportRename: Boolean,
      allowStarWildcardImport: Boolean,
      allowProcedureSyntax: Boolean,
      allowDoWhile: Boolean,
      allowPlusMinusUnderscoreAsIdent: Boolean,
      allowUnderscoreAsTypePlaceholder: Boolean,
      allowStarAsTypePlaceholder: Boolean,
      allowGivenImports: Boolean,
      useInfixTypePrecedence: Boolean,
      allowInfixOperatorAfterNL: Boolean,
      allowParamClauseInterleaving: Boolean,
      allowFewerBraces: Boolean
  ): Boolean = (
    // do not include deprecated values in this comparison
    this.allowAtForExtractorVarargs == allowAtForExtractorVarargs
      && this.allowCaseClassWithoutParameterList == allowCaseClassWithoutParameterList
      && this.allowColonForExtractorVarargs == allowColonForExtractorVarargs
      && this.allowEnums == allowEnums
      && this.allowImplicitByNameParameters == allowImplicitByNameParameters
      && this.allowInlineIdents == allowInlineIdents
      && this.allowInlineMods == allowInlineMods
      && this.allowLiteralTypes == allowLiteralTypes
      && this.allowSpliceUnderscores == allowSpliceUnderscores
      && this.allowToplevelTerms == allowToplevelTerms
      && this.allowTrailingCommas == allowTrailingCommas
      && this.allowTraitParameters == allowTraitParameters
      && this.allowTypeLambdas == allowTypeLambdas
      && this.allowViewBounds == allowViewBounds
      && this.allowXmlLiterals == allowXmlLiterals
      && this.allowNumericLiteralUnderscoreSeparators == allowNumericLiteralUnderscoreSeparators
      && this.allowTryWithAnyExpr == allowTryWithAnyExpr
      && this.allowGivenUsing == allowGivenUsing
      && this.allowErasedDefs == allowErasedDefs
      && this.allowExtensionMethods == allowExtensionMethods
      && this.allowOpenClass == allowOpenClass
      && this.allowToplevelStatements == allowToplevelStatements
      && this.allowOpaqueTypes == allowOpaqueTypes
      && this.allowExportClause == allowExportClause
      && this.allowCommaSeparatedExtend == allowCommaSeparatedExtend
      && this.allowEndMarker == allowEndMarker
      && this.allowInterpolationDolarQuoteEscape == allowInterpolationDolarQuoteEscape
      && this.allowSignificantIndentation == allowSignificantIndentation
      && this.allowQuestionMarkAsTypeWildcard == allowQuestionMarkAsTypeWildcard
      && this.allowTypeParamUnderscore == allowTypeParamUnderscore
      && this.allowByNameRepeatedParameters == allowByNameRepeatedParameters
      && this.allowLazyValAbstractValues == allowLazyValAbstractValues
      && this.allowUpperCasePatternVarBinding == allowUpperCasePatternVarBinding
      && this.allowDerives == allowDerives
      && this.allowTypeInBlock == allowTypeInBlock
      && this.allowPolymorphicFunctions == allowPolymorphicFunctions
      && this.allowMatchAsOperator == allowMatchAsOperator
      && this.allowTypeMatch == allowTypeMatch
      && this.allowInfixMods == allowInfixMods
      && this.allowSpliceAndQuote == allowSpliceAndQuote
      && this.allowSymbolLiterals == allowSymbolLiterals
      && this.allowDependentFunctionTypes == allowDependentFunctionTypes
      && this.allowPostfixStarVarargSplices == allowPostfixStarVarargSplices
      && this.allowAllTypedPatterns == allowAllTypedPatterns
      && this.allowAsForImportRename == allowAsForImportRename
      && this.allowStarWildcardImport == allowStarWildcardImport
      && this.allowProcedureSyntax == allowProcedureSyntax
      && this.allowDoWhile == allowDoWhile
      && this.allowPlusMinusUnderscoreAsIdent == allowPlusMinusUnderscoreAsIdent
      && this.allowUnderscoreAsTypePlaceholder == allowUnderscoreAsTypePlaceholder
      && this.allowStarAsTypePlaceholder == allowStarAsTypePlaceholder
      && this.allowGivenImports == allowGivenImports
      && this.useInfixTypePrecedence == useInfixTypePrecedence
      && this.allowInfixOperatorAfterNL == allowInfixOperatorAfterNL
      && this.allowParamClauseInterleaving == allowParamClauseInterleaving
      && this.allowFewerBraces == allowFewerBraces
  )

  @inline
  private def isEquivalentToInternal(that: Dialect): Boolean =
    (this eq that) || isEquivalentToInternal(
      that.allowAtForExtractorVarargs,
      that.allowCaseClassWithoutParameterList,
      that.allowColonForExtractorVarargs,
      that.allowEnums,
      that.allowImplicitByNameParameters,
      that.allowInlineIdents,
      that.allowInlineMods,
      that.allowLiteralTypes,
      that.allowSpliceUnderscores,
      that.allowToplevelTerms,
      that.allowTrailingCommas,
      that.allowTraitParameters,
      that.allowTypeLambdas,
      that.allowViewBounds,
      that.allowXmlLiterals,
      that.allowNumericLiteralUnderscoreSeparators,
      that.allowTryWithAnyExpr,
      that.allowGivenUsing,
      that.allowErasedDefs,
      that.allowExtensionMethods,
      that.allowOpenClass,
      that.allowToplevelStatements,
      that.allowOpaqueTypes,
      that.allowExportClause,
      that.allowCommaSeparatedExtend,
      that.allowEndMarker,
      that.allowInterpolationDolarQuoteEscape,
      that.allowSignificantIndentation,
      that.allowQuestionMarkAsTypeWildcard,
      that.allowTypeParamUnderscore,
      that.allowByNameRepeatedParameters,
      that.allowLazyValAbstractValues,
      that.allowUpperCasePatternVarBinding,
      that.allowDerives,
      that.allowTypeInBlock,
      that.allowPolymorphicFunctions,
      that.allowMatchAsOperator,
      that.allowTypeMatch,
      that.allowInfixMods,
      that.allowSpliceAndQuote,
      that.allowSymbolLiterals,
      that.allowDependentFunctionTypes,
      that.allowPostfixStarVarargSplices,
      that.allowAllTypedPatterns,
      that.allowAsForImportRename,
      that.allowStarWildcardImport,
      that.allowProcedureSyntax,
      that.allowDoWhile,
      that.allowPlusMinusUnderscoreAsIdent,
      that.allowUnderscoreAsTypePlaceholder,
      that.allowStarAsTypePlaceholder,
      that.allowGivenImports,
      that.useInfixTypePrecedence,
      that.allowInfixOperatorAfterNL,
      that.allowParamClauseInterleaving,
      that.allowFewerBraces
    )

  @deprecated("Use withX method instead", "4.3.11")
  def copy(
      allowAndTypes: Boolean = this.allowAndTypes,
      allowAtForExtractorVarargs: Boolean = this.allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList: Boolean = this.allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs: Boolean = this.allowColonForExtractorVarargs,
      allowEnums: Boolean = this.allowEnums,
      allowImplicitByNameParameters: Boolean = this.allowImplicitByNameParameters,
      allowInlineIdents: Boolean = this.allowInlineIdents,
      allowInlineMods: Boolean = this.allowInlineMods,
      allowLiteralTypes: Boolean = this.allowLiteralTypes,
      allowMultilinePrograms: Boolean = true, // unused
      allowOrTypes: Boolean = this.allowOrTypes,
      allowPatUnquotes: Boolean = false, // unused
      allowSpliceUnderscores: Boolean = this.allowSpliceUnderscores,
      allowTermUnquotes: Boolean = false, // unused
      allowToplevelTerms: Boolean = this.allowToplevelTerms,
      allowTrailingCommas: Boolean = this.allowTrailingCommas,
      allowTraitParameters: Boolean = this.allowTraitParameters,
      allowTypeLambdas: Boolean = this.allowTypeLambdas,
      allowViewBounds: Boolean = this.allowViewBounds,
      allowXmlLiterals: Boolean = this.allowXmlLiterals,
      toplevelSeparator: String = this.toplevelSeparator
  ): Dialect = {
    privateCopy(
      unquoteParentDialect = null,
      unquoteType = UnquoteType.None,
      allowAndTypes,
      allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs,
      allowEnums,
      allowImplicitByNameParameters,
      allowInlineIdents,
      allowInlineMods,
      allowLiteralTypes,
      allowOrTypes,
      allowSpliceUnderscores,
      allowToplevelTerms,
      allowTrailingCommas,
      allowTraitParameters,
      allowTypeLambdas,
      allowViewBounds,
      allowXmlLiterals,
      toplevelSeparator
    )
  }

  private[meta] def unquoteParentOrThis(): Dialect =
    if (null eq unquoteParentDialect) this else unquoteParentDialect

  private[meta] def unquote(unquoteType: UnquoteType): Dialect = {
    require(null eq unquoteParentDialect)
    privateCopy(
      unquoteParentDialect = this,
      unquoteType = unquoteType,
      allowTypeLambdas = true
    )
  }

  private[meta] def unquoteTerm(multiline: Boolean): Dialect =
    unquote(UnquoteType.Term(multiline))

  private[meta] def unquotePat(multiline: Boolean): Dialect =
    unquote(UnquoteType.Pat(multiline))

}

object Dialect extends InternalDialect {
  def apply(
      @deprecated("allowAndTypes unneeded, infix types are supported", "4.5.1")
      allowAndTypes: Boolean,
      allowAtForExtractorVarargs: Boolean,
      allowCaseClassWithoutParameterList: Boolean,
      allowColonForExtractorVarargs: Boolean,
      allowEnums: Boolean,
      allowImplicitByNameParameters: Boolean,
      allowInlineIdents: Boolean,
      allowInlineMods: Boolean,
      allowLiteralTypes: Boolean,
      allowMethodTypes: Boolean,
      allowMultilinePrograms: Boolean = true, // unused
      @deprecated("allowOrTypes unneeded, infix types are supported", "4.5.1")
      allowOrTypes: Boolean,
      allowPatUnquotes: Boolean = false, // unused
      allowSpliceUnderscores: Boolean,
      allowTermUnquotes: Boolean = false, // unused
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      allowViewBounds: Boolean,
      allowXmlLiterals: Boolean,
      @deprecated("toplevelSeparator has never been used", ">4.4.35")
      toplevelSeparator: String
  ): Dialect = {
    new Dialect(
      allowAndTypes = allowAndTypes,
      allowAtForExtractorVarargs = allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs = allowColonForExtractorVarargs,
      allowEnums = allowEnums,
      allowImplicitByNameParameters = allowImplicitByNameParameters,
      allowInlineIdents = allowInlineIdents,
      allowInlineMods = allowInlineMods,
      allowLiteralTypes = allowLiteralTypes,
      allowOrTypes = allowOrTypes,
      allowSpliceUnderscores = allowSpliceUnderscores,
      allowToplevelTerms = allowToplevelTerms,
      allowTrailingCommas = allowTrailingCommas,
      allowTraitParameters = allowTraitParameters,
      allowTypeLambdas = allowTypeLambdas,
      allowViewBounds = allowViewBounds,
      allowXmlLiterals = allowXmlLiterals,
      toplevelSeparator = toplevelSeparator
    )
  }
  private lazy val standardPairs = Seq[sourcecode.Text[Dialect]](
    Dotty,
    Scala3Future,
    Scala3,
    Scala30,
    Scala31,
    Scala32,
    Scala33,
    Paradise211,
    Paradise212,
    ParadiseTypelevel211,
    ParadiseTypelevel212,
    Sbt0136,
    Sbt0137,
    Sbt1,
    Scala210,
    Scala211,
    Scala212,
    Scala213,
    Scala213Source3,
    Scala212Source3,
    Typelevel211,
    Typelevel212
  )
  private[meta] lazy val standards: Map[String, Dialect] =
    standardPairs.map(x => x.source -> x.value).toMap
  private[meta] lazy val inverseStandards: Map[Dialect, String] =
    standardPairs.map(x => x.value -> x.source).toMap

  private[meta] sealed trait UnquoteType {
    // Are multiline programs allowed?
    // Some quasiquotes only support single-line snippets.
    def isMultiline: Boolean
  }
  private[meta] object UnquoteType {
    case object None extends UnquoteType {
      val isMultiline = true
    }
    class Pat private (val isMultiline: Boolean) extends UnquoteType
    object Pat {
      def apply(isMultiline: Boolean) = if (isMultiline) multi else single
      val multi = new Pat(true)
      val single = new Pat(false)
    }
    class Term private (val isMultiline: Boolean) extends UnquoteType
    object Term {
      def apply(isMultiline: Boolean) = if (isMultiline) multi else single
      val multi = new Term(true)
      val single = new Term(false)
    }
  }

}
