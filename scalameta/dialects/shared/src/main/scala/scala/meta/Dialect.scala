package scala.meta

import scala.meta.dialects._
import scala.meta.internal.dialects._
import scala.collection.immutable.TreeMap

/**
 * A dialect is used to configure what Scala syntax is allowed during tokenization and parsing.
 */
final class Dialect private (
    // Are `&` intersection types supported by this dialect?
    val allowAndTypes: Boolean,
    // Are extractor varargs specified using ats, i.e. is `case Extractor(xs @ _*)` legal or not?
    val allowAtForExtractorVarargs: Boolean,
    // Can case classes be declared without a parameter list?
    // Deprecated in 2.10, not supported in 2.11 and newer.
    val allowCaseClassWithoutParameterList: Boolean,
    // Are extractor varargs specified using colons, i.e. is `case Extractor(xs: _*)` legal or not?
    val allowColonForExtractorVarargs: Boolean,
    // Are enums allowed?
    // They are in Dotty, but not in Scala 2.12 or older.
    val allowEnums: Boolean,
    // Are implicit by name parameters supported?
    // They are in Dotty, but not in Scala 2.12 or older.
    val allowImplicitByNameParameters: Boolean,
    // Implicit functions are no longer supported in any dialect
    val allowImplicitFunctionTypes: Boolean,
    // Are `inline` identifiers supported by this dialect?
    val allowInlineIdents: Boolean,
    // Are inline vals and defs supported by this dialect?
    val allowInlineMods: Boolean,
    // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
    val allowLiteralTypes: Boolean,
    // Are method types allowed, i.e. is `(x: X): x.T` legal or not?
    @deprecated("Method type syntax is no longer supported in any dialect", "4.4.3")
    val allowMethodTypes: Boolean,
    // Are multiline programs allowed?
    // Some quasiquotes only support single-line snippets.
    val allowMultilinePrograms: Boolean,
    // Are `|` (union types) supported by this dialect?
    val allowOrTypes: Boolean,
    // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
    // If yes, they will be parsed as patterns.
    val allowPatUnquotes: Boolean,
    // Are naked underscores allowed after $ in pattern interpolators, i.e. is `case q"$_ + $_" =>` legal or not?
    val allowSpliceUnderscores: Boolean,
    // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
    // If yes, they will be parsed as terms.
    val allowTermUnquotes: Boolean,
    // Are terms on the top level supported by this dialect?
    // Necessary to support popular script-like DSLs.
    val allowToplevelTerms: Boolean,
    // Are trailing commas allowed? SIP-27.
    val allowTrailingCommas: Boolean,
    // Are trait allowed to have parameters?
    // They are in Dotty, but not in Scala 2.12 or older.
    val allowTraitParameters: Boolean,
    // Are type lambdas allowed, i.e. is `[T] => (T, T)` legal or not?
    val allowTypeLambdas: Boolean,
    // Are view bounds supported by this dialect?
    // def f[A <% Int](a: A)
    // Removed in Dotty.
    val allowViewBounds: Boolean,
    // Are `with` intersection types supported by this dialect?
    @deprecated(
      "With types are supported in every dialect currently and this option is unused",
      "4.4.6"
    )
    val allowWithTypes: Boolean,
    // Are XML literals supported by this dialect?
    // We plan to deprecate XML literal syntax, and some dialects
    // might go ahead and drop support completely.
    val allowXmlLiterals: Boolean,
    // What kind of separator is necessary to split top-level statements?
    // Normally none is required, but scripts may have their own rules.
    val toplevelSeparator: String,
    // Are numeric literal underscore separators, i.e. `1_000_000` legal or not?
    val allowNumericLiteralUnderscoreSeparators: Boolean,
    // Can try body contain any expression? (2.13.1 https://github.com/scala/scala/pull/8071)
    val allowTryWithAnyExpr: Boolean,
    // Given/using introduced in dotty
    val allowGivenUsing: Boolean,
    // Extension methods introduced in dotty
    val allowExtensionMethods: Boolean,
    // Open modifier for classes introduced in dotty
    val allowOpenClass: Boolean,
    // Scala 3 splices/quotes
    @deprecated("Replaced with allowSpliceAndQuote", "4.4.1")
    val allowWhiteboxMacro: Boolean,
    // Top level statements introduced in dotty.
    // differs from ToplevelTerms because here you can define packages
    val allowToplevelStatements: Boolean,
    // Opaque types introduced in dotty
    val allowOpaqueTypes: Boolean,
    // Literal Unit Type
    @deprecated("Literal unit types are longer supported in any dialect", "4.4.9")
    val allowLiteralUnitType: Boolean,
    // Super traits introduced in dotty, but later removed.
    val allowSuperTrait: Boolean,
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
    // Dotty changed placeholder for types from `_` to `?`
    val allowQuestionMarkPlaceholder: Boolean,
    // Dotty rejects placeholder as Type parameter
    val allowTypeParamUnderscore: Boolean,
    // Dotty allows by-name repeated parameters
    val allowByNameRepeatedParameters: Boolean,
    // Dotty allows lazy val abstract values
    val allowLazyValAbstractValues: Boolean,
    // Dotty used to allow `as` instead of `@` for pattern bindings
    @deprecated("`as` in patterns is no longer supported in any dialect", "4.4.4")
    val allowAsPatternBinding: Boolean,
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
    /* The same as previous but for Scala3
     * works under -Ykind-projector:underscores
     */
    val allowPlusMinusUnderscoreAsPlaceholder: Boolean,
    // import a.b.c.{ given, _} used for -X:source3
    val allowGivenImports: Boolean
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
      allowImplicitFunctionTypes: Boolean,
      allowInlineIdents: Boolean,
      allowInlineMods: Boolean,
      allowLiteralTypes: Boolean,
      allowMethodTypes: Boolean,
      allowMultilinePrograms: Boolean,
      allowOrTypes: Boolean,
      allowPatUnquotes: Boolean,
      allowSpliceUnderscores: Boolean,
      allowTermUnquotes: Boolean,
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      allowViewBounds: Boolean,
      allowWithTypes: Boolean,
      allowXmlLiterals: Boolean,
      toplevelSeparator: String
  ) = {
    this(
      allowAndTypes = allowAndTypes,
      allowAtForExtractorVarargs = allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs = allowColonForExtractorVarargs,
      allowEnums = allowEnums,
      allowImplicitByNameParameters = allowImplicitByNameParameters,
      allowImplicitFunctionTypes = allowImplicitFunctionTypes,
      allowInlineIdents = allowInlineIdents,
      allowInlineMods = allowInlineMods,
      allowLiteralTypes = allowLiteralTypes,
      allowMethodTypes = allowMethodTypes,
      allowMultilinePrograms = allowMultilinePrograms,
      allowOrTypes = allowOrTypes,
      allowPatUnquotes = allowPatUnquotes,
      allowSpliceUnderscores = allowSpliceUnderscores,
      allowTermUnquotes = allowTermUnquotes,
      allowToplevelTerms = allowToplevelTerms,
      allowTrailingCommas = allowTrailingCommas,
      allowTraitParameters = allowTraitParameters,
      allowTypeLambdas = allowTypeLambdas,
      allowViewBounds = allowViewBounds,
      allowWithTypes = allowWithTypes,
      allowXmlLiterals = allowXmlLiterals,
      toplevelSeparator = toplevelSeparator,
      allowNumericLiteralUnderscoreSeparators = false,
      allowTryWithAnyExpr = false,
      allowGivenUsing = false,
      allowExtensionMethods = false,
      allowOpenClass = false,
      allowWhiteboxMacro = false,
      allowToplevelStatements = false,
      allowOpaqueTypes = false,
      allowLiteralUnitType = false,
      allowSuperTrait = false,
      allowExportClause = false,
      allowCommaSeparatedExtend = false,
      allowEndMarker = false,
      allowInterpolationDolarQuoteEscape = false,
      allowSignificantIndentation = false,
      allowQuestionMarkPlaceholder = false,
      allowTypeParamUnderscore = true,
      allowByNameRepeatedParameters = false,
      allowLazyValAbstractValues = false,
      allowAsPatternBinding = false,
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
      allowPlusMinusUnderscoreAsPlaceholder = false,
      allowGivenImports = false
      // NOTE(olafur): declare the default value for new fields above this comment.
    )
  }

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  def allowUnquotes: Boolean = allowTermUnquotes || allowPatUnquotes

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
  @deprecated("Implicit functions are not supported in any dialect", "4.4.1")
  def withAllowImplicitFunctionTypes(newValue: Boolean): Dialect = {
    privateCopy(allowImplicitFunctionTypes = newValue)
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
  @deprecated("Method type syntax is no longer supported in any dialect", "4.4.3")
  def withAllowMethodTypes(newValue: Boolean): Dialect = {
    privateCopy(allowMethodTypes = newValue)
  }
  def withAllowMultilinePrograms(newValue: Boolean): Dialect = {
    privateCopy(allowMultilinePrograms = newValue)
  }
  def withAllowOrTypes(newValue: Boolean): Dialect = {
    privateCopy(allowOrTypes = newValue)
  }
  def withAllowPatUnquotes(newValue: Boolean): Dialect = {
    privateCopy(allowPatUnquotes = newValue)
  }
  def withAllowSpliceUnderscores(newValue: Boolean): Dialect = {
    privateCopy(allowSpliceUnderscores = newValue)
  }
  def withAllowTermUnquotes(newValue: Boolean): Dialect = {
    privateCopy(allowTermUnquotes = newValue)
  }
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
  @deprecated(
    "With types are supported in every dialect currently and this option is unused",
    "4.4.6"
  )
  def withAllowWithTypes(newValue: Boolean): Dialect = {
    privateCopy(allowWithTypes = newValue)
  }
  def withAllowXmlLiterals(newValue: Boolean): Dialect = {
    privateCopy(allowXmlLiterals = newValue)
  }
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
  def withAllowExtensionMethods(newValue: Boolean): Dialect = {
    privateCopy(allowExtensionMethods = newValue)
  }
  def withAllowOpenClass(newValue: Boolean): Dialect = {
    privateCopy(allowOpenClass = newValue)
  }
  @deprecated("Replaced by withAllowSplicesAndQuotes", "4.4.1")
  def withAllowWhiteboxMacro(newValue: Boolean): Dialect = {
    privateCopy(allowWhiteboxMacro = newValue)
  }
  def withAllowToplevelStatements(newValue: Boolean): Dialect = {
    privateCopy(allowToplevelStatements = newValue)
  }
  def withAllowOpaqueTypes(newValue: Boolean): Dialect = {
    privateCopy(allowOpaqueTypes = newValue)
  }
  @deprecated("Literal unit types are longer supported in any dialect", "4.4.9")
  def withAllowLiteralUnitType(newValue: Boolean): Dialect = {
    privateCopy(allowLiteralUnitType = newValue)
  }

  def withAllowInterpolationDolarQuoteEscape(newValue: Boolean): Dialect = {
    privateCopy(allowInterpolationDolarQuoteEscape = newValue)
  }
  @deprecated("Super traits are not supported in any dialect", "4.4.1")
  def withAllowSuperTrait(newValue: Boolean): Dialect = {
    privateCopy(allowSuperTrait = newValue)
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
  def withAllowQuestionMarkPlaceholder(newValue: Boolean): Dialect = {
    privateCopy(allowQuestionMarkPlaceholder = newValue)
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
  @deprecated("`as` in patterns is no longer supported in any dialect", "4.4.4")
  def withAllowAsPatternBinding(newValue: Boolean): Dialect = {
    privateCopy(allowAsPatternBinding = newValue)
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
    privateCopy(allowAsRenames = newValue)
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

  def withAllowPlusMinusUnderscoreAsPlaceholder(newValue: Boolean): Dialect = {
    privateCopy(allowPlusMinusUnderscoreAsPlaceholder = newValue)
  }
  def withAllowGivenImports(newValue: Boolean): Dialect = {
    privateCopy(allowGivenImports = newValue)
  }
  // NOTE(olafur): add the next `withX()` method above this comment. Please try
  // to use consistent formatting, use `newValue` as the parameter name and wrap
  // the body inside curly braces.

  private[this] def privateCopy(
      allowAndTypes: Boolean = this.allowAndTypes,
      allowAtForExtractorVarargs: Boolean = this.allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList: Boolean = this.allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs: Boolean = this.allowColonForExtractorVarargs,
      allowEnums: Boolean = this.allowEnums,
      allowImplicitByNameParameters: Boolean = this.allowImplicitByNameParameters,
      allowImplicitFunctionTypes: Boolean = this.allowImplicitFunctionTypes,
      allowInlineIdents: Boolean = this.allowInlineIdents,
      allowInlineMods: Boolean = this.allowInlineMods,
      allowLiteralTypes: Boolean = this.allowLiteralTypes,
      allowMethodTypes: Boolean = this.allowMethodTypes,
      allowMultilinePrograms: Boolean = this.allowMultilinePrograms,
      allowOrTypes: Boolean = this.allowOrTypes,
      allowPatUnquotes: Boolean = this.allowPatUnquotes,
      allowSpliceUnderscores: Boolean = this.allowSpliceUnderscores,
      allowTermUnquotes: Boolean = this.allowTermUnquotes,
      allowToplevelTerms: Boolean = this.allowToplevelTerms,
      allowTrailingCommas: Boolean = this.allowTrailingCommas,
      allowTraitParameters: Boolean = this.allowTraitParameters,
      allowTypeLambdas: Boolean = this.allowTypeLambdas,
      allowViewBounds: Boolean = this.allowViewBounds,
      allowWithTypes: Boolean = this.allowWithTypes,
      allowXmlLiterals: Boolean = this.allowXmlLiterals,
      toplevelSeparator: String = this.toplevelSeparator,
      allowNumericLiteralUnderscoreSeparators: Boolean =
        this.allowNumericLiteralUnderscoreSeparators,
      allowTryWithAnyExpr: Boolean = this.allowTryWithAnyExpr,
      allowGivenUsing: Boolean = this.allowGivenUsing,
      allowExtensionMethods: Boolean = this.allowExtensionMethods,
      allowOpenClass: Boolean = this.allowOpenClass,
      allowWhiteboxMacro: Boolean = this.allowWhiteboxMacro,
      allowToplevelStatements: Boolean = this.allowToplevelStatements,
      allowOpaqueTypes: Boolean = this.allowOpaqueTypes,
      allowLiteralUnitType: Boolean = this.allowLiteralUnitType,
      allowSuperTrait: Boolean = this.allowSuperTrait,
      allowExportClause: Boolean = this.allowExportClause,
      allowCommaSeparatedExtend: Boolean = this.allowCommaSeparatedExtend,
      allowEndMarker: Boolean = this.allowEndMarker,
      allowInterpolationDolarQuoteEscape: Boolean = this.allowInterpolationDolarQuoteEscape,
      allowSignificantIndentation: Boolean = this.allowSignificantIndentation,
      allowQuestionMarkPlaceholder: Boolean = this.allowQuestionMarkPlaceholder,
      allowTypeParamUnderscore: Boolean = this.allowTypeParamUnderscore,
      allowByNameRepeatedParameters: Boolean = this.allowByNameRepeatedParameters,
      allowLazyValAbstractValues: Boolean = this.allowLazyValAbstractValues,
      allowAsPatternBinding: Boolean = this.allowAsPatternBinding,
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
      allowAsRenames: Boolean = this.allowAsForImportRename,
      allowStarWildcardImport: Boolean = this.allowStarWildcardImport,
      allowProcedureSyntax: Boolean = this.allowProcedureSyntax,
      allowDoWhile: Boolean = this.allowDoWhile,
      allowPlusMinusUnderscoreAsIdent: Boolean = this.allowPlusMinusUnderscoreAsIdent,
      allowPlusMinusUnderscoreAsPlaceholder: Boolean = this.allowPlusMinusUnderscoreAsPlaceholder,
      allowGivenImports: Boolean = this.allowGivenImports
      // NOTE(olafur): add the next parameter above this comment.
  ): Dialect = {
    new Dialect(
      allowAndTypes,
      allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs,
      allowEnums,
      allowImplicitByNameParameters,
      allowImplicitFunctionTypes,
      allowInlineIdents,
      allowInlineMods,
      allowLiteralTypes,
      allowMethodTypes,
      allowMultilinePrograms,
      allowOrTypes,
      allowPatUnquotes,
      allowSpliceUnderscores,
      allowTermUnquotes,
      allowToplevelTerms,
      allowTrailingCommas,
      allowTraitParameters,
      allowTypeLambdas,
      allowViewBounds,
      allowWithTypes,
      allowXmlLiterals,
      toplevelSeparator,
      allowNumericLiteralUnderscoreSeparators,
      allowTryWithAnyExpr,
      allowGivenUsing,
      allowExtensionMethods,
      allowOpenClass,
      allowWhiteboxMacro,
      allowToplevelStatements,
      allowOpaqueTypes,
      allowLiteralUnitType,
      allowSuperTrait,
      allowExportClause,
      allowCommaSeparatedExtend,
      allowEndMarker,
      allowInterpolationDolarQuoteEscape,
      allowSignificantIndentation,
      allowQuestionMarkPlaceholder,
      allowTypeParamUnderscore,
      allowByNameRepeatedParameters,
      allowLazyValAbstractValues,
      allowAsPatternBinding,
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
      allowAsRenames,
      allowStarWildcardImport,
      allowProcedureSyntax,
      allowDoWhile,
      allowPlusMinusUnderscoreAsIdent,
      allowPlusMinusUnderscoreAsPlaceholder,
      allowGivenImports
      // NOTE(olafur): add the next argument above this comment.
    )
  }

  // NOTE(olafur): Do not edit below here, these methods can remain unchanged.

  @deprecated("Dialect should not be a Product", "4.3.11")
  override def productPrefix: String = "Dialect"
  @deprecated("Dialect should not be a Product", "4.3.11")
  def productArity: Int = 0
  @deprecated("Dialect should not be a Product", "4.3.11")
  def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString())

  // Dialects have reference equality semantics,
  // because sometimes dialects representing distinct Scala versions
  // can be structurally equal to each other.
  override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def equals(other: Any): Boolean = this eq other.asInstanceOf[AnyRef]
  override def hashCode: Int = System.identityHashCode(this)
  // Smart prettyprinting that knows about standard dialects.
  override def toString = {
    Dialect.inverseStandards.get(this) match {
      case Some(name) => name
      case None => s"Dialect()"
    }
  }

  @deprecated("Use withX method instead", "4.3.11")
  def copy(
      allowAndTypes: Boolean = this.allowAndTypes,
      allowAtForExtractorVarargs: Boolean = this.allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList: Boolean = this.allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs: Boolean = this.allowColonForExtractorVarargs,
      allowEnums: Boolean = this.allowEnums,
      allowImplicitByNameParameters: Boolean = this.allowImplicitByNameParameters,
      allowImplicitFunctionTypes: Boolean = this.allowImplicitFunctionTypes,
      allowInlineIdents: Boolean = this.allowInlineIdents,
      allowInlineMods: Boolean = this.allowInlineMods,
      allowLiteralTypes: Boolean = this.allowLiteralTypes,
      allowMethodTypes: Boolean = this.allowMethodTypes,
      allowMultilinePrograms: Boolean = this.allowMultilinePrograms,
      allowOrTypes: Boolean = this.allowOrTypes,
      allowPatUnquotes: Boolean = this.allowPatUnquotes,
      allowSpliceUnderscores: Boolean = this.allowSpliceUnderscores,
      allowTermUnquotes: Boolean = this.allowTermUnquotes,
      allowToplevelTerms: Boolean = this.allowToplevelTerms,
      allowTrailingCommas: Boolean = this.allowTrailingCommas,
      allowTraitParameters: Boolean = this.allowTraitParameters,
      allowTypeLambdas: Boolean = this.allowTypeLambdas,
      allowViewBounds: Boolean = this.allowViewBounds,
      allowWithTypes: Boolean = this.allowWithTypes,
      allowXmlLiterals: Boolean = this.allowXmlLiterals,
      toplevelSeparator: String = this.toplevelSeparator
  ): Dialect = {
    privateCopy(
      allowAndTypes,
      allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs,
      allowEnums,
      allowImplicitByNameParameters,
      allowImplicitFunctionTypes,
      allowInlineIdents,
      allowInlineMods,
      allowLiteralTypes,
      allowMethodTypes,
      allowMultilinePrograms,
      allowOrTypes,
      allowPatUnquotes,
      allowSpliceUnderscores,
      allowTermUnquotes,
      allowToplevelTerms,
      allowTrailingCommas,
      allowTraitParameters,
      allowTypeLambdas,
      allowViewBounds,
      allowWithTypes,
      allowXmlLiterals,
      toplevelSeparator
    )
  }
}

object Dialect extends InternalDialect {
  def apply(
      allowAndTypes: Boolean,
      allowAtForExtractorVarargs: Boolean,
      allowCaseClassWithoutParameterList: Boolean,
      allowColonForExtractorVarargs: Boolean,
      allowEnums: Boolean,
      allowImplicitByNameParameters: Boolean,
      allowImplicitFunctionTypes: Boolean,
      allowInlineIdents: Boolean,
      allowInlineMods: Boolean,
      allowLiteralTypes: Boolean,
      allowMethodTypes: Boolean,
      allowMultilinePrograms: Boolean,
      allowOrTypes: Boolean,
      allowPatUnquotes: Boolean,
      allowSpliceUnderscores: Boolean,
      allowTermUnquotes: Boolean,
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      allowViewBounds: Boolean,
      allowWithTypes: Boolean,
      allowXmlLiterals: Boolean,
      toplevelSeparator: String
  ): Dialect = {
    new Dialect(
      allowAndTypes = allowAndTypes,
      allowAtForExtractorVarargs = allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs = allowColonForExtractorVarargs,
      allowEnums = allowEnums,
      allowImplicitByNameParameters = allowImplicitByNameParameters,
      allowImplicitFunctionTypes = allowImplicitFunctionTypes,
      allowInlineIdents = allowInlineIdents,
      allowInlineMods = allowInlineMods,
      allowLiteralTypes = allowLiteralTypes,
      allowMethodTypes = allowMethodTypes,
      allowMultilinePrograms = allowMultilinePrograms,
      allowOrTypes = allowOrTypes,
      allowPatUnquotes = allowPatUnquotes,
      allowSpliceUnderscores = allowSpliceUnderscores,
      allowTermUnquotes = allowTermUnquotes,
      allowToplevelTerms = allowToplevelTerms,
      allowTrailingCommas = allowTrailingCommas,
      allowTraitParameters = allowTraitParameters,
      allowTypeLambdas = allowTypeLambdas,
      allowViewBounds = allowViewBounds,
      allowWithTypes = allowWithTypes,
      allowXmlLiterals = allowXmlLiterals,
      toplevelSeparator = toplevelSeparator
    )
  }
  // NOTE: Spinning up a macro just for this is too hard.
  // Using JVM reflection won't be portable to Scala.js.
  private[meta] lazy val standards: Map[String, Dialect] = TreeMap(
    "Dotty" -> Dotty,
    "Scala3" -> Scala3,
    "Paradise211" -> Paradise211,
    "Paradise212" -> Paradise212,
    "ParadiseTypelevel211" -> ParadiseTypelevel211,
    "ParadiseTypelevel212" -> ParadiseTypelevel212,
    "Sbt0136" -> Sbt0136,
    "Sbt0137" -> Sbt0137,
    "Sbt1" -> Sbt1,
    "Scala210" -> Scala210,
    "Scala211" -> Scala211,
    "Scala212" -> Scala212,
    "Scala213" -> Scala213,
    "Scala213Source3" -> Scala213Source3,
    "Scala212Source3" -> Scala212Source3,
    "Typelevel211" -> Typelevel211,
    "Typelevel212" -> Typelevel212
  )
  private[meta] lazy val inverseStandards: Map[Dialect, String] =
    standards.iterator.map(_.swap).toMap
}
