package scala.meta

import org.scalameta.invariants
import scala.meta.Dialect.UnquoteType
import scala.meta.dialects._
import scala.meta.internal.dialects._

/**
 * A dialect is used to configure what Scala syntax is allowed during tokenization and parsing.
 */
final class Dialect private[meta] (
    // Are `&` intersection types supported by this dialect?
    @deprecated("allowAndTypes unneeded, infix types are supported", "4.5.1")
    private[meta] val allowAndTypes: Boolean = false, // unused
    // Are extractor varargs specified using ats, i.e. is `case Extractor(xs @ _*)` legal or not?
    val allowAtForExtractorVarargs: Boolean = false,
    // Can case classes be declared without a parameter list?
    // Deprecated in 2.10, not supported in 2.11 and newer.
    val allowCaseClassWithoutParameterList: Boolean = false,
    // Are extractor varargs specified using colons, i.e. is `case Extractor(xs: _*)` legal or not?
    val allowColonForExtractorVarargs: Boolean = false,
    // treatment of "a b ()" changed from 2.12 (empty args, aka "nullary") to 2.13 (a Unit arg)
    val allowEmptyInfixArgs: Boolean = true,

    // *** scala3 flags below ***

    // Are enums allowed?
    // They are in Dotty, but not in Scala 2.13 or older.
    val allowEnums: Boolean = false,
    // Are implicit by name parameters supported?
    // They are in Dotty, but not in Scala 2.12 or older.
    val allowImplicitByNameParameters: Boolean = false,
    // Are `inline` identifiers supported by this dialect?
    val allowInlineIdents: Boolean = false,
    // Are inline vals and defs supported by this dialect?
    val allowInlineMods: Boolean = false,
    // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
    val allowLiteralTypes: Boolean = false,
    // Are `|` (union types) supported by this dialect?
    @deprecated("allowOrTypes unneeded, infix types are supported", "4.5.1")
    private[meta] val allowOrTypes: Boolean = false, // unused
    // Are naked underscores allowed after $ in pattern interpolators, i.e. is `case q"$_ + $_" =>` legal or not?
    val allowSpliceUnderscores: Boolean = false,
    // Are terms on the top level supported by this dialect?
    // Necessary to support popular script-like DSLs.
    val allowToplevelTerms: Boolean = false,
    // Are trailing commas allowed? SIP-27.
    val allowTrailingCommas: Boolean = false,
    // Are trait allowed to have parameters?
    // They are in Dotty, but not in Scala 2.13 or older.
    val allowTraitParameters: Boolean = false,
    // Are type lambdas allowed, i.e. is `[T] => (T, T)` legal or not?
    val allowTypeLambdas: Boolean = false,
    // Are view bounds supported by this dialect?
    // def f[A <% Int](a: A)
    // Removed in Dotty.
    @deprecated("allowViewBounds unneeded, it was only used for an error", ">4.10.2")
    private[meta] val allowViewBounds: Boolean = false,
    // Are XML literals supported by this dialect?
    // We plan to deprecate XML literal syntax, and some dialects
    // might go ahead and drop support completely.
    val allowXmlLiterals: Boolean = false,
    @deprecated("toplevelSeparator has never been used", ">4.4.35")
    private[meta] val toplevelSeparator: String = "", // unused
    // Are numeric literal underscore separators, i.e. `1_000_000` legal or not?
    val allowNumericLiteralUnderscoreSeparators: Boolean = false,
    // Can try body contain any expression? (2.13.1 https://github.com/scala/scala/pull/8071)
    val allowTryWithAnyExpr: Boolean = false,
    // Given/using introduced in dotty
    val allowGivenUsing: Boolean = false,
    // https://dotty.epfl.ch/docs/reference/experimental/erased-defs.html
    val allowErasedDefs: Boolean = false,
    // Extension methods introduced in dotty
    val allowExtensionMethods: Boolean = false,
    // Open modifier for classes introduced in dotty
    val allowOpenClass: Boolean = false,
    // Top level statements introduced in dotty.
    // differs from ToplevelTerms because here you can define packages
    val allowToplevelStatements: Boolean = false,
    // Opaque types introduced in dotty
    val allowOpaqueTypes: Boolean = false,
    // Export selected members of an object introduced in dotty
    val allowExportClause: Boolean = false,
    // Extended classes separated by ',' introduced in dotty
    val allowCommaSeparatedExtend: Boolean = false,
    // end marker introduced in dotty
    val allowEndMarker: Boolean = false,
    // Support for escaping `"` in interpolated strings using $ - "$""
    val allowInterpolationDolarQuoteEscape: Boolean = false,
    // Significant identation introduced in dotty
    val allowSignificantIndentation: Boolean = false,
    // Dotty changed wildcard for types from `_` to `?`
    val allowQuestionMarkAsTypeWildcard: Boolean = false,
    // Dotty rejects placeholder as Type parameter
    @deprecated("allowTypeParamUnderscore not used", "4.14.1")
    private[meta] val allowTypeParamUnderscore: Boolean = false,
    // Dotty allows by-name repeated parameters
    val allowByNameRepeatedParameters: Boolean = false,
    // Dotty allows lazy val abstract values
    val allowLazyValAbstractValues: Boolean = false,
    // Dotty allows capital pattern vars in `case A @ _ =>`
    val allowUpperCasePatternVarBinding: Boolean = false,
    // Dotty allows to use derives to automatically generate given instances for type classes
    val allowDerives: Boolean = false,
    // Dotty allows to specify `type T` inside blocks
    val allowTypeInBlock: Boolean = false,
    // Dotty allows to define function like `[T] => (ts: List[T]) => ts.headOption`
    val allowPolymorphicFunctions: Boolean = false,
    // Dotty allows `.match` expressions and chaining matches
    val allowMatchAsOperator: Boolean = false,
    // Dotty allows `match` on type
    val allowTypeMatch: Boolean = false,
    // Dotty allows to define types and methods with an `infix` soft keyword modifier
    val allowInfixMods: Boolean = false,
    // Scala 3 splices/quotes
    val allowSpliceAndQuote: Boolean = false,
    // https://docs.scala-lang.org/sips/quote-pattern-type-variable-syntax.html
    val allowQuotedTypeVariables: Boolean = false,
    // Scala 3 disallowed symbol literals
    val allowSymbolLiterals: Boolean = true,
    // Scala 3 disallowed symbol literals
    val allowDependentFunctionTypes: Boolean = false,
    /* Scala 3 added possibility to use simpler splices such as:
     * val arr = Array(1, 2, 3)
     * val lst = List(0, arr*)                  // vararg splice argument
     *  lst match
     *    case List(0, 1, xs*) => println(xs)   // binds xs to Seq(2, 3)
     *    case List(1, _*) =>                   // wildcard pattern
     */
    val allowPostfixStarVarargSplices: Boolean = false,
    /* Scala 3 allows us to specify:
     * `case tp @ OrNull(tp1): OrType`
     * the last section after : was not allowed previously.
     */
    val allowAllTypedPatterns: Boolean = false,
    // Scala 3 import renames can use as soft keyword `import a.b.C as D`
    val allowAsForImportRename: Boolean = false,
    // Scala 3 wildcard imports can be specified as `import a.b.*`
    val allowStarWildcardImport: Boolean = false,
    // Scala 3 no longer allows def hello(){} - `=` is always needed
    val allowProcedureSyntax: Boolean = true,
    // Scala 3 no longer allows `do {...} while(...)`
    val allowDoWhile: Boolean = true,
    /* Kind-project support
     * works under -Xsource3 flag
     * https://github.com/scala/scala/pull/9605
     */
    val allowPlusMinusUnderscoreAsIdent: Boolean = false,
    // Dotty uses `_` for placeholder for types since 3.2
    val allowUnderscoreAsTypePlaceholder: Boolean = false,
    // Dotty uses `*` for placeholder for types in 3.0-3.2
    val allowStarAsTypePlaceholder: Boolean = false,
    // import a.b.c.{ given, _} used for -X:source3
    val allowGivenImports: Boolean = false,
    // Scala 3 uses proper precedence rules for infix types, unlike Scala 2
    val useInfixTypePrecedence: Boolean = false,
    // Scala213Source3 and Scala3 allow infix operator being placed after nl
    val allowInfixOperatorAfterNL: Boolean = false,
    // Scala 3 allows `def f[X](x: X)[Y](y: Y)`
    val allowParamClauseInterleaving: Boolean = false,
    /* Scala 3 allows dropping braces for block arguments such as `list.map: a =>`
     * It wasn't available in Scala 3.0 and got introduced later.
     */
    val allowFewerBraces: Boolean = false,
    // https://docs.scala-lang.org/scala3/reference/other-new-features/control-syntax.html
    val allowQuietSyntax: Boolean = false,
    // Are binary literals allowed? SIP-42.
    val allowBinaryLiterals: Boolean = false,
    // Are tracked parameters allowed? docs: https://dotty.epfl.ch/docs/reference/experimental/modularity.html
    val allowTrackedParameters: Boolean = false,
    // https://dotty.epfl.ch/docs/reference/experimental/into-modifier.html
    val allowParameterTypeConversions: Boolean = false,
    // https://dotty.epfl.ch/docs/reference/experimental/purefuns.html
    val allowPureFunctions: Boolean = false,
    // https://dotty.epfl.ch/docs/reference/experimental/cc.html
    val allowCaptureChecking: Boolean = false,
    // https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html
    val allowNamedTuples: Boolean = false,
    // https://docs3.scala-lang.org/sips/sips/typeclasses-syntax.html?
    val allowImprovedTypeClassesSyntax: Boolean = false,
    // https://github.com/scala/scala/pull/8282
    val treatUnicodeEscapesAsOrdinary: Boolean = false,

    // NOTE: add new fields above this line
    private[meta] val unquoteParentDialect: Dialect = null,
    // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
    // If yes, they will be parsed as patterns or terms.
    private[meta] val unquoteType: UnquoteType = UnquoteType.None
) extends Product with Serializable {

  // NOTE(olafur) checklist for adding a new dialect field in a binary compatible way:
  // - add new field to primary constructor.
  // - add new parameter to `privateCopy()` method.
  // - add new `withX()` method to allow overriding that specific field.
  // - add new `x = FIELD_DEFAULT_VALUE` in the `def this()` constructor below.

  @deprecated("Use Dialect.empty (or any of the predefined dialects) and `.withXxx` methods")
  def this(
      allowAndTypes: Boolean, // unused
      allowAtForExtractorVarargs: Boolean,
      allowCaseClassWithoutParameterList: Boolean,
      allowColonForExtractorVarargs: Boolean,
      allowEnums: Boolean,
      allowImplicitByNameParameters: Boolean,
      allowInlineIdents: Boolean,
      allowInlineMods: Boolean,
      allowLiteralTypes: Boolean,
      allowMultilinePrograms: Boolean, // unused
      allowOrTypes: Boolean, // unused
      allowPatUnquotes: Boolean, // unused
      allowSpliceUnderscores: Boolean,
      allowTermUnquotes: Boolean, // unused
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      allowViewBounds: Boolean, // unused
      allowXmlLiterals: Boolean,
      toplevelSeparator: String // unused
  ) = this(
    allowAtForExtractorVarargs = allowAtForExtractorVarargs,
    allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
    allowColonForExtractorVarargs = allowColonForExtractorVarargs,
    allowEnums = allowEnums,
    allowImplicitByNameParameters = allowImplicitByNameParameters,
    allowInlineIdents = allowInlineIdents,
    allowInlineMods = allowInlineMods,
    allowLiteralTypes = allowLiteralTypes,
    allowSpliceUnderscores = allowSpliceUnderscores,
    allowToplevelTerms = allowToplevelTerms,
    allowTrailingCommas = allowTrailingCommas,
    allowTraitParameters = allowTraitParameters,
    allowTypeLambdas = allowTypeLambdas,
    allowXmlLiterals = allowXmlLiterals
  )

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  def allowUnquotes: Boolean = null ne unquoteParentDialect
  def allowPatUnquotes: Boolean = unquoteType.isInstanceOf[UnquoteType.Pat]
  def allowTermUnquotes: Boolean = unquoteType.isInstanceOf[UnquoteType.Term]
  def allowMultilinePrograms: Boolean = unquoteType.isMultiline

  @deprecated("allowAndTypes unneeded, infix types are supported", "4.5.1")
  private[meta] def withAllowAndTypes(newValue: Boolean): Dialect = this
  def withAllowAtForExtractorVarargs(newValue: Boolean): Dialect =
    privateCopy(allowAtForExtractorVarargs = newValue)
  def withAllowCaseClassWithoutParameterList(newValue: Boolean): Dialect =
    privateCopy(allowCaseClassWithoutParameterList = newValue)
  def withAllowColonForExtractorVarargs(newValue: Boolean): Dialect =
    privateCopy(allowColonForExtractorVarargs = newValue)
  def withAllowEmptyInfixArgs(newValue: Boolean): Dialect =
    privateCopy(allowEmptyInfixArgs = newValue)

  def withAllowEnums(newValue: Boolean): Dialect = privateCopy(allowEnums = newValue)
  def withAllowImplicitByNameParameters(newValue: Boolean): Dialect =
    privateCopy(allowImplicitByNameParameters = newValue)
  def withAllowInlineIdents(newValue: Boolean): Dialect = privateCopy(allowInlineIdents = newValue)
  def withAllowInlineMods(newValue: Boolean): Dialect = privateCopy(allowInlineMods = newValue)
  def withAllowLiteralTypes(newValue: Boolean): Dialect = privateCopy(allowLiteralTypes = newValue)
  def withAllowMultilinePrograms(newValue: Boolean): Dialect = ???
  @deprecated("allowOrTypes unneeded, infix types are supported", "4.5.1")
  private[meta] def withAllowOrTypes(newValue: Boolean): Dialect = this
  def withAllowPatUnquotes(newValue: Boolean): Dialect = ???
  def withAllowSpliceUnderscores(newValue: Boolean): Dialect =
    privateCopy(allowSpliceUnderscores = newValue)
  def withAllowTermUnquotes(newValue: Boolean): Dialect = ???
  def withAllowToplevelTerms(newValue: Boolean): Dialect = privateCopy(allowToplevelTerms = newValue)
  def withAllowTrailingCommas(newValue: Boolean): Dialect =
    privateCopy(allowTrailingCommas = newValue)
  def withAllowTraitParameters(newValue: Boolean): Dialect =
    privateCopy(allowTraitParameters = newValue)
  def withAllowTypeLambdas(newValue: Boolean): Dialect = privateCopy(allowTypeLambdas = newValue)
  @deprecated("allowViewBounds unneeded, it was only used for an error", ">4.10.2")
  private[meta] def withAllowViewBounds(newValue: Boolean): Dialect = this
  def withAllowXmlLiterals(newValue: Boolean): Dialect = privateCopy(allowXmlLiterals = newValue)
  @deprecated("toplevelSeparator has never been used", ">4.4.35")
  private[meta] def withToplevelSeparator(newValue: String): Dialect = this
  def withAllowNumericLiteralUnderscoreSeparators(newValue: Boolean): Dialect =
    privateCopy(allowNumericLiteralUnderscoreSeparators = newValue)
  def withAllowTryWithAnyExpr(newValue: Boolean): Dialect =
    privateCopy(allowTryWithAnyExpr = newValue)
  def withAllowGivenUsing(newValue: Boolean): Dialect = privateCopy(allowGivenUsing = newValue)
  def withAllowErasedDefs(newValue: Boolean): Dialect = privateCopy(allowErasedDefs = newValue)
  def withAllowExtensionMethods(newValue: Boolean): Dialect =
    privateCopy(allowExtensionMethods = newValue)
  def withAllowOpenClass(newValue: Boolean): Dialect = privateCopy(allowOpenClass = newValue)
  def withAllowToplevelStatements(newValue: Boolean): Dialect =
    privateCopy(allowToplevelStatements = newValue)
  def withAllowOpaqueTypes(newValue: Boolean): Dialect = privateCopy(allowOpaqueTypes = newValue)

  def withAllowInterpolationDolarQuoteEscape(newValue: Boolean): Dialect =
    privateCopy(allowInterpolationDolarQuoteEscape = newValue)
  def withAllowExportClause(newValue: Boolean): Dialect = privateCopy(allowExportClause = newValue)
  def withAllowCommaSeparatedExtend(newValue: Boolean): Dialect =
    privateCopy(allowCommaSeparatedExtend = newValue)
  def withAllowEndMarker(newValue: Boolean): Dialect = privateCopy(allowEndMarker = newValue)
  def withAllowSignificantIndentation(newValue: Boolean): Dialect =
    privateCopy(allowSignificantIndentation = newValue)
  def withAllowQuestionMarkAsTypeWildcard(newValue: Boolean): Dialect =
    privateCopy(allowQuestionMarkAsTypeWildcard = newValue)
  @deprecated("use allowQuestionMarkAsTypeWildcard", ">4.5.13")
  private[meta] def allowQuestionMarkPlaceholder: Boolean = allowQuestionMarkAsTypeWildcard
  @deprecated("use withAllowQuestionMarkAsTypeWildcard", ">4.5.13")
  private[meta] def withAllowQuestionMarkPlaceholder(newValue: Boolean): Dialect =
    withAllowQuestionMarkAsTypeWildcard(newValue)

  @deprecated("allowTypeParamUnderscore not used", "4.14.1")
  def withAllowTypeParamUnderscore(newValue: Boolean): Dialect = this
  def withAllowByNameRepeatedParameters(newValue: Boolean): Dialect =
    privateCopy(allowByNameRepeatedParameters = newValue)
  def withAllowLazyValAbstractValues(newValue: Boolean): Dialect =
    privateCopy(allowLazyValAbstractValues = newValue)
  def withAllowMatchAsOperator(newValue: Boolean): Dialect =
    privateCopy(allowMatchAsOperator = newValue)
  def withAllowUpperCasePatternVarBinding(newValue: Boolean): Dialect =
    privateCopy(allowUpperCasePatternVarBinding = newValue)
  def withAllowDerives(newValue: Boolean): Dialect = privateCopy(allowDerives = newValue)
  def withAllowTypeInBlock(newValue: Boolean): Dialect = privateCopy(allowTypeInBlock = newValue)
  def withAllowPolymorphicFunctions(newValue: Boolean): Dialect =
    privateCopy(allowPolymorphicFunctions = newValue)
  def withAllowTypeMatch(newValue: Boolean): Dialect = privateCopy(allowTypeMatch = newValue)
  def withAllowInfixMods(newValue: Boolean): Dialect = privateCopy(allowInfixMods = newValue)
  def withAllowSpliceAndQuote(newValue: Boolean): Dialect = {
    if (newValue) invariants.require(!this.allowSymbolLiterals)
    privateCopy(allowSpliceAndQuote = newValue)
  }
  def withAllowQuotedTypeVariables(newValue: Boolean): Dialect =
    privateCopy(allowQuotedTypeVariables = newValue)
  def withAllowSymbolLiterals(newValue: Boolean): Dialect = {
    if (newValue) invariants.require(!this.allowSpliceAndQuote)
    privateCopy(allowSymbolLiterals = newValue)
  }
  def withAllowDependentFunctionTypes(newValue: Boolean): Dialect =
    privateCopy(allowDependentFunctionTypes = newValue)
  def withAllowPostfixStarVarargSplices(newValue: Boolean): Dialect =
    privateCopy(allowPostfixStarVarargSplices = newValue)
  def withAllowAllTypedPatterns(newValue: Boolean): Dialect =
    privateCopy(allowAllTypedPatterns = newValue)
  def withAllowAsForImportRename(newValue: Boolean): Dialect =
    privateCopy(allowAsForImportRename = newValue)
  def withAllowStarWildcardImport(newValue: Boolean): Dialect =
    privateCopy(allowStarWildcardImport = newValue)
  def withAllowProcedureSyntax(newValue: Boolean): Dialect =
    privateCopy(allowProcedureSyntax = newValue)
  def withAllowDoWhile(newValue: Boolean): Dialect = privateCopy(allowDoWhile = newValue)

  def withAllowPlusMinusUnderscoreAsIdent(newValue: Boolean): Dialect =
    privateCopy(allowPlusMinusUnderscoreAsIdent = newValue)

  def withAllowUnderscoreAsTypePlaceholder(newValue: Boolean): Dialect =
    privateCopy(allowUnderscoreAsTypePlaceholder = newValue)
  def withAllowStarAsTypePlaceholder(newValue: Boolean): Dialect =
    privateCopy(allowStarAsTypePlaceholder = newValue)
  @deprecated("use withAllowUnderscoreAsTypePlaceholder", ">4.5.13")
  private[meta] def withAllowPlusMinusUnderscoreAsPlaceholder(newValue: Boolean): Dialect =
    withAllowUnderscoreAsTypePlaceholder(newValue)
  @deprecated("use allowUnderscoreAsTypePlaceholder", ">4.5.13")
  private[meta] def allowPlusMinusUnderscoreAsPlaceholder: Boolean =
    allowUnderscoreAsTypePlaceholder

  def withAllowGivenImports(newValue: Boolean): Dialect = privateCopy(allowGivenImports = newValue)

  def withUseInfixTypePrecedence(newValue: Boolean): Dialect =
    privateCopy(useInfixTypePrecedence = newValue)

  def withAllowInfixOperatorAfterNL(newValue: Boolean): Dialect =
    privateCopy(allowInfixOperatorAfterNL = newValue)

  def withAllowParamClauseInterleaving(newValue: Boolean): Dialect =
    privateCopy(allowParamClauseInterleaving = newValue)

  def withAllowFewerBraces(newValue: Boolean): Dialect = privateCopy(allowFewerBraces = newValue)

  def withAllowQuietSyntax(newValue: Boolean): Dialect = privateCopy(allowQuietSyntax = newValue)

  def withAllowBinaryLiterals(newValue: Boolean): Dialect =
    privateCopy(allowBinaryLiterals = newValue)

  def withAllowTrackedParameters(newValue: Boolean): Dialect =
    privateCopy(allowTrackedParameters = newValue)

  def withAllowParameterTypeConversions(newValue: Boolean): Dialect =
    privateCopy(allowParameterTypeConversions = newValue)

  def withAllowPureFunctions(newValue: Boolean): Dialect = privateCopy(allowPureFunctions = newValue)

  def withAllowCaptureChecking(newValue: Boolean): Dialect =
    privateCopy(allowCaptureChecking = newValue)

  def withAllowNamedTuples(newValue: Boolean): Dialect = privateCopy(allowNamedTuples = newValue)

  def withAllowImprovedTypeClassesSyntax(newValue: Boolean): Dialect =
    privateCopy(allowImprovedTypeClassesSyntax = newValue)

  def withTreatUnicodeEscapesAsOrdinary(newValue: Boolean): Dialect =
    privateCopy(treatUnicodeEscapesAsOrdinary = newValue)

  // NOTE(olafur): add the next `withX()` method above this comment. Please try
  // to use consistent formatting, use `newValue` as the parameter name and wrap
  // the body inside curly braces.

  private[this] def privateCopy(
      allowAtForExtractorVarargs: Boolean = this.allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList: Boolean = this.allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs: Boolean = this.allowColonForExtractorVarargs,
      allowEmptyInfixArgs: Boolean = this.allowEmptyInfixArgs,
      allowEnums: Boolean = this.allowEnums,
      allowImplicitByNameParameters: Boolean = this.allowImplicitByNameParameters,
      allowInlineIdents: Boolean = this.allowInlineIdents,
      allowInlineMods: Boolean = this.allowInlineMods,
      allowLiteralTypes: Boolean = this.allowLiteralTypes,
      allowSpliceUnderscores: Boolean = this.allowSpliceUnderscores,
      allowToplevelTerms: Boolean = this.allowToplevelTerms,
      allowTrailingCommas: Boolean = this.allowTrailingCommas,
      allowTraitParameters: Boolean = this.allowTraitParameters,
      allowTypeLambdas: Boolean = this.allowTypeLambdas,
      allowXmlLiterals: Boolean = this.allowXmlLiterals,
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
      allowQuotedTypeVariables: Boolean = this.allowQuotedTypeVariables,
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
      allowFewerBraces: Boolean = this.allowFewerBraces,
      allowQuietSyntax: Boolean = this.allowQuietSyntax,
      allowBinaryLiterals: Boolean = this.allowBinaryLiterals,
      allowTrackedParameters: Boolean = this.allowTrackedParameters,
      allowParameterTypeConversions: Boolean = this.allowParameterTypeConversions,
      allowPureFunctions: Boolean = this.allowPureFunctions,
      allowCaptureChecking: Boolean = this.allowCaptureChecking,
      allowNamedTuples: Boolean = this.allowNamedTuples,
      allowImprovedTypeClassesSyntax: Boolean = this.allowImprovedTypeClassesSyntax,
      treatUnicodeEscapesAsOrdinary: Boolean = this.treatUnicodeEscapesAsOrdinary,
      // NOTE(olafur): add the next parameter above this comment.
      unquoteType: UnquoteType = UnquoteType.None
  ): Dialect = {
    val notForUnquote = unquoteType eq UnquoteType.None
    val that = new Dialect(
      allowAtForExtractorVarargs = allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs = allowColonForExtractorVarargs,
      allowEmptyInfixArgs = allowEmptyInfixArgs,
      allowEnums = allowEnums,
      allowImplicitByNameParameters = allowImplicitByNameParameters,
      allowInlineIdents = allowInlineIdents,
      allowInlineMods = allowInlineMods,
      allowLiteralTypes = allowLiteralTypes,
      allowSpliceUnderscores = allowSpliceUnderscores,
      allowToplevelTerms = allowToplevelTerms,
      allowTrailingCommas = allowTrailingCommas,
      allowTraitParameters = allowTraitParameters,
      allowTypeLambdas = allowTypeLambdas,
      allowXmlLiterals = allowXmlLiterals,
      allowNumericLiteralUnderscoreSeparators = allowNumericLiteralUnderscoreSeparators,
      allowTryWithAnyExpr = allowTryWithAnyExpr,
      allowGivenUsing = allowGivenUsing,
      allowErasedDefs = allowErasedDefs,
      allowExtensionMethods = allowExtensionMethods,
      allowOpenClass = allowOpenClass,
      allowToplevelStatements = allowToplevelStatements,
      allowOpaqueTypes = allowOpaqueTypes,
      allowExportClause = allowExportClause,
      allowCommaSeparatedExtend = allowCommaSeparatedExtend,
      allowEndMarker = allowEndMarker,
      allowInterpolationDolarQuoteEscape = allowInterpolationDolarQuoteEscape,
      allowSignificantIndentation = allowSignificantIndentation,
      allowQuestionMarkAsTypeWildcard = allowQuestionMarkAsTypeWildcard,
      allowByNameRepeatedParameters = allowByNameRepeatedParameters,
      allowLazyValAbstractValues = allowLazyValAbstractValues,
      allowUpperCasePatternVarBinding = allowUpperCasePatternVarBinding,
      allowDerives = allowDerives,
      allowTypeInBlock = allowTypeInBlock,
      allowPolymorphicFunctions = allowPolymorphicFunctions,
      allowMatchAsOperator = allowMatchAsOperator,
      allowTypeMatch = allowTypeMatch,
      allowInfixMods = allowInfixMods,
      allowSpliceAndQuote = allowSpliceAndQuote,
      allowQuotedTypeVariables = allowQuotedTypeVariables,
      allowSymbolLiterals = allowSymbolLiterals,
      allowDependentFunctionTypes = allowDependentFunctionTypes,
      allowPostfixStarVarargSplices = allowPostfixStarVarargSplices,
      allowAllTypedPatterns = allowAllTypedPatterns,
      allowAsForImportRename = allowAsForImportRename,
      allowStarWildcardImport = allowStarWildcardImport,
      allowProcedureSyntax = allowProcedureSyntax,
      allowDoWhile = allowDoWhile,
      allowPlusMinusUnderscoreAsIdent = allowPlusMinusUnderscoreAsIdent,
      allowUnderscoreAsTypePlaceholder = allowUnderscoreAsTypePlaceholder,
      allowStarAsTypePlaceholder = allowStarAsTypePlaceholder,
      allowGivenImports = allowGivenImports,
      useInfixTypePrecedence = useInfixTypePrecedence,
      allowInfixOperatorAfterNL = allowInfixOperatorAfterNL,
      allowParamClauseInterleaving = allowParamClauseInterleaving,
      allowFewerBraces = allowFewerBraces,
      allowQuietSyntax = allowQuietSyntax,
      allowBinaryLiterals = allowBinaryLiterals,
      allowTrackedParameters = allowTrackedParameters,
      allowParameterTypeConversions = allowParameterTypeConversions,
      allowPureFunctions = allowPureFunctions,
      allowCaptureChecking = allowCaptureChecking,
      allowNamedTuples = allowNamedTuples,
      allowImprovedTypeClassesSyntax = allowImprovedTypeClassesSyntax,
      treatUnicodeEscapesAsOrdinary = treatUnicodeEscapesAsOrdinary,
      // NOTE(olafur): add the next argument above this comment.
      unquoteType = unquoteType,
      unquoteParentDialect = if (notForUnquote) null else this
    )
    val equivalent = notForUnquote && isEquivalentToInternal(that)
    if (equivalent) this else that
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
  override def toString = Dialect.inverseStandards.getOrElse(unquoteParentOrThis(), "Dialect()")

  def isEquivalentTo(that: Dialect): Boolean = {
    val thisParent = unquoteParentOrThis()
    val thatParent = that.unquoteParentOrThis()
    (thisParent eq thatParent) || thisParent.isEquivalentToInternal(thatParent)
  }

  private def isEquivalentToInternal(that: Dialect): Boolean =
    // do not include deprecated values in this comparison
    this.allowAtForExtractorVarargs == that.allowAtForExtractorVarargs &&
      this.allowCaseClassWithoutParameterList == that.allowCaseClassWithoutParameterList &&
      this.allowColonForExtractorVarargs == that.allowColonForExtractorVarargs &&
      this.allowEmptyInfixArgs == that.allowEmptyInfixArgs && this.allowEnums == that.allowEnums &&
      this.allowImplicitByNameParameters == that.allowImplicitByNameParameters &&
      this.allowInlineIdents == that.allowInlineIdents &&
      this.allowInlineMods == that.allowInlineMods &&
      this.allowLiteralTypes == that.allowLiteralTypes &&
      this.allowSpliceUnderscores == that.allowSpliceUnderscores &&
      this.allowToplevelTerms == that.allowToplevelTerms &&
      this.allowTrailingCommas == that.allowTrailingCommas &&
      this.allowTraitParameters == that.allowTraitParameters &&
      this.allowTypeLambdas == that.allowTypeLambdas &&
      this.allowXmlLiterals == that.allowXmlLiterals &&
      this.allowNumericLiteralUnderscoreSeparators ==
      that.allowNumericLiteralUnderscoreSeparators &&
      this.allowTryWithAnyExpr == that.allowTryWithAnyExpr &&
      this.allowGivenUsing == that.allowGivenUsing &&
      this.allowErasedDefs == that.allowErasedDefs &&
      this.allowExtensionMethods == that.allowExtensionMethods &&
      this.allowOpenClass == that.allowOpenClass &&
      this.allowToplevelStatements == that.allowToplevelStatements &&
      this.allowOpaqueTypes == that.allowOpaqueTypes &&
      this.allowExportClause == that.allowExportClause &&
      this.allowCommaSeparatedExtend == that.allowCommaSeparatedExtend &&
      this.allowEndMarker == that.allowEndMarker &&
      this.allowInterpolationDolarQuoteEscape == that.allowInterpolationDolarQuoteEscape &&
      this.allowSignificantIndentation == that.allowSignificantIndentation &&
      this.allowQuestionMarkAsTypeWildcard == that.allowQuestionMarkAsTypeWildcard &&
      this.allowByNameRepeatedParameters == that.allowByNameRepeatedParameters &&
      this.allowLazyValAbstractValues == that.allowLazyValAbstractValues &&
      this.allowUpperCasePatternVarBinding == that.allowUpperCasePatternVarBinding &&
      this.allowDerives == that.allowDerives && this.allowTypeInBlock == that.allowTypeInBlock &&
      this.allowPolymorphicFunctions == that.allowPolymorphicFunctions &&
      this.allowMatchAsOperator == that.allowMatchAsOperator &&
      this.allowTypeMatch == that.allowTypeMatch && this.allowInfixMods == that.allowInfixMods &&
      this.allowSpliceAndQuote == that.allowSpliceAndQuote &&
      this.allowQuotedTypeVariables == that.allowQuotedTypeVariables &&
      this.allowSymbolLiterals == that.allowSymbolLiterals &&
      this.allowDependentFunctionTypes == that.allowDependentFunctionTypes &&
      this.allowPostfixStarVarargSplices == that.allowPostfixStarVarargSplices &&
      this.allowAllTypedPatterns == that.allowAllTypedPatterns &&
      this.allowAsForImportRename == that.allowAsForImportRename &&
      this.allowStarWildcardImport == that.allowStarWildcardImport &&
      this.allowProcedureSyntax == that.allowProcedureSyntax &&
      this.allowDoWhile == that.allowDoWhile &&
      this.allowPlusMinusUnderscoreAsIdent == that.allowPlusMinusUnderscoreAsIdent &&
      this.allowUnderscoreAsTypePlaceholder == that.allowUnderscoreAsTypePlaceholder &&
      this.allowStarAsTypePlaceholder == that.allowStarAsTypePlaceholder &&
      this.allowGivenImports == that.allowGivenImports &&
      this.useInfixTypePrecedence == that.useInfixTypePrecedence &&
      this.allowInfixOperatorAfterNL == that.allowInfixOperatorAfterNL &&
      this.allowParamClauseInterleaving == that.allowParamClauseInterleaving &&
      this.allowQuietSyntax == that.allowQuietSyntax && // separated from "significant indentation"
      this.allowFewerBraces == that.allowFewerBraces &&
      this.allowBinaryLiterals == that.allowBinaryLiterals &&
      this.allowTrackedParameters == that.allowTrackedParameters &&
      this.allowParameterTypeConversions == that.allowParameterTypeConversions &&
      this.allowPureFunctions == that.allowPureFunctions &&
      this.allowCaptureChecking == that.allowCaptureChecking &&
      this.allowNamedTuples == that.allowNamedTuples &&
      this.treatUnicodeEscapesAsOrdinary == that.treatUnicodeEscapesAsOrdinary &&
      this.allowImprovedTypeClassesSyntax == that.allowImprovedTypeClassesSyntax

  @deprecated("Use withX method instead", "4.3.11")
  private[meta] def copy(
      allowAndTypes: Boolean = true, // unused
      allowAtForExtractorVarargs: Boolean = this.allowAtForExtractorVarargs,
      allowCaseClassWithoutParameterList: Boolean = this.allowCaseClassWithoutParameterList,
      allowColonForExtractorVarargs: Boolean = this.allowColonForExtractorVarargs,
      allowEnums: Boolean = this.allowEnums,
      allowImplicitByNameParameters: Boolean = this.allowImplicitByNameParameters,
      allowInlineIdents: Boolean = this.allowInlineIdents,
      allowInlineMods: Boolean = this.allowInlineMods,
      allowLiteralTypes: Boolean = this.allowLiteralTypes,
      allowMultilinePrograms: Boolean = true, // unused
      allowOrTypes: Boolean = true, // unused
      allowPatUnquotes: Boolean = false, // unused
      allowSpliceUnderscores: Boolean = this.allowSpliceUnderscores,
      allowTermUnquotes: Boolean = false, // unused
      allowToplevelTerms: Boolean = this.allowToplevelTerms,
      allowTrailingCommas: Boolean = this.allowTrailingCommas,
      allowTraitParameters: Boolean = this.allowTraitParameters,
      allowTypeLambdas: Boolean = this.allowTypeLambdas,
      allowViewBounds: Boolean = true, // unused
      allowXmlLiterals: Boolean = this.allowXmlLiterals,
      toplevelSeparator: String = "" // unused
  ): Dialect = privateCopy(
    unquoteType = UnquoteType.None,
    allowAtForExtractorVarargs = allowAtForExtractorVarargs,
    allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
    allowColonForExtractorVarargs = allowColonForExtractorVarargs,
    allowEnums = allowEnums,
    allowImplicitByNameParameters = allowImplicitByNameParameters,
    allowInlineIdents = allowInlineIdents,
    allowInlineMods = allowInlineMods,
    allowLiteralTypes = allowLiteralTypes,
    allowSpliceUnderscores = allowSpliceUnderscores,
    allowToplevelTerms = allowToplevelTerms,
    allowTrailingCommas = allowTrailingCommas,
    allowTraitParameters = allowTraitParameters,
    allowTypeLambdas = allowTypeLambdas,
    allowXmlLiterals = allowXmlLiterals
  )

  private[meta] def unquoteParentOrThis(): Dialect =
    if (null eq unquoteParentDialect) this else unquoteParentDialect

  private[meta] def unquote(unquoteType: UnquoteType): Dialect = {
    require(null eq unquoteParentDialect)
    privateCopy(unquoteType = unquoteType, allowTypeLambdas = true)
  }

  private[meta] def unquoteTerm(multiline: Boolean): Dialect = unquote(UnquoteType.Term(multiline))

  private[meta] def unquotePat(multiline: Boolean): Dialect = unquote(UnquoteType.Pat(multiline))

}

object Dialect extends InternalDialect {
  val empty: Dialect = new Dialect().withAllowEmptyInfixArgs(false).withAllowSymbolLiterals(false)
    .withAllowProcedureSyntax(false).withAllowDoWhile(false)

  @deprecated("Use Dialect.empty (or any of the predefined dialects) and `.withXxx` methods")
  def apply(
      @deprecated("allowAndTypes unneeded, infix types are supported", "4.5.1")
      allowAndTypes: Boolean, // unused
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
      allowOrTypes: Boolean, // unused
      allowPatUnquotes: Boolean = false, // unused
      allowSpliceUnderscores: Boolean,
      allowTermUnquotes: Boolean = false, // unused
      allowToplevelTerms: Boolean,
      allowTrailingCommas: Boolean,
      allowTraitParameters: Boolean,
      allowTypeLambdas: Boolean,
      @deprecated("allowViewBounds unneeded, it was only used for an error", ">4.10.2")
      allowViewBounds: Boolean,
      allowXmlLiterals: Boolean,
      @deprecated("toplevelSeparator has never been used", ">4.4.35")
      toplevelSeparator: String // unused
  ): Dialect = new Dialect(
    allowAtForExtractorVarargs = allowAtForExtractorVarargs,
    allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList,
    allowColonForExtractorVarargs = allowColonForExtractorVarargs,
    allowEnums = allowEnums,
    allowImplicitByNameParameters = allowImplicitByNameParameters,
    allowInlineIdents = allowInlineIdents,
    allowInlineMods = allowInlineMods,
    allowLiteralTypes = allowLiteralTypes,
    allowSpliceUnderscores = allowSpliceUnderscores,
    allowToplevelTerms = allowToplevelTerms,
    allowTrailingCommas = allowTrailingCommas,
    allowTraitParameters = allowTraitParameters,
    allowTypeLambdas = allowTypeLambdas,
    allowXmlLiterals = allowXmlLiterals
  )
  private lazy val standardPairs = Seq[sourcecode.Text[Dialect]](
    Dotty,
    Scala3Future,
    Scala3,
    Scala30,
    Scala31,
    Scala32,
    Scala33,
    Scala34,
    Scala35,
    Scala36,
    Scala37,
    Scala38,
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
  private[meta] lazy val standards: Map[String, Dialect] = standardPairs
    .map(x => x.source -> x.value).toMap
  private[meta] lazy val inverseStandards: Map[Dialect, String] = standardPairs
    .map(x => x.value -> x.source).toMap

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
