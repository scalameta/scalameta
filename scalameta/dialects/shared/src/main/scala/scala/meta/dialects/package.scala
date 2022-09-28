package scala.meta

import org.scalameta.invariants._

package object dialects {
  implicit val Scala210 = Dialect(
    allowAndTypes = false,
    allowAtForExtractorVarargs = true,
    allowCaseClassWithoutParameterList = true,
    allowColonForExtractorVarargs = false,
    allowEnums = false,
    allowImplicitByNameParameters = false,
    allowInlineIdents = true,
    allowInlineMods = false,
    allowLiteralTypes = false,
    allowMethodTypes = false,
    allowMultilinePrograms = true,
    allowOrTypes = false,
    allowPatUnquotes = false,
    allowSpliceUnderscores = false, // SI-7715, only fixed in 2.11.0-M5
    allowTermUnquotes = false,
    allowToplevelTerms = false,
    allowTrailingCommas = false,
    allowTraitParameters = false,
    allowTypeLambdas = false,
    allowViewBounds = true,
    allowXmlLiterals = true, // Not even deprecated yet, so we need to support xml literals
    toplevelSeparator = ""
  )

  implicit val Scala211 = Scala210
    .withAllowCaseClassWithoutParameterList(false)
    .withAllowSpliceUnderscores(true) // SI-7715, only fixed in 2.11.0-M5

  implicit val Typelevel211 = Scala211
    .withAllowLiteralTypes(true)

  @deprecated("Scalameta macro annotations are no longer supported", "4.3.11")
  implicit val Paradise211 = Scala211
    .withAllowInlineMods(true)

  @deprecated("Scalameta macro annotations are no longer supported", "4.3.11")
  implicit val ParadiseTypelevel211 = Typelevel211
    .withAllowInlineMods(true)

  implicit val Scala212 = Scala211
    .withAllowTrailingCommas(true)
    .withAllowQuestionMarkAsTypeWildcard(true)

  implicit val Scala213 = Scala212
    .withAllowImplicitByNameParameters(true)
    .withAllowLiteralTypes(true)
    .withAllowNumericLiteralUnderscoreSeparators(true)
    .withAllowTryWithAnyExpr(true)

  /**
   * Dialect starting with Scala 2.13.6 for `-Xsource:3` option
   */
  implicit val Scala213Source3 = Scala213
    .withAllowAsForImportRename(true)
    .withAllowStarWildcardImport(true)
    .withAllowOpenClass(true)
    .withAllowInfixMods(true)
    .withAllowPostfixStarVarargSplices(true)
    .withAllowPlusMinusUnderscoreAsIdent(true)
    .withAllowGivenImports(true)
    .withAllowInfixOperatorAfterNL(true)

  /**
   * Dialect starting with Scala 2.12.14 for `-Xsource:3` option
   */
  implicit val Scala212Source3 = Scala212
    .withAllowAsForImportRename(true)
    .withAllowStarWildcardImport(true)
    .withAllowOpenClass(true)
    .withAllowInfixMods(true)
    .withAllowPostfixStarVarargSplices(true)
    .withAllowPlusMinusUnderscoreAsIdent(true)
    .withAllowGivenImports(true)

  implicit val Scala = Scala213 // alias for latest Scala dialect.

  implicit val Sbt0136 = Scala210
    .withAllowToplevelTerms(true)

  implicit val Sbt0137 = Scala211
    .withAllowToplevelTerms(true)

  implicit val Sbt1 = Scala212
    .withAllowToplevelTerms(true)

  implicit val Sbt = Sbt1 // alias for latest Sbt dialect.

  implicit val Typelevel212 = Scala212
    .withAllowLiteralTypes(true)

  @deprecated("Scalameta macro annotations are no longer supported", "4.3.11")
  implicit val Paradise212 = Scala212
    .withAllowInlineMods(true)

  @deprecated("Scalameta macro annotations are no longer supported", "4.3.11")
  implicit val ParadiseTypelevel212 = Typelevel212
    .withAllowInlineMods(true)

  implicit val Scala30 = Scala213
    // there 3 different ways to specify vargs, some will be removed in future Scala 3 versions
    .withAllowAtForExtractorVarargs(true) // both @ and : work currently for Scala 3
    .withAllowColonForExtractorVarargs(true) // both @ and : work currently for Scala 3
    .withAllowPostfixStarVarargSplices(true)
    .withAllowEnums(true)
    .withAllowImplicitByNameParameters(true)
    .withAllowInlineMods(true)
    .withAllowLiteralTypes(true)
    .withAllowTrailingCommas(true)
    .withAllowTraitParameters(true)
    .withAllowTypeLambdas(true)
    .withAllowViewBounds(false) // View bounds have been removed in Dotty
    .withAllowGivenUsing(true)
    .withAllowExtensionMethods(true)
    .withAllowOpenClass(true)
    .withAllowSpliceAndQuote(true)
    .withAllowToplevelStatements(true)
    .withAllowOpaqueTypes(true)
    .withAllowExportClause(true)
    .withAllowCommaSeparatedExtend(true)
    .withAllowEndMarker(true)
    .withAllowInterpolationDolarQuoteEscape(true)
    .withAllowSignificantIndentation(true)
    .withAllowTypeParamUnderscore(false)
    .withAllowByNameRepeatedParameters(true)
    .withAllowLazyValAbstractValues(true)
    .withAllowUpperCasePatternVarBinding(true)
    .withAllowDerives(true)
    .withAllowTypeInBlock(true)
    .withAllowPolymorphicFunctions(true)
    .withAllowMatchAsOperator(true)
    .withAllowTypeMatch(true)
    .withAllowInfixMods(true)
    .withAllowDependentFunctionTypes(true)
    .withAllowAllTypedPatterns(true)
    .withAllowAsForImportRename(true)
    .withAllowStarWildcardImport(true)
    .withAllowProcedureSyntax(false)
    .withAllowDoWhile(false)
    .withAllowStarAsTypePlaceholder(true)
    .withUseInfixTypePrecedence(true)
    .withAllowInfixOperatorAfterNL(true)

  implicit val Scala31 = Scala30

  implicit val Scala32 = Scala31

  implicit val Scala3 = Scala32

  implicit val Scala3Future = Scala3
    .withAllowUnderscoreAsTypePlaceholder(true)

  @deprecated("Use Scala3 instead", "4.4.2")
  implicit val Dotty = Scala3

  private[meta] def QuasiquoteTerm(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(
      allowTermUnquotes = true,
      allowMultilinePrograms = multiline,
      allowTypeLambdas = true
    )
  }

  private[meta] def QuasiquotePat(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(
      allowPatUnquotes = true,
      allowMultilinePrograms = multiline,
      allowTypeLambdas = true
    )
  }
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait DialectLiftables {
  val c: scala.reflect.macros.blackbox.Context

  import c.universe._
  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  implicit lazy val liftDialect: Liftable[Dialect] = Liftable { dialect =>
    Dialect.standards.find(_._2 == dialect) match {
      case Some((name, _)) =>
        q"_root_.scala.meta.dialects.`package`.${TermName(name)}"
      case _ =>
        val fields = dialect.productIterator.toList.map {
          case null => q"null"
          case f: Boolean => q"$f"
          case f: String => q"$f"
          case f => sys.error(s"unsupported field $f of type ${f.getClass}")
        }
        q"_root_.scala.meta.Dialect(..$fields)"
    }
  }
}
