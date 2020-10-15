package scala.meta

import org.scalameta.data._
import org.scalameta.invariants._
import scala.meta.dialects._
import scala.meta.internal.dialects._
import scala.compat.Platform.EOL

package object dialects {
  implicit val Scala210 = Dialect(
    allowAndTypes = false,
    allowAtForExtractorVarargs = true,
    allowCaseClassWithoutParameterList = true,
    allowColonForExtractorVarargs = false,
    allowEnums = false,
    allowImplicitByNameParameters = false,
    allowImplicitFunctionTypes = false,
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
    allowWithTypes = true,
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

  implicit val Scala213 = Scala212
    .withAllowImplicitByNameParameters(true)
    .withAllowLiteralTypes(true)
    .withAllowNumericLiteralUnderscoreSeparators(true)
    .withAllowLiteralUnitType(true)
    .withAllowTryWithAnyExpr(true)

  implicit val Scala = Scala213 // alias for latest Scala dialect.

  implicit val Sbt0136 = Scala210
    .withAllowToplevelTerms(true)
    .withToplevelSeparator(EOL)

  implicit val Sbt0137 = Sbt0136
    .withToplevelSeparator("")

  implicit val Sbt1 = Scala212
    .withAllowToplevelTerms(true)
    .withToplevelSeparator("")

  implicit val Sbt = Sbt1 // alias for latest Sbt dialect.

  implicit val Typelevel212 = Scala212
    .withAllowLiteralTypes(true)

  @deprecated("Scalameta macro annotations are no longer supported", "4.3.11")
  implicit val Paradise212 = Scala212
    .withAllowInlineMods(true)

  @deprecated("Scalameta macro annotations are no longer supported", "4.3.11")
  implicit val ParadiseTypelevel212 = Typelevel212
    .withAllowInlineMods(true)

  implicit val Dotty = Scala213
    .withAllowAndTypes(true) // New feature in Dotty
    .withAllowAtForExtractorVarargs(false)
    .withAllowColonForExtractorVarargs(true) // New feature in Dotty
    .withAllowEnums(true) // New feature in Dotty
    .withAllowImplicitByNameParameters(true) // New feature in Dotty
    .withAllowImplicitFunctionTypes(true) // New feature in Dotty
    .withAllowInlineMods(true) // New feature in Dotty
    .withAllowLiteralTypes(true) // New feature in Dotty
    .withAllowMethodTypes(false)
    .withAllowOrTypes(true) // New feature in Dotty
    .withAllowTrailingCommas(true)
    .withAllowTraitParameters(true) // New feature in Dotty
    .withAllowTypeLambdas(true) // New feature in Dotty
    .withAllowViewBounds(false) // View bounds have been removed in Dotty
    .withAllowWithTypes(false)
    .withAllowXmlLiterals(false) // Dotty parser doesn't support xml
    .withAllowGivenUsing(true)
    .withAllowExtensionMethods(true)
    .withAllowOpenClass(true)
    .withAllowWhiteboxMacro(true)
    .withAllowToplevelStatements(true)
    .withAllowOpaqueTypes(true)
    .withAllowLiteralUnitType(false)
    .withAllowSuperTrait(true)
    .withAllowExportClause(true)
    .withAllowCommaSeparatedExtend(true)
    .withAllowEndMarker(true)
    .withAllowInterpolationDolarQuoteEscape(true)
    .withAllowSignificantIndentation(true)
    .withAllowQuestionMarkPlaceholder(true)
    .withAllowTypeParamUnderscore(false)
    .withAllowByNameRepeatedParameters(true)
    .withAllowLazyValAbstractValues(true)
    .withAllowAsPatternBinding(true)
    .withAllowUpperCasePatternVarBinding(true)
    .withAllowDerives(true)
    .withAllowTypeInBlock(true)

  private[meta] def QuasiquoteTerm(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(
      allowTermUnquotes = true,
      allowMethodTypes = true,
      allowMultilinePrograms = multiline,
      allowTypeLambdas = true
    )
  }

  private[meta] def QuasiquotePat(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(
      allowPatUnquotes = true,
      allowMethodTypes = true,
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
