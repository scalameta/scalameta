package scala.meta

import org.scalameta.data._
import org.scalameta.invariants._
import scala.meta.dialects._
import scala.meta.internal.dialects._
import scala.compat.Platform.EOL

// NOTE: can't put Dialect into scala.meta.Dialects
// because then implicit scope for Dialect lookups will contain members of the package object
// i.e. both Scala211 and Dotty, which is definitely not what we want
@data class Dialect(
  // Are `&` intersection types supported by this dialect?
  allowAndTypes: Boolean,

  // Are extractor varargs specified using ats, i.e. is `case Extractor(xs @ _*)` legal or not?
  allowAtForExtractorVarargs: Boolean,

  // Can case classes be declared without a parameter list?
  // Deprecated in 2.10, not supported in 2.11 and newer.
  allowCaseClassWithoutParameterList: Boolean,

  // Are extractor varargs specified using colons, i.e. is `case Extractor(xs: _*)` legal or not?
  allowColonForExtractorVarargs: Boolean,

  // Are enums allowed?
  // They are in Dotty, but not in Scala 2.12 or older.
  allowEnums: Boolean,

  // Are implicit by name parameters supported?
  // They are in Dotty, but not in Scala 2.12 or older.
  allowImplicitByNameParameters: Boolean,

  // Are implicit functions supported by this dialect?
  allowImplicitFunctionTypes: Boolean,

  // Are `inline` identifiers supported by this dialect?
  allowInlineIdents: Boolean,

  // Are inline vals and defs supported by this dialect?
  allowInlineMods: Boolean,

  // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
  allowLiteralTypes: Boolean,

  // Are method types allowed, i.e. is `(x: X): x.T` legal or not?
  allowMethodTypes: Boolean,

  // Are multiline programs allowed?
  // Some quasiquotes only support single-line snippets.
  allowMultilinePrograms: Boolean,

  // Are `|` (union types) supported by this dialect?
  allowOrTypes: Boolean,

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  // If yes, they will be parsed as patterns.
  allowPatUnquotes: Boolean,

  // Are naked underscores allowed after $ in pattern interpolators, i.e. is `case q"$_ + $_" =>` legal or not?
  allowSpliceUnderscores: Boolean,

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  // If yes, they will be parsed as terms.
  allowTermUnquotes: Boolean,

  // Are terms on the top level supported by this dialect?
  // Necessary to support popular script-like DSLs.
  allowToplevelTerms: Boolean,

  // Are trailing commas allowed? SIP-27.
  allowTrailingCommas: Boolean,

  // Are trait allowed to have parameters?
  // They are in Dotty, but not in Scala 2.12 or older.
  allowTraitParameters: Boolean,

  // Are type lambdas allowed, i.e. is `[T] => (T, T)` legal or not?
  allowTypeLambdas: Boolean,

  // Are view bounds supported by this dialect?
  // Removed in Dotty.
  allowViewBounds: Boolean,

  // Are `with` intersection types supported by this dialect?
  allowWithTypes: Boolean,

  // Are XML literals supported by this dialect?
  // We plan to deprecate XML literal syntax, and some dialects
  // might go ahead and drop support completely.
  allowXmlLiterals: Boolean,

  // What kind of separator is necessary to split top-level statements?
  // Normally none is required, but scripts may have their own rules.
  toplevelSeparator: String
) {
  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  def allowUnquotes: Boolean = allowTermUnquotes || allowPatUnquotes

  // Dialects have reference equality semantics,
  // because sometimes dialects representing distinct Scala versions
  // can be structurally equal to each other.
  override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def equals(other: Any): Boolean = this eq other.asInstanceOf[AnyRef]
  override def hashCode: Int = System.identityHashCode(this)

  // Smart prettyprinting that knows about standard dialects.
  override def toString = {
    Dialect.standards.find(_._2 == this) match {
      case Some((name, _)) => name
      case None => s"Dialect(${this.productIterator.map(_.toString).mkString(", ")})"
    }
  }
}

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

  implicit val Scala211 = Scala210.copy(
    allowCaseClassWithoutParameterList = false,
    allowSpliceUnderscores = true // SI-7715, only fixed in 2.11.0-M5
  )

  implicit val Typelevel211 = Scala211.copy(
    allowLiteralTypes = true
  )

  implicit val Paradise211 = Scala211.copy(
    allowInlineIdents = true,
    allowInlineMods = true
  )

  implicit val ParadiseTypelevel211 = Typelevel211.copy(
    allowInlineIdents = true,
    allowInlineMods = true
  )

  implicit val Scala212 = Scala211.copy(
    // NOTE: support for literal types is tentatively scheduled for 2.12.5
    // https://github.com/scala/scala/pull/5311#issuecomment-290617202
    allowLiteralTypes = false,
    allowTrailingCommas = true
  )

  implicit val Scala = Scala212 // alias for latest Scala dialect.

  implicit val Sbt0136 = Scala210.copy(
    allowToplevelTerms = true,
    toplevelSeparator = EOL
  )

  implicit val Sbt0137 = Scala210.copy(
    allowToplevelTerms = true,
    toplevelSeparator = ""
  )

  implicit val Sbt1 = Scala212.copy(
    allowToplevelTerms = true,
    toplevelSeparator = ""
  )

  implicit val Sbt = Sbt1 // alias for latest Sbt dialect.

  implicit val Typelevel212 = Scala212.copy(
    allowLiteralTypes = true
  )

  implicit val Paradise212 = Scala212.copy(
    allowInlineIdents = true,
    allowInlineMods = true
  )

  implicit val ParadiseTypelevel212 = Typelevel212.copy(
    allowInlineIdents = true,
    allowInlineMods = true
  )

  implicit val Dotty = Scala212.copy(
    allowAndTypes = true, // New feature in Dotty
    allowAtForExtractorVarargs = false, // New feature in Dotty
    allowColonForExtractorVarargs = true, // New feature in Dotty
    allowEnums = true, // New feature in Dotty
    allowImplicitByNameParameters = true, // New feature in Dotty
    allowImplicitFunctionTypes = true, // New feature in Dotty
    allowInlineIdents = false, // New feature in Dotty
    allowInlineMods = true, // New feature in Dotty
    allowLiteralTypes = true, // New feature in Dotty
    allowMethodTypes = false,
    allowOrTypes = true, // New feature in Dotty
    allowTrailingCommas = true,
    allowTraitParameters = true, // New feature in Dotty
    allowTypeLambdas = true, // New feature in Dotty
    allowViewBounds = false, // View bounds have been removed in Dotty
    allowWithTypes = false, // New feature in Dotty
    allowXmlLiterals = false // Dotty parser doesn't have the corresponding code, so it can't really support xml literals
  )

  private[meta] def QuasiquoteTerm(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(allowTermUnquotes = true, allowMethodTypes = true, allowMultilinePrograms = multiline, allowTypeLambdas = true)
  }

  private[meta] def QuasiquotePat(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(allowPatUnquotes = true, allowMethodTypes = true, allowMultilinePrograms = multiline, allowTypeLambdas = true)
  }
}

object Dialect extends InternalDialect {
  // NOTE: Spinning up a macro just for this is too hard.
  // Using JVM reflection won't be portable to Scala.js.
  private[meta] lazy val standards: Map[String, Dialect] = Map(
    "Dotty" -> Dotty,
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
    "Typelevel211" -> Typelevel211,
    "Typelevel212" -> Typelevel212
  )
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
