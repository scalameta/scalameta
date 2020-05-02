package scala.meta

import scala.meta.dialects._
import scala.meta.internal.dialects._

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
    // Are implicit functions supported by this dialect?
    val allowImplicitFunctionTypes: Boolean,
    // Are `inline` identifiers supported by this dialect?
    val allowInlineIdents: Boolean,
    // Are inline vals and defs supported by this dialect?
    val allowInlineMods: Boolean,
    // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
    val allowLiteralTypes: Boolean,
    // Are method types allowed, i.e. is `(x: X): x.T` legal or not?
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
    // Removed in Dotty.
    val allowViewBounds: Boolean,
    // Are `with` intersection types supported by this dialect?
    val allowWithTypes: Boolean,
    // Are XML literals supported by this dialect?
    // We plan to deprecate XML literal syntax, and some dialects
    // might go ahead and drop support completely.
    val allowXmlLiterals: Boolean,
    // What kind of separator is necessary to split top-level statements?
    // Normally none is required, but scripts may have their own rules.
    val toplevelSeparator: String,
    // Are numeric literal underscore separators, i.e. `1_000_000` legal or not?
    val allowNumericLiteralUnderscoreSeparators: Boolean
) extends Product
    with Serializable {

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
      allowNumericLiteralUnderscoreSeparators = false
      // NOTE(olafur): declare the default value for new fields above this comment.
    )
  }

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  def allowUnquotes: Boolean = allowTermUnquotes || allowPatUnquotes

  def withAllowAndTypes(allowAndTypes: Boolean): Dialect = {
    privateCopy(allowAndTypes = allowAndTypes)
  }
  def withAllowAtForExtractorVarargs(allowAtForExtractorVarargs: Boolean): Dialect = {
    privateCopy(allowAtForExtractorVarargs = allowAtForExtractorVarargs)
  }
  def withAllowCaseClassWithoutParameterList(
      allowCaseClassWithoutParameterList: Boolean
  ): Dialect = {
    privateCopy(allowCaseClassWithoutParameterList = allowCaseClassWithoutParameterList)
  }
  def withAllowColonForExtractorVarargs(allowColonForExtractorVarargs: Boolean): Dialect = {
    privateCopy(allowColonForExtractorVarargs = allowColonForExtractorVarargs)
  }
  def withAllowEnums(allowEnums: Boolean): Dialect = {
    privateCopy(allowEnums = allowEnums)
  }
  def withAllowImplicitByNameParameters(allowImplicitByNameParameters: Boolean): Dialect = {
    privateCopy(allowImplicitByNameParameters = allowImplicitByNameParameters)
  }
  def withAllowImplicitFunctionTypes(allowImplicitFunctionTypes: Boolean): Dialect = {
    privateCopy(allowImplicitFunctionTypes = allowImplicitFunctionTypes)
  }
  def withAllowInlineIdents(allowInlineIdents: Boolean): Dialect = {
    privateCopy(allowInlineIdents = allowInlineIdents)
  }
  def withAllowInlineMods(allowInlineMods: Boolean): Dialect = {
    privateCopy(allowInlineMods = allowInlineMods)
  }
  def withAllowLiteralTypes(allowLiteralTypes: Boolean): Dialect = {
    privateCopy(allowLiteralTypes = allowLiteralTypes)
  }
  def withAllowMethodTypes(allowMethodTypes: Boolean): Dialect = {
    privateCopy(allowMethodTypes = allowMethodTypes)
  }
  def withAllowMultilinePrograms(allowMultilinePrograms: Boolean): Dialect = {
    privateCopy(allowMultilinePrograms = allowMultilinePrograms)
  }
  def withAllowOrTypes(allowOrTypes: Boolean): Dialect = {
    privateCopy(allowOrTypes = allowOrTypes)
  }
  def withAllowPatUnquotes(allowPatUnquotes: Boolean): Dialect = {
    privateCopy(allowPatUnquotes = allowPatUnquotes)
  }
  def withAllowSpliceUnderscores(allowSpliceUnderscores: Boolean): Dialect = {
    privateCopy(allowSpliceUnderscores = allowSpliceUnderscores)
  }
  def withAllowTermUnquotes(allowTermUnquotes: Boolean): Dialect = {
    privateCopy(allowTermUnquotes = allowTermUnquotes)
  }
  def withAllowToplevelTerms(allowToplevelTerms: Boolean): Dialect = {
    privateCopy(allowToplevelTerms = allowToplevelTerms)
  }
  def withAllowTrailingCommas(allowTrailingCommas: Boolean): Dialect = {
    privateCopy(allowTrailingCommas = allowTrailingCommas)
  }
  def withAllowTraitParameters(allowTraitParameters: Boolean): Dialect = {
    privateCopy(allowTraitParameters = allowTraitParameters)
  }
  def withAllowTypeLambdas(allowTypeLambdas: Boolean): Dialect = {
    privateCopy(allowTypeLambdas = allowTypeLambdas)
  }
  def withAllowViewBounds(allowViewBounds: Boolean): Dialect = {
    privateCopy(allowViewBounds = allowViewBounds)
  }
  def withAllowWithTypes(allowWithTypes: Boolean): Dialect = {
    privateCopy(allowWithTypes = allowWithTypes)
  }
  def withAllowXmlLiterals(allowXmlLiterals: Boolean): Dialect = {
    privateCopy(allowXmlLiterals = allowXmlLiterals)
  }
  def withToplevelSeparator(toplevelSeparator: String): Dialect = {
    privateCopy(toplevelSeparator = toplevelSeparator)
  }
  def withAllowNumericLiteralUnderscoreSeparators(
      allowNumericLiteralUnderscoreSeparators: Boolean
  ): Dialect = {
    privateCopy(allowNumericLiteralUnderscoreSeparators = allowNumericLiteralUnderscoreSeparators)
  }

  // NOTE(olafur): add the next `withX()` method above this comment.

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
        this.allowNumericLiteralUnderscoreSeparators
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
      allowNumericLiteralUnderscoreSeparators
    )
  }

  // NOTE(olafur): Do not edit below here, these methods can remain unchanged.

  override def productPrefix: String = "Dialect"
  def productArity: Int = 0
  def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString())

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
      case None => s"Dialect(/* with custom parameters */)"
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
    "Scala213" -> Scala213,
    "Typelevel211" -> Typelevel211,
    "Typelevel212" -> Typelevel212
  )
}
