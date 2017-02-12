package scala.meta

import scala.annotation.implicitNotFound
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.data._
import org.scalameta.invariants._
import scala.meta.dialects._
import scala.meta.internal.dialects._
import scala.language.experimental.macros
import scala.compat.Platform.EOL

// NOTE: can't put Dialect into scala.meta.Dialects
// because then implicit scope for Dialect lookups will contain members of the package object
// i.e. both Scala211 and Dotty, which is definitely not what we want
@data class Dialect(
  // Are multiline programs allowed?
  // Some quasiquotes only support single-line snippets.
  allowMultilinePrograms: Boolean,

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  // If yes, they will be parsed as terms.
  allowTermUnquotes: Boolean,

  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  // If yes, they will be parsed as patterns.
  allowPatUnquotes: Boolean,

  // The sequence of characters that's used to express a bind
  // to a sequence wildcard pattern.
  bindToSeqWildcardDesignator: String,

  // Are naked underscores allowed after $ in pattern interpolators, i.e. is `case q"$_ + $_" =>` legal or not?
  allowSpliceUnderscores: Boolean,

  // Are XML literals supported by this dialect?
  // We plan to deprecate XML literal syntax, and some dialects
  // might go ahead and drop support completely.
  allowXmlLiterals: Boolean,

  // Are inline vals and defs supported by this dialect?
  allowInlines: Boolean,

  // Are terms on the top level supported by this dialect?
  // Necessary to support popular script-like DSLs.
  allowToplevelTerms: Boolean,

  // Are `|` (union types) supported by this dialect?
  allowOrTypes: Boolean,

  // What kind of separator is necessary to split top-level statements?
  // Normally none is required, but scripts may have their own rules.
  toplevelSeparator: String,

  // Are view bounds supported by this dialect?
  // Removed in Dotty.
  allowViewBounds: Boolean,

  // Are `&` intersection types supported by this dialect?
  allowAndTypes: Boolean,

  // Are trait allowed to have parameters?
  // They are in Dotty, but not in Scala 2.12 or older.
  allowTraitParameters: Boolean,

  // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
  allowLiteralTypes: Boolean,

  // Are trailing commas allowed? SIP-27.
  allowTrailingCommas: Boolean
) extends Serializable {
  // Are unquotes ($x) and splices (..$xs, ...$xss) allowed?
  def allowUnquotes: Boolean = allowTermUnquotes || allowPatUnquotes

  // Are `with` intersection types supported by this dialect?
  def allowWithTypes: Boolean = !allowAndTypes

  // Dialects have reference equality semantics,
  // because sometimes dialects representing distinct Scala versions
  // can be structurally equal to each other.
  override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  override def equals(other: Any): Boolean = this eq other.asInstanceOf[AnyRef]
  override def hashCode: Int = System.identityHashCode(this)

  // Smart prettyprinting that knows about standard dialects.
  override def toString = {
    Dialect.standardByDialect.get(this) match {
      case Some(name) => name
      case None => s"Dialect(${this.productIterator.map(_.toString).mkString(", ")})"
    }
  }
}

package object dialects {
  implicit val Scala210 = Dialect(
    allowMultilinePrograms = true,
    allowTermUnquotes = false,
    allowPatUnquotes = false,
    bindToSeqWildcardDesignator = "@", // List(1, 2, 3) match { case List(xs @ _*) => ... }
    allowSpliceUnderscores = false, // SI-7715, only fixed in 2.11.0-M5
    allowXmlLiterals = true, // Not even deprecated yet, so we need to support xml literals
    allowInlines = false,
    allowToplevelTerms = false,
    allowOrTypes = false,
    toplevelSeparator = "",
    allowViewBounds = true,
    allowAndTypes = false,
    allowTraitParameters = false,
    allowLiteralTypes = false,
    allowTrailingCommas = false
  )

  implicit val Sbt0136 = Scala210.copy(
    allowToplevelTerms = true,
    toplevelSeparator = EOL
  )

  implicit val Sbt0137 = Scala210.copy(
    allowToplevelTerms = true,
    toplevelSeparator = ""
  )

  implicit val Scala211 = Scala210.copy(
    allowSpliceUnderscores = true // SI-7715, only fixed in 2.11.0-M5
  )

  implicit val Paradise211 = Scala211.copy(
    allowInlines = true
  )

  implicit val Scala212 = Scala211.copy(
    allowLiteralTypes = false, // Scheduled to be included in 2.12.2
    allowTrailingCommas = false // Scheduled to be included in 2.12.2
  )

  implicit val Paradise212 = Scala212.copy(
    allowInlines = true
  )

  implicit val Dotty = Scala212.copy(
    bindToSeqWildcardDesignator = ":", // List(1, 2, 3) match { case List(xs: _*) => ... }
    allowXmlLiterals = false, // Dotty parser doesn't have the corresponding code, so it can't really support xml literals
    allowInlines = true, // New feature in Dotty
    allowViewBounds = false, // View bounds have been removed in Dotty
    allowOrTypes = true, // New feature in Dotty
    allowAndTypes = true, // New feature in Dotty
    allowTraitParameters = true, // New feature in Dotty
    allowLiteralTypes = true, // New feature in Dotty
    allowTrailingCommas = false // Not yet implemented in Dotty
  )

  // TODO: https://github.com/scalameta/scalameta/issues/380
  private[meta] def QuasiquoteTerm(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(allowTermUnquotes = true, allowMultilinePrograms = multiline)
  }

  // TODO: https://github.com/scalameta/scalameta/issues/380
  private[meta] def QuasiquotePat(underlying: Dialect, multiline: Boolean) = {
    require(!underlying.allowUnquotes)
    underlying.copy(allowPatUnquotes = true, allowMultilinePrograms = multiline)
  }
}

object Dialect extends InternalDialect {
  private[meta] lazy val standardByName: Map[String, Dialect] = {
    val dialects = scala.meta.dialects.`package`
    def isDialectGetter(m: java.lang.reflect.Method) = m.getParameterTypes.isEmpty && m.getReturnType == classOf[Dialect]
    val dialectGetters = dialects.getClass.getDeclaredMethods.filter(isDialectGetter)
    dialectGetters.map(m => (m.getName, m.invoke(dialects).asInstanceOf[Dialect])).toMap
  }

  private[meta] lazy val standardByDialect: Map[Dialect, String] = {
    standardByName.map(_.swap)
  }
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait DialectLiftables {
  val c: scala.reflect.macros.blackbox.Context

  import c.universe._
  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  implicit lazy val liftDialect: Liftable[Dialect] = Liftable { dialect =>
    Dialect.standardByDialect.get(dialect) match {
      case Some(name) =>
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
