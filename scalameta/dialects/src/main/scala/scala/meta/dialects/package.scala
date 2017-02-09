package scala.meta

import scala.annotation.implicitNotFound
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import scala.meta.dialects._
import scala.meta.internal.dialects._
import scala.language.experimental.macros
import scala.compat.Platform.EOL

// NOTE: can't put Dialect into scala.meta.Dialects
// because then implicit scope for Dialect lookups will contain members of the package object
// i.e. both Scala211 and Dotty, which is definitely not what we want
@root trait Dialect extends Serializable {
  // Canonical name for the dialect.
  // Can be used to uniquely identify the dialect, e.g. during serialization/deserialization.
  def name: String

  // What level of quoting are we at?
  // The underlying data structure captures additional information necessary for parsing.
  def metalevel: Metalevel

  // Permission to parse unquotes.
  // Necessary to support quasiquotes, e.g. `q"$x + $y"`.
  def allowUnquotes: Boolean = metalevel.isQuoted

  // Permission to tokenize repeated dots as ellipses and to parse ellipses.
  // Necessary to support quasiquotes, e.g. `q"foo(..$args)"`.
  def allowEllipses: Boolean = metalevel.isQuoted

  // The sequence of characters that's used to express a bind
  // to a sequence wildcard pattern.
  def bindToSeqWildcardDesignator: String

  // Are naked underscores allowed after $ in pattern interpolators, i.e. is `case q"$_ + $_" =>` legal or not?
  def allowSpliceUnderscore: Boolean

  // Are XML literals supported by this dialect?
  // We plan to deprecate XML literal syntax, and some dialects
  // might go ahead and drop support completely.
  def allowXmlLiterals: Boolean

  // Are inline vals and defs supported by this dialect?
  def allowInline: Boolean

  // Are terms on the top level supported by this dialect?
  // Necessary to support popular script-like DSLs.
  def allowToplevelTerms: Boolean

  // Are `|` (union types) supported by this dialect?
  def allowOrTypes: Boolean

  // What kind of separator is necessary to split top-level statements?
  // Normally none is required, but scripts may have their own rules.
  def toplevelSeparator: String

  // Are view bounds supported by this dialect?
  // Removed in Dotty.
  def allowViewBounds: Boolean

  // Are `&` intersection types supported by this dialect?
  def allowAndTypes: Boolean

  // Are `with` intersection types supported by this dialect?
  def allowWithTypes: Boolean = !allowAndTypes

  // Are trait allowed to have parameters?
  // They are in Dotty, but not in Scala 2.12 or older.
  def allowTraitParameters: Boolean

  // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
  def allowLiteralTypes: Boolean
}

package object dialects {
  @branch private[meta] trait DelegatingDialect extends Dialect {
    def delegate: Dialect
    def metalevel                   = delegate.metalevel
    def bindToSeqWildcardDesignator = delegate.bindToSeqWildcardDesignator
    def allowXmlLiterals            = delegate.allowXmlLiterals
    def allowInline                 = delegate.allowInline
    def allowSpliceUnderscore       = delegate.allowSpliceUnderscore
    def allowToplevelTerms          = delegate.allowToplevelTerms
    def allowOrTypes                = delegate.allowOrTypes
    def toplevelSeparator           = delegate.toplevelSeparator
    def allowViewBounds             = delegate.allowViewBounds
    def allowAndTypes               = delegate.allowAndTypes
    def allowTraitParameters        = delegate.allowTraitParameters
    def allowLiteralTypes           = delegate.allowLiteralTypes

    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf implicit object Scala210 extends Dialect {
    def name = "Scala210"
    def metalevel = Metalevel.Zero
    def bindToSeqWildcardDesignator = "@" // List(1, 2, 3) match { case List(xs @ _*) => ... }
    def allowXmlLiterals = true // Not even deprecated yet, so we need to support xml literals
    def allowInline = false
    def allowSpliceUnderscore = false // SI-7715, only fixed in 2.11.0-M5
    def allowToplevelTerms = false
    def allowOrTypes = false
    def toplevelSeparator = ""
    def allowViewBounds = true
    def allowAndTypes = false
    def allowTraitParameters = false
    def allowLiteralTypes: Boolean = false
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf implicit object Sbt0136 extends DelegatingDialect {
    def name = "Sbt0136"
    def delegate = Scala210
    override def allowToplevelTerms = true
    override def toplevelSeparator = EOL
  }

  @leaf implicit object Sbt0137 extends DelegatingDialect {
    def name = "Sbt0137"
    def delegate = Scala210
    override def allowToplevelTerms = true
  }

  @leaf implicit object Scala211 extends DelegatingDialect {
    def name = "Scala211"
    def delegate = Scala210
    override def allowSpliceUnderscore = true // SI-7715, only fixed in 2.11.0-M5
  }

  @leaf implicit object Paradise211 extends DelegatingDialect {
    def name = "Paradise211"
    def delegate = Scala211
    override def allowInline = true
  }

  @leaf implicit object Scala212 extends DelegatingDialect {
    def name = "Scala212"
    def delegate = Scala211
    override def allowLiteralTypes = false // Scheduled to be included in 2.12.2
  }

  @leaf implicit object Paradise212 extends DelegatingDialect {
    def name = "Paradise212"
    def delegate = Scala212
    override def allowInline = true
  }

  @leaf implicit object Dotty extends Dialect {
    def name = "Dotty"
    def metalevel = Metalevel.Zero
    def bindToSeqWildcardDesignator = ":" // // List(1, 2, 3) match { case List(xs: _*) => ... }
    def allowXmlLiterals = false // Dotty parser doesn't have the corresponding code, so it can't really support xml literals
    def allowInline = true
    def allowSpliceUnderscore = true
    def allowToplevelTerms = false
    def toplevelSeparator = ""
    def allowViewBounds = false // View bounds have been removed in Dotty
    def allowAndTypes = true
    def allowOrTypes = true
    def allowTraitParameters = true
    def allowLiteralTypes: Boolean = true
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @branch private[meta] trait Quasiquote extends Dialect {
    def name = s"$qualifier(${underlying.name}, ${if (multiline) "Multi" else "Single"})"
    def multiline: Boolean
    def qualifier: String
    def underlying: Dialect
    def metalevel = Metalevel.Quoted
    def bindToSeqWildcardDesignator = underlying.bindToSeqWildcardDesignator
    def allowXmlLiterals = underlying.allowXmlLiterals
    def allowInline = underlying.allowInline
    def allowSpliceUnderscore = underlying.allowSpliceUnderscore
    def allowToplevelTerms = underlying.allowToplevelTerms
    def toplevelSeparator = underlying.toplevelSeparator
    def allowViewBounds = underlying.allowViewBounds
    def allowAndTypes = underlying.allowAndTypes
    def allowOrTypes = underlying.allowOrTypes
    def allowTraitParameters = underlying.allowTraitParameters
    def allowLiteralTypes = underlying.allowLiteralTypes
  }

  @leaf private[meta] class QuasiquoteTerm(underlying: Dialect, multiline: Boolean) extends Quasiquote {
    require(!underlying.isInstanceOf[Quasiquote])
    def qualifier = s"QuasiquoteTerm"
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
    override def toString = name
  }

  @leaf private[meta] class QuasiquotePat(underlying: Dialect, multiline: Boolean) extends Quasiquote {
    require(!underlying.isInstanceOf[Quasiquote])
    def qualifier = s"QuasiquotePat"
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
    override def toString = name
  }

  // NOTE: The syntax for nested interpolation is ugly to the point of being impractical.
  // That's why we won't implement support for it for the time being.
  // However, it'd have been reasonably simple:
  //  * For 0..nesting-1, 2^n dollars in a row drop us to a level-n unquote.
  //  * 2^nesting dollars in a row represent a dollar character itself.
  @root trait Metalevel {
    def isQuoted: Boolean = nesting > 0
    def nesting: Int
  }
  object Metalevel {
    @leaf object Zero extends Metalevel { def nesting: Int = 0 }
    @leaf object Quoted extends Metalevel { def nesting: Int = 1 }
  }
}

object Dialect {
  lazy val all: Seq[Dialect] = scala.meta.internal.dialects.all

  // NOTE: See https://github.com/scalameta/scalameta/issues/253 for discussion.
  implicit def current: Dialect = macro Macros.current

  def forName(name: String): Dialect = {
    def maybeVanilla = all.find(_.name == name)
    def maybeQuasiquote = """^Quasiquote(.+?)\((.*?)\)$""".r.unapplySeq(name).map {
      case List(qualifier, props) =>
        def failQualifier() = {
          val message = s"unexpected quasiquote qualifier: expected = Term|Pat, actual = $qualifier"
          throw new IllegalArgumentException(message)
        }
        def failProps() = {
          val expected = """QuasiquoteTerm|QuasiquotePat(.*?, Single|Multi)"""
          val message = s"unexpected quasiquote properties: expected = $expected, actual = Quasiquote$qualifier($props)"
          throw new IllegalArgumentException(message)
        }
        val (underlying, multiline) = """^\s*(.*?)\s*,\s*(Single|Multi)\s*$""".r.unapplySeq(props) match {
          case Some(List(s_underlying, s_multiline)) => (Dialect.forName(s_underlying), s_multiline == "Multi")
          case _ => failProps()
        }
        qualifier match {
          case "Term" => QuasiquoteTerm(underlying, multiline)
          case "Pat" => QuasiquotePat(underlying, multiline)
          case other => failQualifier()
        }
    }
    def fail() = throw new IllegalArgumentException(s"unknown dialect $name")
    maybeVanilla.orElse(maybeQuasiquote).getOrElse(fail())
  }

  @SerialVersionUID(1L) private[meta] class SerializationProxy(@transient private var orig: Dialect) extends Serializable {
    private def writeObject(out: java.io.ObjectOutputStream): Unit = {
      out.writeObject(orig.name)
    }
    private def readObject(in: java.io.ObjectInputStream): Unit = {
      val name = in.readObject.asInstanceOf[String]
      orig = Dialect.forName(name)
    }
    private def readResolve(): AnyRef = orig
    override def toString = s"Proxy($orig)"
  }
}
