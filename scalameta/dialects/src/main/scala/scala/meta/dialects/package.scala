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

  // The sequence of characters that's used to express a bind
  // to a sequence wildcard pattern.
  def bindToSeqWildcardDesignator: String

  // Are XML literals supported by this dialect?
  // We plan to deprecate XML literal syntax, and some dialects
  // might go ahead and drop support completely.
  def allowXmlLiterals: Boolean

  // Permission to tokenize repeated dots as ellipses.
  // Necessary to support quasiquotes, e.g. `q"foo(..$args)"`.
  def allowEllipses: Boolean

  // Are terms on the top level supported by this dialect?
  // Necessary to support popular script-like DSLs.
  def allowToplevelTerms: Boolean

  // What kind of separator is necessary to split top-level statements?
  // Normally none is required, but scripts may have their own rules.
  def toplevelSeparator: String
}

package object dialects {
  @leaf implicit object Scala210 extends Dialect {
    def name = "Scala210"
    def bindToSeqWildcardDesignator = "@" // List(1, 2, 3) match { case List(xs @ _*) => ... }
    def allowXmlLiterals = true // Not even deprecated yet, so we need to support xml literals
    def allowEllipses = false // Vanilla Scala doesn't support ellipses, somewhat similar concept is varargs and _*
    def allowToplevelTerms = false
    def toplevelSeparator = ""
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf implicit object Sbt0136 extends Dialect {
    def name = "Sbt0136"
    def bindToSeqWildcardDesignator = Scala210.bindToSeqWildcardDesignator
    def allowXmlLiterals = Scala210.allowXmlLiterals
    def allowEllipses = Scala210.allowEllipses
    def allowToplevelTerms = false
    def toplevelSeparator = EOL
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf implicit object Sbt0137 extends Dialect {
    def name = "Sbt0137"
    def bindToSeqWildcardDesignator = Scala210.bindToSeqWildcardDesignator
    def allowXmlLiterals = Scala210.allowXmlLiterals
    def allowEllipses = Scala210.allowEllipses
    def allowToplevelTerms = true
    def toplevelSeparator = ""
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf implicit object Scala211 extends Dialect {
    def name = "Scala211"
    def bindToSeqWildcardDesignator = Scala210.bindToSeqWildcardDesignator
    def allowXmlLiterals = Scala210.allowXmlLiterals
    def allowEllipses = Scala210.allowEllipses
    def allowToplevelTerms = Scala210.allowToplevelTerms
    def toplevelSeparator = Scala210.toplevelSeparator
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf implicit object Dotty extends Dialect {
    def name = "Dotty"
    def bindToSeqWildcardDesignator = ":" // // List(1, 2, 3) match { case List(xs: _*) => ... }
    def allowXmlLiterals = false // Dotty parser doesn't have the corresponding code, so it can't really support xml literals
    def allowEllipses = false // Vanilla Dotty doesn't support ellipses, somewhat similar concept is varargs and _*
    def allowToplevelTerms = false
    def toplevelSeparator = ""
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }

  @leaf private[meta] class Quasiquote(dialect: Dialect) extends Dialect {
    def name = s"Quasiquote(${dialect.name})"
    def bindToSeqWildcardDesignator = dialect.bindToSeqWildcardDesignator
    def allowXmlLiterals = dialect.allowXmlLiterals
    def allowEllipses = true
    def allowToplevelTerms = dialect.allowToplevelTerms
    def toplevelSeparator = dialect.toplevelSeparator
    private def writeReplace(): AnyRef = new Dialect.SerializationProxy(this)
  }
}

object Dialect {
  lazy val all: Seq[Dialect] = scala.meta.internal.dialects.all

  // NOTE: See https://github.com/scalameta/scalameta/issues/253 for discussion.
  implicit def current: Dialect = macro Macros.current

  def forName(name: String): Dialect = {
    def maybeVanilla = all.find(_.name == name)
    def maybeQuasiquote = "^Quasiquote\\((.*?)\\)$".r.unapplySeq(name).map {
      case List(subname) => Quasiquote(Dialect.forName(subname))
    }
    def fail = throw new IllegalArgumentException(s"unknown dialect $name")
    maybeVanilla.orElse(maybeQuasiquote).getOrElse(fail)
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
