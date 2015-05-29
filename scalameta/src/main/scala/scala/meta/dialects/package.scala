package scala.meta

import scala.language.experimental.{macros => prettyPlease}
import scala.annotation.implicitNotFound
import scala.reflect.macros.blackbox.Context

// NOTE: can't put Dialect into scala.meta.Dialects
// because then implicit scope for Dialect lookups will contain members of the package object
// i.e. both Scala211 and Dotty, which is definitely not what we want
@implicitNotFound("don't know what dialect to use here (to fix this, import something from scala.dialects, e.g. scala.meta.dialects.Scala211)")
trait Dialect {
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
}

package object dialects {
  implicit object Scala211 extends Dialect {
    override def toString = "Scala211"
    def bindToSeqWildcardDesignator = "@" // List(1, 2, 3) match { case List(xs @ _*) => ... }
    def allowXmlLiterals = true // Not even deprecated yet, so we need to support xml literals
    def allowEllipses = false // Vanilla Scala doesn't support ellipses, somewhat similar concept is varargs and _*
  }

  implicit object Dotty extends Dialect {
    override def toString = "Dotty"
    def bindToSeqWildcardDesignator = ":" // // List(1, 2, 3) match { case List(xs: _*) => ... }
    def allowXmlLiterals = false // Dotty parser doesn't have the corresponding code, so it can't really support xml literals
    def allowEllipses = false // Vanilla Dotty doesn't support ellipses, somewhat similar concept is varargs and _*
  }

  def Quasiquote(dialect: Dialect): Dialect = new Dialect {
    override def toString = s"Quasiquotes(${dialect.toString})"
    def bindToSeqWildcardDesignator = dialect.bindToSeqWildcardDesignator
    def allowXmlLiterals = dialect.allowXmlLiterals
    def allowEllipses = true
  }
}

trait DialectLiftables {
  val c: Context

  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  import c.universe._

  implicit lazy val liftDialect: Liftable[Dialect] = Liftable[Dialect] { dialect =>
    dialect match {
      case dialects.Scala211 => q"_root_.scala.meta.dialects.Scala211"
      case dialects.Dotty    => q"_root_.scala.meta.dialects.Dotty"
      case other =>
        q"""new _root_.scala.meta.Dialect {
              override def toString = ${other.toString}
              def bindToSeqWildcardDesignator = ${other.bindToSeqWildcardDesignator}
              def allowXmlLiterals = ${other.allowXmlLiterals}
              def allowEllipses = ${other.allowEllipses}
            }"""
    }
  }
}
