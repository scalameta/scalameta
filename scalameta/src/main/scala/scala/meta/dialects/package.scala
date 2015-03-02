package scala.meta

import scala.language.experimental.{macros => prettyPlease}
import scala.meta.internal.dialects.Macros
import scala.annotation.implicitNotFound

// NOTE: can't put Dialect into scala.meta.Dialects
// because then implicit scope for Dialect lookups will contain members of the package object
// i.e. both Scala211 and Dotty, which is definitely not what we want
@implicitNotFound("don't know what dialect to use here (to fix this, import something from scala.dialects, e.g. scala.meta.dialects.Scala211)")
trait Dialect {
  // The sequence of characters that's used to express a bind
  // to a sequence wildcard pattern.
  def bindToSeqWildcardDesignator: String

  // Permission to tokenize repeated dots as ellipses.
  // Necessary to support quasiquotes, e.g. `q"foo(..$args)"`.
  def allowEllipses: Boolean
}

object Dialect {
  // NOTE: this is much better than having contexts extend Dialect, because
  // a) context is not a dialect, so that'd be an abuse of subtyping
  // b) even if we squint, bite the bullet and extend, that'd create a bunch of random methods in Context, and that's ugly
  // NOTE: this has to be a whitebox macro, because otherwise we're going to lose precise type of the dialect
  // and that's important for those who depend on it at compile time (e.g. for quasiquotes)
  // NOTE: also, the macro has to be defined in foundation, because semantic/package.scala depends on it
  // how?? well, by using show[Summary] inside its @hosted methods!!! that was very unexpected, but profoundly correct :)
  implicit def dialectFromSemanticContext(implicit c: scala.meta.semantic.Context): Dialect = macro Macros.dialectFromSemanticContext
}

package object dialects {
  implicit object Scala211 extends Dialect {
    override def toString = "Scala211"
    def bindToSeqWildcardDesignator = "@" // List(1, 2, 3) match { case List(xs @ _*) => ... }
    def allowEllipses: Boolean = false // Vanilla Scala doesn't support ellipses, somewhat similar concept is varargs and _*
  }

  implicit object Dotty extends Dialect {
    override def toString = "Dotty"
    def bindToSeqWildcardDesignator = ":" // // List(1, 2, 3) match { case List(xs: _*) => ... }
    def allowEllipses: Boolean = false // Vanilla Dotty doesn't support ellipses, somewhat similar concept is varargs and _*
  }

  def Quasiquote(dialect: Dialect): Dialect = new Dialect {
    override def toString = s"Quasiquotes(${dialect.toString})"
    def bindToSeqWildcardDesignator = dialect.bindToSeqWildcardDesignator
    def allowEllipses: Boolean = true
  }
}
