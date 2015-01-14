package scala.meta

// NOTE: can't put Dialect into scala.meta.Dialects
// because then implicit scope for Dialect lookups will contain members of the package object
// i.e. both Scala211 and Dotty, which is definitely not what we want
trait Dialect {
}

object Dialect {
  // NOTE: this is much better than having contexts extend Dialect, because
  // a) context is not a dialect, so that'd be an abuse of subtyping
  // b) even if we squint, bite the bullet and extend, that'd create a bunch of random methods in Context, and that's ugly
  implicit def dialectFromSemanticContext(implicit c: scala.meta.semantic.Context): Dialect = c.dialect
}

package object dialects {
  implicit object Scala211 extends Dialect {
  }

  implicit object Dotty extends Dialect {
  }
}
