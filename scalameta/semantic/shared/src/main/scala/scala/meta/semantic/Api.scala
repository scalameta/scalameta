package star.meta.internal.semanticdb {
  trait Api extends star.meta.semanticdb.Api
  trait Aliases extends star.meta.semanticdb.Aliases
}

package scala.meta.semantic {
  private[meta] trait Api extends star.meta.internal.semanticdb.Api
  private[meta] trait Aliases extends star.meta.internal.semanticdb.Aliases
}
