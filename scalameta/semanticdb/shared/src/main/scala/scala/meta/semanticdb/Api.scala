package lang.meta.internal.semanticdb {
  trait Api extends lang.meta.semanticdb.Api
  trait Aliases extends lang.meta.semanticdb.Aliases
}

package scala.meta.semanticdb {
  private[meta] trait Api extends lang.meta.internal.semanticdb.Api
  private[meta] trait Aliases extends lang.meta.internal.semanticdb.Aliases
}
