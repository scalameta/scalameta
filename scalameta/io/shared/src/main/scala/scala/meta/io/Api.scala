package lang.meta.internal.io {
  trait Api extends lang.meta.io.Api
  trait Aliases extends lang.meta.io.Aliases
}

package scala.meta.io {
  private[meta] trait Api extends lang.meta.internal.io.Api
  private[meta] trait Aliases extends lang.meta.internal.io.Aliases
}
