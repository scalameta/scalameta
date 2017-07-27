package star.meta.internal.io {
  trait Api extends star.meta.io.Api
  trait Aliases extends star.meta.io.Aliases
}

package scala.meta.io {
  private[meta] trait Api extends star.meta.internal.io.Api
  private[meta] trait Aliases extends star.meta.internal.io.Aliases
}
