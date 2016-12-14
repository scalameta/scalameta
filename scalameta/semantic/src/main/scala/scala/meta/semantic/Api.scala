package scala.meta
package semantic

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  // there's no companion for Mirror, so we don't have a term alias here
}
