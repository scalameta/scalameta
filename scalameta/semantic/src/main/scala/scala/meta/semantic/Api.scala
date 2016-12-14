package scala.meta
package semantic

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  // there's no companion for Mirror, so we don't have a term alias here

  type Completed[+T] = scala.meta.semantic.Completed[T]
  lazy val Completed = scala.meta.semantic.Completed

  type SemanticException = scala.meta.semantic.SemanticException
  lazy val SemanticException = scala.meta.semantic.SemanticException
}
