package scala.meta
package semantic
package v1

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the long-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

private[meta] trait Api {
  implicit class XtensionSemanticEquality(tree1: Tree)(implicit m: Mirror) {
    def ===(tree2: Tree): Boolean = scala.meta.internal.semantic.v1.Equality.equals(tree1, tree2)
    def =!=(tree2: Tree): Boolean = !(tree1 === tree2)
  }

  implicit class XtensionRefSymbol(ref: Ref)(implicit m: Mirror) {
    def symbol: Symbol = m.symbol(ref).get
  }
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.v1.Mirror
  // there's no companion for Mirror, so we don't have a term alias here

  type Symbol = scala.meta.semantic.v1.Symbol
  val Symbol = scala.meta.semantic.v1.Symbol

  type SemanticException = scala.meta.semantic.v1.SemanticException
  lazy val SemanticException = scala.meta.semantic.v1.SemanticException
}
