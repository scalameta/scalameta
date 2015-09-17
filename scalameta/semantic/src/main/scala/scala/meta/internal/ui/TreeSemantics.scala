package scala.meta
package internal
package ui

object TreeSemantics {
  def apply[T <: Tree](implicit c: SemanticContext): Semantics[T] = {
    Semantics { (x: T) =>
      val tree = c.typecheck(untypedTree).asInstanceOf[T]
      val prettyprinter = Attributes.attributesTree[T](Attributes.Recursion.Deep)
      prettyprinter(tree)
    }
  }
}
