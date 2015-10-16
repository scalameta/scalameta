package scala.meta
package internal
package prettyprinters

import org.scalameta.show._
import scala.meta.prettyprinters.Semantics
import scala.meta.semantic.{Context => SemanticContext}

object TreeSemantics {
  def apply[T <: Tree](implicit c: SemanticContext): Semantics[T] = {
    Semantics { (x: T) =>
      val tree = c.typecheck(x).asInstanceOf[T]
      val prettyprinter = Attributes.attributesTree[T](Attributes.Recursion.Deep, Attributes.Force.Always)
      prettyprinter(tree)
    }
  }
}
