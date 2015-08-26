package scala.meta
package ui

import org.scalameta.show._
import scala.annotation.implicitNotFound
import scala.meta.internal.ui.Attributes
import scala.meta.semantic.{Context => SemanticContext}

@implicitNotFound(msg = "don't know how to show[Semantics] for ${T} (be sure to have an implicit scala.meta.semantic.Context in scope)")
trait Semantics[T] extends Show[T]
object Semantics {
  def apply[T](f: T => Show.Result): Semantics[T] = new Semantics[T] { def apply(input: T) = f(input) }

  type Style = Attributes.Style
  val Style = Attributes.Style

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def semanticsTree[T <: scala.meta.Tree](implicit c: SemanticContext, style: Style): Semantics[T] = new Semantics[T] {
    def apply(untypedTree: T): Show.Result = {
      val tree = c.typecheck(untypedTree).asInstanceOf[T]
      val prettyprinter = Attributes.attributesTree[T](style)
      prettyprinter(tree)
    }
  }
}
