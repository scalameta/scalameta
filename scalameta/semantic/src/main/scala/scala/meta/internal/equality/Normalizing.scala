package scala.meta
package internal
package equality

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.semantic.{Context => SemanticContext}
import scala.meta.semantic.api._

// TODO: I really don't like what I'm doing here.
// See Semantic.scala for an explanation and future work.

object Normalizing {
  private def normalize(tree: Tree)(implicit c: SemanticContext): Tree = {
    val ttree = c.typecheck(tree)
    ttree match {
      case tterm: Term => tterm.desugar
      case ttpe: Type => ttpe.dealias
      case ttree => tree
    }
  }

  def equals(x1: Any, x2: Any)(implicit c: SemanticContext): Boolean = customEquals(x1, x2)

  private def customEquals(x: Any, y: Any)(implicit c: SemanticContext): Boolean = (x, y) match {
    case (x: Some[_], y: Some[_]) =>
      customEquals(x.get, y.get)
    case (xs: Seq[_], ys: Seq[_]) =>
      xs.length == ys.length && xs.zip(ys).forall{ case (x, y) => customEquals(x, y) }
    case (x: Tree, y: Tree) =>
      val x1 = normalize(x)
      val y1 = normalize(y)
      Semantic.equals(x1, y1)
    case _ =>
      Semantic.equals(x, y)
  }

  def hashCode(x: Any)(implicit c: SemanticContext): Int = customHashcode(x)

  private def customHashcode(x: Any)(implicit c: SemanticContext): Int = x match {
    case x: Option[_] =>
      x.map(customHashcode).getOrElse(0)
    case xs: Seq[_] =>
      xs.foldLeft(0)((acc, curr) => acc * 37 + customHashcode(curr))
    case (x: Tree, y: Tree) =>
      val x1 = normalize(x)
      Semantic.hashCode(x1)
    case _ =>
      Semantic.hashCode(x)
  }
}
