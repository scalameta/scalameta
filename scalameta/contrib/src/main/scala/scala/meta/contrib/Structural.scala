package scala.meta.contrib

import scala.language.implicitConversions

import scala.meta.Tree

/** Represents structural equality between trees */
class Structural[+A <: Tree](val tree: A) {
  // TODO(olafur) more efficient impl.
  private lazy val hash: Int   = tree.structure.hashCode
  override def hashCode(): Int = hash
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case e2: Structural[_] => loopStructure(tree, e2.tree)
      case _                 => false
    }
  }
  private def loopStructure(x: Any, y: Any): Boolean = (x, y) match {
    case (x, y) if x == null || y == null => x == null && y == null
    case (Some(x), Some(y))               => loopStructure(x, y)
    case (None, None)                     => true
    case (xs: Seq[_], ys: Seq[_]) =>
      xs.length == ys.length &&
        xs.zip(ys).forall { case (x, y) => loopStructure(x, y) }
    case (x: Tree, y: Tree) =>
      def sameStructure =
        x.productPrefix == y.productPrefix &&
          loopStructure(x.productIterator.toList, y.productIterator.toList)
      sameStructure
    case _ => x == y
  }
}

object Structural {
  implicit def StructuralEq[A <: Tree]: Equal[Structural[A]] =
    new Equal[Structural[A]] {
      override def equal(a: Structural[A], b: Structural[A]): Boolean = a.equals(b)
    }
  implicit def toStructural[A <: Tree](e: A): Structural[A] = new Structural[A](e)
}
