package scala.meta.contrib.equality

import scala.language.implicitConversions
import scala.meta.Tree

/** Represents structural equality between trees
  *
  * Two trees are structurally equal if their .structure is equal.
  * This implementation is however more efficient that doing
  * a.structure == b.structure.
  */
class Structurally[+A <: Tree](val tree: A) extends TreeEquality[A] {
  // TODO(olafur) more efficient hashCode
  private lazy val hash: Int = tree.structure.hashCode
  override def hashCode(): Int = hash
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case e2: Structurally[_] => Structurally.equal(tree, e2.tree)
      case _ => false
    }
  }
}

object Structurally {

  def apply[A <: Tree](tree: A): Structurally[A] =
    new Structurally[A](tree)

  def equal(a: Tree, b: Tree): Boolean = loopStructure(a, b)

  private def loopStructure(x: Any, y: Any): Boolean = (x, y) match {
    case (x, y) if x == null || y == null => x == null && y == null
    case (Some(x), Some(y)) => loopStructure(x, y)
    case (None, None) => true
    case (xs: List[_], ys: List[_]) =>
      xs.length == ys.length &&
        xs.zip(ys).forall { case (x, y) => loopStructure(x, y) }
    case (x: Tree, y: Tree) =>
      def sameStructure =
        x.productPrefix == y.productPrefix &&
          loopStructure(x.productIterator.toList, y.productIterator.toList)

      sameStructure
    case _ => x == y
  }

  implicit def StructuralEq[A <: Tree]: Equal[Structurally[A]] =
    new Equal[Structurally[A]] {
      override def isEqual(a: Structurally[A], b: Structurally[A]): Boolean = a.equals(b)
    }

  implicit def toStructural[A <: Tree](e: A): Structurally[A] = apply(e)
}
