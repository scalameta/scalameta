package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib.Equal
import scala.meta.prettyprinters.{Show, Structure}
import scala.language.higherKinds

trait EqualityInstances {
  implicit def showEquality[F[x] <: Show[x]]: Equal[F] =
    new Equal[F] {
      override def isEqual[A](a: A, b: A): Boolean =
        a.show[F].equals(b.show[F])
    }

  implicit def structuralEquality: Equal[Structure] =
    new Equal[Structure] {
      override def isEqual[A <: Tree](a: A, b: A): Boolean =
        Structurally.equal(a, b)
    }
}

object EqualityInstances extends EqualityInstances


/** Represents structural equality between trees
  *
  * Two trees are structurally equal if their .show[Structure] is equal.
  * This implementation is however more efficient that doing
  * a.structure == b.structure.
  */
object Structurally {

  def equal(a: Tree, b: Tree): Boolean = loopStructure(a, b)

  private def loopStructure(x: Any, y: Any): Boolean = (x, y) match {
    case (x, y) if x == null || y == null => x == null && y == null
    case (Some(x), Some(y)) => loopStructure(x, y)
    case (None, None) => true
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