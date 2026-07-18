package scala.meta.contrib.equality

import scala.meta.{Tree, XtensionStructure}

import scala.language.implicitConversions

/**
 * Represents structural equality between trees
 *
 * Two trees are structurally equal if their .structure is equal. This implementation is however
 * more efficient that doing a.structure == b.structure.
 */
class Structurally[+A <: Tree](val tree: A) extends TreeEquality[A] {
  // TODO(olafur) more efficient hashCode
  private lazy val hash: Int = tree.structure.hashCode
  override def hashCode(): Int = hash
  override def equals(obj: scala.Any): Boolean = obj match {
    case e2: Structurally[_] => Structurally.equal(tree, e2.tree)
    case _ => false
  }
}

object Structurally {

  def apply[A <: Tree](tree: A): Structurally[A] = new Structurally[A](tree)

  def equal(a: Tree, b: Tree): Boolean = loopStructure(a, b)

  // Iterative (deeply nested trees would otherwise overflow the stack).
  // Children are pushed
  // onto an explicit worklist instead of recursing; comparison short-circuits
  // on the first mismatch.
  private def loopStructure(x0: Any, y0: Any): Boolean = {
    // Push directly onto `stack` (rather than via a helper def) so it stays a
    // local var; capturing it in a nested def would lift it to a heap ObjectRef.
    var stack: List[(Any, Any)] = (x0, y0) :: Nil
    while (stack.nonEmpty) {
      val (x, y) = stack.head
      stack = stack.tail
      (x, y) match {
        case (x, y) if x == null || y == null => if (x != null || y != null) return false
        case (x: Tree, y: Tree) =>
          if (x.productPrefix != y.productPrefix) return false
          val xl = x.productArity
          if (xl != y.productArity) return false
          var i = 0
          while (i < xl) {
            stack = (x.productElement(i), y.productElement(i)) :: stack
            i += 1
          }
        case (Some(x), Some(y)) => stack = (x, y) :: stack
        case (None, None) =>
        case (xs: Iterable[_], ys: Iterable[_]) =>
          val xi = xs.iterator
          val yi = ys.iterator
          while (xi.hasNext)
            if (yi.hasNext) stack = (xi.next(), yi.next()) :: stack else return false
          if (yi.hasNext) return false
        case _ => if (x != y) return false
      }
    }
    true
  }

  implicit def StructuralEq[A <: Tree]: Equal[Structurally[A]] = new Equal[Structurally[A]] {
    override def isEqual(a: Structurally[A], b: Structurally[A]): Boolean = a.equals(b)
  }

  implicit def toStructural[A <: Tree](e: A): Structurally[A] = apply(e)
}
