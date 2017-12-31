package scala.meta.contrib.implicits

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.meta.Tree
import scala.meta.contrib.equality.Equal
import scala.meta.contrib.equality.TreeEquality
import scala.meta.contrib.equality.Structurally

trait Equality {

  /** Helper to default tree equality to use structural equality. */
  implicit def treeToContainer(tree: Tree): Structurally[Tree] = Structurally.toStructural(tree)

  type Structurally[A <: Tree] = scala.meta.contrib.equality.Structurally[A]
  type Syntactically[A <: Tree] = scala.meta.contrib.equality.Syntactically[A]

  implicit class XtensionTreeEquality[A <: Tree](a: A) {

    @inline
    def isEqual[F[x <: Tree] <: TreeEquality[x]](
        b: A)(implicit conv: Tree => F[Tree], eqEv: Equal[F[Tree]]): Boolean =
      (a eq b) || eqEv.isEqual(a, b)
  }
}

object Equality extends Equality
