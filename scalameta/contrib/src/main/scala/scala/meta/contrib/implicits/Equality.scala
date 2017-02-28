package scala.meta.contrib.implicits

import scala.language.higherKinds
import scala.meta.Tree
import scala.meta.contrib.equality.Equal

trait Equality {

  type Structurally[A <: Tree] = scala.meta.contrib.equality.Structurally[A]
  type Syntactically[A <: Tree] = scala.meta.contrib.equality.Syntactically[A]

  implicit class XtensionTreeEquality[A <: Tree](a: A) {

    @inline
    def equal[F[_ <: Tree]](b: Tree)(implicit conv: Tree => F[Tree], eqEv: Equal[F[Tree]]): Boolean =
      eqEv.equal(a, b)
  }
}

object Equality extends Equality
