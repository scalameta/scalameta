package scala.meta.contrib

import scala.language.higherKinds
import scala.meta.Tree

trait Equality {
  implicit class XtensionTreeEquality[A <: Tree](a: A) {

    @inline
    def equal[F[_ <: Tree]](b: Tree)(implicit conv: Tree => F[Tree], eqEv: Equal[F[Tree]]): Boolean =
      eqEv.equal(a, b)
  }
}

object Equality extends Equality
