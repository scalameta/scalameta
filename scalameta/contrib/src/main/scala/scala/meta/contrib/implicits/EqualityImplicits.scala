package scala.meta.contrib.implicits

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.meta.Tree
import scala.meta.contrib.Equal
import scala.meta.prettyprinters.Show

trait EqualityImplicits {

  implicit class XtensionTreeEquality[A <: Tree](a: A) {

    @inline
    def isEqual[F[x] <: Show[x]](b: A)(ev: Equal[F]): Boolean =
      ev.isEqual(a, b)
  }
}

object EqualityImplicits extends EqualityImplicits
