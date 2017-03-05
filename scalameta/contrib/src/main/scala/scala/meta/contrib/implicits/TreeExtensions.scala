package scala.meta.contrib.implicits

import scala.language.higherKinds
import scala.meta.Tree
import scala.meta.contrib.{Equal, TreeOps}
import scala.meta.prettyprinters.Show

trait TreeExtensions {
  implicit class XtensionTreeOps[A <: Tree](a: A) {
    @inline
    def ancestors: Seq[Tree] =
      TreeOps.ancestors(a)

    @inline
    def descendants: Seq[Tree] =
      TreeOps.descendants(a)

    @inline
    def foreach(f: Tree => Unit): Unit =
      TreeOps.foreach(a)(f)

    @inline
    def collectFirst[B](pf: PartialFunction[Tree, B]): Option[B] =
      TreeOps.collectFirst(a)(pf)

    @inline
    def find(f: Tree => Boolean): Option[Tree] =
      TreeOps.find(a)(f)

    @inline
    def forall(f: Tree => Boolean): Boolean =
      TreeOps.forall(a)(f)

    @inline
    def exists(f: Tree => Boolean): Boolean =
      TreeOps.exists(a)(f)

    @inline
    def contains[F[x] <: Show[x]](toFind: A)(ev: Equal[F]): Boolean =
      TreeOps.contains(a)(toFind)
  }
}

object TreeExtensions extends TreeExtensions
