package scala.meta.contrib

import scala.language.higherKinds
import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.meta._

trait ContribApi {
  implicit class XtensionTreeOps[A <: Tree](val tree: A) {

    def parents: Seq[Tree] = TreeOps.parents(tree)

    /** Returns true if tree is equal to argument according to F. */
    def equal[F[_ <: Tree]](tree2: Tree)(implicit conv: Tree => F[Tree],
                                         eqEv: Equal[F[Tree]]): Boolean =
      eqEv.equal(tree, tree2)
    def foreach(f: Tree => Unit): Unit = tree.traverse { case t => f(t) }
    def collectFirst[B](pf: PartialFunction[Tree, B]): Option[B] = {
      val f      = pf.lift
      var result = Option.empty[B]
      def loop(t: Tree): Unit = {
        f(t) match {
          case None =>
            t.children.withFilter(_ => result.isEmpty).foreach(loop)
          case som =>
            result = som
        }
      }
      loop(tree)
      result
    }
    def find(f: Tree => Boolean): Option[Tree] = collectFirst { case x if f(x) => x }
    def contains[F[_ <: Tree]](toFind: Tree)(implicit conv: Tree => F[Tree],
                                             eqEv: Equal[F[Tree]]): Boolean =
      find(_.equal[F](toFind)).nonEmpty
  }

}

object TreeOps {
  @tailrec
  final def parents(tree: Tree, accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => parents(parent, parent +: accum)
      case _            => accum
    }
  }
}
