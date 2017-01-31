package scala.meta.contrib

import scala.language.higherKinds
import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.meta._

trait ContribApi {
  implicit class XtensionTreeOps[A <: Tree](val tree: A) {
    def ancestors: Seq[Tree] = TreeOps.ancestors(tree)
    def equal[F[_ <: Tree]](tree2: Tree)(implicit conv: Tree => F[Tree],
                                         eqEv: Equal[F[Tree]]): Boolean =
      eqEv.equal(tree, tree2)
    def foreach(f: Tree => Unit): Unit = tree.traverse { case t => f(t) }

    def collectFirst[B](pf: PartialFunction[Tree, B]): Option[B] = TreeOps.collectFirst(tree)(pf)

    def find(f: Tree => Boolean): Option[Tree] = collectFirst { case x if f(x) => x }

    def contains[F[_ <: Tree]](toFind: Tree)(implicit conv: Tree => F[Tree],
                                             eqEv: Equal[F[Tree]]): Boolean =
      find(_.equal[F](toFind)).nonEmpty
  }
}

object TreeOps {
  def collectFirst[B](tree: Tree)(pf: PartialFunction[Tree, B]): Option[B] = {
    var result = Option.empty[B]
    object traverser extends Traverser {
      override def apply(t: Tree): Unit = {
        if (result.isEmpty && pf.isDefinedAt(t)) {
          result = Some(pf(t))
        } else if (result.isEmpty) {
          super.apply(t)
        }
      }
    }
    traverser(tree)
    result
  }
  @tailrec
  final def ancestors(tree: Tree, accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => ancestors(parent, parent +: accum)
      case _            => accum
    }
  }
}
