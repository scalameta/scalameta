package scala.meta
package contrib

import scala.language.higherKinds

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.meta.contrib.Equality._

object TreeOps {

  def contains[F[_ <: Tree]](tree: Tree)(toFind: Tree)(implicit conv: Tree => F[Tree],
                                                       eqEv: Equal[F[Tree]]): Boolean =
    find(tree)(_.equal[F](toFind)).nonEmpty

  def find(tree: Tree)(f: Tree => Boolean): Option[Tree] =
    collectFirst(tree) { case x if f(x) => x }

  def forall(tree: Tree)(f: Tree => Boolean): Boolean =
    find(tree)(t => !f(t)).isEmpty

  def exists(tree: Tree)(f: Tree => Boolean): Boolean =
    find(tree)(t => f(t)).isDefined

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

  def foreach(tree: Tree)(f: Tree => Unit): Unit =
    tree.traverse { case t => f(t) }

  def descendants(tree: Tree): Seq[Tree] = {
    val builder = Seq.newBuilder[Tree]
    object traverser extends Traverser {
      override def apply(t: Tree): Unit = {
        if (t ne tree) builder += t
        super.apply(t)
      }
    }
    traverser(tree)
    builder.result()
  }

  @tailrec
  final def ancestors(tree: Tree, accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] =
    tree.parent match {
      case Some(parent) => ancestors(parent, parent +: accum)
      case _ => accum
    }
}
