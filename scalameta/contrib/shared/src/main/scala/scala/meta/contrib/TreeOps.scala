package scala.meta
package contrib

import scala.meta.contrib.equality.{Equal, TreeEquality}

import scala.annotation.tailrec
import scala.language.higherKinds

object TreeOps {

  def contains[F[x <: Tree] <: TreeEquality[x]](
      tree: Tree,
  )(toFind: Tree)(implicit conv: Tree => F[Tree], eqEv: Equal[F[Tree]]): Boolean =
    exists(tree)(_.isEqual[F](toFind))

  def find(tree: Tree)(f: Tree => Boolean): Option[Tree] = tree.dfsFind(f)

  def forall(tree: Tree)(f: Tree => Boolean): Boolean = tree.dfsForall(f)

  def exists(tree: Tree)(f: Tree => Boolean): Boolean = tree.dfsExists(f)

  def collectFirst[B](tree: Tree)(pf: PartialFunction[Tree, B]): Option[B] = tree.dfsCollectFirst(pf)

  def foreach(tree: Tree)(f: Tree => Unit): Unit = tree.dfs(f)

  def descendants(tree: Tree): List[Tree] = tree.dfsCollect(t => if (t ne tree) Some(t) else None)

  def collect[B](tree: Tree)(pf: PartialFunction[Tree, B]): List[B] = tree.dfsCollect(pf)

  @tailrec
  final def ancestors(tree: Tree, accum: List[Tree] = Nil): List[Tree] = tree.parent match {
    case Some(parent) => ancestors(parent, parent +: accum)
    case _ => accum
  }
}
