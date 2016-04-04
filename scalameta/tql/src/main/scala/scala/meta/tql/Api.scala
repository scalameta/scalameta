package scala.meta
package tql

import scala.language.implicitConversions
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.tql._
import scala.meta.internal.{ast => impl}

// TODO: The current incarnation of the traversal/transformation API is very generic (that is interesting)
// but also contains a lot of moving parts, e.g. see things like EvaluatorMetaCollector (that is daunting).
// We need to study how accessible this is for the users and see where to go from there.
private[meta] trait Api {
  implicit def collectionLikeUIForTree[V <: Tree](v: V): Evaluator[V] = new Evaluator[V](v)
  implicit def collectionLikeUIForTrees[V <: Tree](v: Seq[V]): Evaluator[Seq[V]] = new Evaluator[Seq[V]](v)
  implicit def collectionLikeUIForTreess[V <: Tree](v: Seq[Seq[V]]): Evaluator[Seq[Seq[V]]] = new Evaluator[Seq[Seq[V]]](v)
  implicit def collectionLikeUIForTreeopt[V <: Tree](v: Option[V]): Evaluator[Option[V]] = new Evaluator[Option[V]](v)
  implicit def forceResultUI[V, A : Monoid, R](x: EvaluatorAndThen[V, A]): ForceResult[V, A, R] = new ForceResult[V, A, R](x)
}

private[meta] trait Aliases {
}
