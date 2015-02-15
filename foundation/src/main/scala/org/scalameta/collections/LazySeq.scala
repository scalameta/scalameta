package org.scalameta.collections

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.generic.CanBuildFrom

final class LazySeq[A](thunk: () => Seq[A]) extends Seq[A] {
  private lazy val cache = thunk()
  def iterator: Iterator[A] = cache.iterator
  def apply(idx: Int): A = cache.apply(idx)
  def length: Int = cache.length
  // TODO: allright, so how do I consistently make all collection operations to be lazy?
  // of course, this is a very naive way of doing things
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Seq[A], B, That]): That = {
    LazySeq(cache.map(f).asInstanceOf[Seq[B]]).asInstanceOf[That]
  }
}

object LazySeq {
  def apply[A](seq: => Seq[A]): LazySeq[A] = new LazySeq(() => seq)
}