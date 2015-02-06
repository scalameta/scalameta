package org.scalameta.collections

import scala.{Seq => _}
import scala.collection.immutable.Seq

final class LazySeq[T](thunk: () => Seq[T]) extends Seq[T] {
  private lazy val cache = thunk()
  def iterator: Iterator[T] = cache.iterator
  def apply(idx: Int): T = cache.apply(idx)
  def length: Int = cache.length
}

object LazySeq {
  def apply[T](seq: => Seq[T]): LazySeq[T] = new LazySeq(() => seq)
}