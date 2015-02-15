package org.scalameta

import scala.{Seq => _}
import scala.collection.immutable.Seq

package object collections {
  implicit class RichCollectionHelper[T](seq: Seq[T]) {
    // TODO: this is obviously very naive
    def isLazy = !seq.isInstanceOf[List[T]]
  }
}