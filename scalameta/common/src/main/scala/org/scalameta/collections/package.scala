package org.scalameta

import scala.{Seq => _}
import scala.collection.immutable.Seq

package object collections {
  implicit class XtensionCollection[T](seq: Seq[T]) {
    // TODO: We'll have to think what kind of collections
    // we want to use to signify laziness of tree's children.
    def isLazy: Boolean = seq.isInstanceOf[Stream[T]]
  }
}