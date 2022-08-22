package org.scalameta.internal

import scala.collection.immutable

trait MacroCompat

object MacroCompat {
  val productFieldNamesAvailable = false
}

object ScalaCompat {
  type IndexedSeqOptimized[+A] = scala.collection.IndexedSeqOptimized[A, immutable.IndexedSeq[A]]
  implicit class XtensionScala213ToSeq[T](private val seq: collection.Seq[T]) extends AnyVal {
    def toScalaSeq: Seq[T] = seq
  }
}
