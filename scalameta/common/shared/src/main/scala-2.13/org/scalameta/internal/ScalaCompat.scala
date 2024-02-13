package org.scalameta.internal

object ScalaCompat {
  // Removed in 2.13
  trait IndexedSeqOptimized[+A]
  implicit class XtensionScala213ToSeq[T](private val seq: collection.Seq[T]) extends AnyVal {
    def toScalaSeq: Seq[T] = seq.toSeq
  }

  val EOL = System.lineSeparator
}
