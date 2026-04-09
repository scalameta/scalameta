package scala.meta.internal

object SemanticCompat {
  implicit class XtensionScala213ToSeq[T](private val seq: collection.Seq[T]) extends AnyVal {
    def toScalaSeq: Seq[T] = seq
  }
}
