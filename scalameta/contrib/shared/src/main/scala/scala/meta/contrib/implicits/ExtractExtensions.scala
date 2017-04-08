package scala.meta.contrib.implicits

import scala.collection.immutable.Seq
import scala.meta.contrib._

trait ExtractExtensions {
  implicit class XtensionExtractors[A](a: A) {
    def extract[B](implicit ev: Extract[A, B]): Seq[B] = ev.extract(a)
  }
}
