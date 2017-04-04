package scala.meta.contrib.implicits

import scala.meta.contrib._

trait ExtractExtensions {

  /**
    * The motivation for this implicit enrichment is to extract values out of trees
    * that could have been fields on the classes. However, these values are
    * not included as fields on the tree nodes because of various reasons.
    */
  implicit class XtensionExtractors[A](a: A) {
    def extract[B](implicit ev: Extract[A, B]): Seq[B] = ev.extract(a)
  }
}
