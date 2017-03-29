package scala.meta.contrib.implicits

import scala.meta._
import scala.meta.contrib.Extract
import scala.collection.immutable.Seq

trait ExtractPimps {
  implicit class XtensionExtractors[A](a: A) {

    /**
      * Logical extraction of B from A.
      *
      * Meaning that this is supposed to replicate what the use thinks should happen.
      * Not the actual class representation
      *
      * eg. Extract[Defn.Class, Seq[Stat]]
      *
      * is actually extracting the stats from the Template, which is a child of Defn.Class.
      */
    def extract[B](implicit ev: Extract[A, B]): Seq[B] = ev.extract(a)
  }
}
