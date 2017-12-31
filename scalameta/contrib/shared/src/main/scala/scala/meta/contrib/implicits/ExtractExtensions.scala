package scala.meta.contrib.implicits

import scala.meta._
import scala.meta.contrib._

trait ExtractExtensions {

  type StatExtractor[A] = Extract[A, Stat]
  type ModExtractor[A] = Extract[A, Mod]

  implicit class XtensionExtractors[A](a: A) {
    def extract[B](implicit ev: Extract[A, B]): List[B] = ev.extract(a)

    def hasMod(mod: Mod)(implicit ev: Extract[A, Mod]): Boolean =
      ev.extract(a).exists(_.isEqual(mod))
  }
}

object ExtractExtensions extends ExtractExtensions
