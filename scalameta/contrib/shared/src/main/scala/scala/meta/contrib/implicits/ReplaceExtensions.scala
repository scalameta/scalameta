package scala.meta.contrib.implicits

import scala.meta._
import scala.meta.contrib._

trait ReplaceExtensions {

  type StatReplacer[A] = Replace[A, Stat]
  type ModReplacer[A] = Replace[A, Mod]

  implicit class ExtensionReplacers[A](a: A) {
    def withStats(bs: List[Stat])(implicit ev: Replace[A, Stat]): A = ev.replace(a, bs)
    def withMods(bs: List[Mod])(implicit ev: Replace[A, Mod]): A = ev.replace(a, bs)
  }
}

object ReplaceExtensions extends ReplaceExtensions
