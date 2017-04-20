package scala.meta.contrib.instances

import scala.meta.Defn
import scala.meta._
import scala.meta.contrib.Extract
import scala.meta.contrib.implicits.Equality._

trait DefnExtensions {

  implicit class DefnEnrichments[A <: Defn](d: A) {

    def hasMod(mod: Mod)(implicit ev: Extract[A, Mod]): Boolean =
      ev.extract(d).exists(_.isEqual(mod))
  }

}

object DefnExtensions extends DefnExtensions
