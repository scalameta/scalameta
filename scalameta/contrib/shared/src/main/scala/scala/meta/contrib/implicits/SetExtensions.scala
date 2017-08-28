package scala.meta.contrib.implicits

import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.equality.{Structurally, Syntactically}

trait SetExtensions {
  implicit class SetEnrichments[A <: Tree](set: Set[A]) {
    def structurally: Set[Structurally[A]] =
      set.map(Structurally(_))

    def syntactically: Set[Syntactically[A]] =
      set.map(Syntactically(_))
  }
}

object SetExtensions extends SetExtensions
