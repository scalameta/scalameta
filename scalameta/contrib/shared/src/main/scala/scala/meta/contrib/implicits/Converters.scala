package scala.meta.contrib.implicits

import scala.meta._

trait Converters {
  implicit class convertName(n: Name) {
    def asTerm: Term.Name = Term.Name(n.value)
    def asType: Type.Name = Type.Name(n.value)
    def asPat: Pat.Var = Pat.Var(n.asTerm)
  }
}

object Converters extends Converters
