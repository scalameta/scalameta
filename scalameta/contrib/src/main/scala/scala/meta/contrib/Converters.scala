package scala.meta.contrib.conversion

import scala.meta._

trait Converters {
  implicit class convertName(n: Name) {
    def asTerm: Term.Name = Term.Name(n.value)
    def asType: Type.Name = Type.Name(n.value)
    def asCtorRef: Ctor.Ref.Name = Ctor.Ref.Name(n.value)
    def asPat: Pat.Var.Term = Pat.Var.Term(n.asTerm)
  }
}

object Converters extends Converters
