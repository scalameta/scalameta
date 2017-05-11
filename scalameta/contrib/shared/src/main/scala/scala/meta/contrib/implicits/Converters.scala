package scala.meta.contrib.implicits

import scala.meta._

trait Converters {
  implicit class convertName(n: Name) {
    def asTerm: Term.Name = Term.Name(n.value)
    def asType: Type.Name = Type.Name(n.value)
    def asCtorRef: Ctor.Ref.Name = Ctor.Ref.Name(n.value)
    def asPat: Pat.Var.Term = Pat.Var.Term(n.asTerm)
  }

  implicit class XtensionClassTypeArg(arg: Type.Arg) {
    def toType: Type = arg match {
      case Type.Arg.Repeated(tpe) => tpe
      case Type.Arg.ByName(tpe) => tpe
      case tpe: Type => tpe
    }
  }
}

object Converters extends Converters
