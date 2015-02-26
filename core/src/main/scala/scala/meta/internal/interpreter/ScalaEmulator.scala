package scala.meta.internal
package interpreter

import environment._

import scala.meta._
import scala.meta.internal.{ ast => i }

object ScalaEmulator {
  def emulate(lst: Seq[i.Term.Arg], args: Seq[Object], env: Env): (Object, Env) = lst match {
    case List(i.Lit.String("Int.$plus(I)I"), i.Term.This(None), _) =>
      (Object(args.head.ref.asInstanceOf[Int] + args.tail.head.ref.asInstanceOf[Int]), env)
  }
}
