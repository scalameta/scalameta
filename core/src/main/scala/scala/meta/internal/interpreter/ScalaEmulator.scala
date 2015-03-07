package scala.meta.internal
package interpreter

import environment._
import scala.meta.semantic._
import scala.meta.semantic.Context
import scala.meta.dialects.Scala211

import scala.meta._
import scala.meta.internal.{ ast => i }

object ScalaEmulator {
  def emulate(lst: Seq[String], args: Seq[Object], env: Env)(implicit c: Context): (Object, Env) = lst match {
    case List("I", "$plus", "(I)I") =>
      (Object(args.head.as[Int] + args.tail.head.as[Int], t"Int"), env)
    case List("Ljava/lang/String;", "$plus", "(Ljava/lang/Object;)Ljava/lang/String;") =>
      (Object(args.head.as[String] + args.tail.head.as[AnyRef].toString, t"String"), env)
  }
}
