package scala.meta.internal
package interpreter

import Environment._
import scala.meta.semantic._
import scala.meta.semantic.Context
import scala.meta.dialects.Scala211

import scala.meta._
import scala.meta.internal.{ ast => m }

object ScalaEmulator {
  def emulate(lst: Seq[String], args: Seq[Object], env: Env)(implicit c: Context): (Object, Env) = lst match {
    case List("I", "$plus", "(I)I") =>
      (Object(args.head.as[Int] + args.tail.head.as[Int], t"Int"), env)
    case List("I", "$minus", "(I)I") =>
      (Object(args.head.as[Int] - args.tail.head.as[Int], t"Int"), env)
    case List("I", "$times", "(I)I") =>
      (Object(args.head.as[Int] * args.tail.head.as[Int], t"Int"), env)
    case List("I", "$greater", "(I)I") =>
      (Object(args.head.as[Int] > args.tail.head.as[Int], t"Boolean"), env)
    case List("I", "$less", "(I)I") =>
      (Object(args.head.as[Int] < args.tail.head.as[Int], t"Boolean"), env)
    case List("I", "$eq$eq", "(I)Z") =>
      (Object(args.head.as[Int] == args.tail.head.as[Int], t"Boolean"), env)
    case List("I", "$bang$eq", "(I)Z") =>
      (Object(args.head.as[Int] != args.tail.head.as[Int], t"Boolean"), env)
    case List("Ljava/lang/String;", "$plus", "(Ljava/lang/Object;)Ljava/lang/String;") =>
      (Object(args.head.as[String] + args.tail.head.as[AnyRef].toString, t"String"), env)
    case List("Z", "$bar$bar", "(Z)Z") =>
      (Object(args.head.as[Boolean] || args.tail.head.as[Boolean], t"Boolean"), env)
    case List("Z", "unary_$bang", "()Z") =>
      (Object(!args.head.as[Boolean], t"Boolean"), env)
    case List("Ljava/lang/Object;", "toString", "()Ljava/lang/String;") =>
      (Object(args.head.ref.toString, t"String"), env)
    case List("I", "$plus", "(Ljava/lang/String;)Ljava/lang/String;") =>
      (Object(args.head.ref.toString + args.tail.head.ref.toString, t"String"), env)
    case _ =>
      println(s"Unsupported op: $lst")
      ???
  }
}
