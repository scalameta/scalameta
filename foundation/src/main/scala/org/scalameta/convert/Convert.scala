package org.scalameta

import scala.annotation.implicitNotFound

package convert {
  @implicitNotFound("don't know how to convert ${A} to ${B}")
  trait Convert[A, B] {
    def apply(a: A): B
  }

  // TODO: add default instances
  object Convert {
    def apply[A, B](f: A => B): Convert[A, B] =
      new Convert[A, B] { def apply(a: A): B = f(a) }

    implicit def implicitToExplicitConversion[A, B](implicit conv: A => B): Convert[A, B] = Convert(conv)
  }
}

package object convert {
  implicit class ConvertA2B[A](a: A) {
    def convert[B](implicit convert: Convert[A, B]): B = convert(a)
    def convertOrElse[B](orElse: => B)(implicit convert: Convert[A, B]): B =
      try convert(a)
      catch { case _: Exception => orElse }
  }
}
