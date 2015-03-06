package org.scalameta

import scala.annotation.implicitNotFound

package convert {
  // NOTE: neither A nor B can have variance annotations
  // we can't turn A into -A because contravariant implicits don't work
  // we can't turn B into +B because a converter from A to B isn't necessarily a conversion from A to a supertype of B
  // take serialization, for instance. a deserializer for Foo is not the same as a deserializer for AnyRef.
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
  implicit class XtensionConvert[A](a: A) {
    def convert[B](implicit convert: Convert[A, B]): B = convert(a)
    def convertOrElse[B](orElse: => B)(implicit convert: Convert[A, B]): B =
      try convert(a)
      catch { case _: Exception => orElse }
  }
}
