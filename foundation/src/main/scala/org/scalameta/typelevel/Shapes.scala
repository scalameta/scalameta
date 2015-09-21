package org.scalameta.typelevel

@annotation.implicitNotFound(msg = "${A} is a tuple.")
trait NotTuple[A]

object NotTuple {
  class Impl[A]
  object Impl {
    implicit def ok[A]: Impl[A] = null
    implicit def ambig1[A, B]: Impl[(A, B)] = null
    implicit def ambig2[A, B]: Impl[(A, B)] = null
  }

  implicit def materialize[A](implicit e: Impl[A]): NotTuple[A] = null
}
