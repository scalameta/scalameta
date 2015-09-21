package org.scalameta.typelevel

/**
 * Allows to specify that the type A must be different from type B.
 * Use the same as with =:=
 * */
//thanks to http://stackoverflow.com/questions/6909053/enforce-type-difference/17047288#17047288
@annotation.implicitNotFound(msg = "Cannot prove that ${A} =!= ${B}.")
trait =!=[A, B]

object =!= {
  class Impl[A, B]
  object Impl {
    implicit def neq[A, B] : A Impl B = null
    implicit def neqAmbig1[A] : A Impl A = null
    implicit def neqAmbig2[A] : A Impl A = null
  }

  implicit def materialize[A, B](implicit e: A Impl B): A =!= B = null
}
