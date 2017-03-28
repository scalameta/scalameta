package scala.meta
package common

// NOTE: neither A nor B can have variance annotations
// we can't turn A into -A because contravariant implicits don't work
// we can't turn B into +B because a converter from A to B isn't necessarily a conversion from A to a supertype of B
// take serialization, for instance. a deserializer for Foo is not the same as a deserializer for AnyRef.
@scala.annotation.implicitNotFound("don't know how to convert ${A} to ${B}")
trait Convert[A, B] {
  def apply(a: A): B
}

object Convert {
  def apply[A, B](f: A => B): Convert[A, B] = {
    new Convert[A, B] { def apply(a: A): B = f(a) }
  }

  implicit def trivial[T, U <: T]: Convert[U, T] = Convert(x => x)
}
