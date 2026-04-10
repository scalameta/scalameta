package scala.meta
package classifiers

@scala.annotation.implicitNotFound("don't know how to check whether ${T} is ${U}")
trait Classifier[T, U] {
  def apply(x: T): Boolean
}
