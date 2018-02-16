package example

import scala.math.Ordering

class Methods[T] {
  class List[T]
  type AList[T] = List[T]
  def m1 = ???
  def m2() = ???
  def m3(x: Int) = ???
  def m4(x: Int)(y: Int) = ???
  def m5(x: String) = ???
  def m5(x: Int) = ???
  def m6(x: Int) = ???
  def m6(x: List[T]) = ???
  def m6(x: scala.List[T]) = ???
  def m7[U: Ordering](c: Methods[T], l: List[U]) = ???
  def `m8().`() = ???
  class `m9().`
  def m9(x: `m9().`) = ???
  def m10(x: AList[T]) = ???
  def m11a(x: Predef.type) = ???
  def m11b(x: Example.type) = ???
  def m12a(x: {}) = ???
  def m12b(x: { val x: Int }) = ???
  def m13(x: Int @unchecked) = ???
  def m14(x: T forSome { type T }) = ???
  def m15(x: => Int) = ???
  def m16(x: Int*) = ???
}
