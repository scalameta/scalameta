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
  def m7a[U: Ordering](c: Methods[T], l: List[U]) = ???
  def m7b[U <% T](l: List[U]) = ???
  def `m8().`() = ???
  class `m9().`
  def m9(x: `m9().`) = ???
  def m10(x: AList[T]) = ???
  def m11(x: Predef.type) = ???
  def m11(x: Example.type) = ???
  def m12a(x: {}) = ???
  def m12b(x: { val x: Int }) = ???
  def m13(x: Int @unchecked) = ???
  def m14(x: T forSome { type T }) = ???
  def m15(x: => Int) = ???
  def m16(x: Int*) = ???
}

class Usages {
  val m = new Methods[Int]
  m.m1
  m.m2()
  m.m3(0)
  m.m4(0)(0)
  m.m5("")
  m.m5(0)
  m.m6(0)
  m.m6(new m.List[Int])
  m.m6(Nil)
  m.m7a(m, new m.List[Int])
  m.m7b(new m.List[Int])
  m.`m8().`()
  m.m9(null)
  m.m10(null)
  m.m11(Predef)
  m.m11(Example)
  m.m12a(null)
  m.m12b(null)
  m.m13(0)
  m.m14(null)
  m.m15(0)
  m.m16(0)
}
