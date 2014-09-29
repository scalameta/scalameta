object Types {
  class A
  class B
  class C
  class X
  trait Y
  class Z[T] { type U = T }

  class D1 extends X
  class D2 extends B with Y

  class D3 extends {} with Y
  class D4 extends {} with B with Y

  val x5 = new X
  val x6 = new X{}
  val x7 = new B with Y{}

  @(X @X) class D8

  type T9 = X => X
  type T10 = (X, X)
  type T11 = X { def x: Int }
  type T12 = B with Y { def x: Int }

  new X{} match { case x13: X => }

  class D14[T: Z](val x15: X) {
    self16: X =>
    val x17: X = new X{}
    def x18(x19: X)(x20: X): X = (x20: X): @X
    type T21 >: X <: X
    type T22 = Z[X]
    type T23 = Z[T] forSome { type T }
    type T24 = Z[X]#U
  }
}