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
  val x6 = new Y{}
  val x7 = new B with Y{}

  @(Y @Y) class D8

  type T9 = Y => Y
  type T10 = (Y, Y)
  type T11 = Y { def x: Int }
  type T12 = B with Y { def x: Int }

  new C{} match { case x13: Y => }

  class D14[T: Z](val x15: Y) {
    self16: Y =>
    val x17: Y = new Y{}
    def x18(x19: Y)(x20: Y): Y = (x20: Y): @Y
    type T21 >: Y <: T
    type T22 = Z[Y]
    type T23 = Z[T] forSome { type T }
    type T24 = Z[Y]#U
  }
}