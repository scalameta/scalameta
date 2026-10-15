package prefixes

class C {
  type T
  def m1: T = ???

  object N {
    type U
  }
  def k1: N.U = ???
}

object M {
  type T
  def n1: T = ???
}

object O extends C {
  def o1: T = ???
}

object Test {
  val c: C = ???
  def m2: c.T = ???
  def k2: c.N.U = ???
  import c.N._
  def k3: U = ???

  def n2: M.T = ???

  import M._
  def n3: T = ???
}

class B {
  class X
  def b1: B.Z = ???
}

object B extends B {
  type Z
  def b2: X = ???
  def b3: B.X = ???
}

object P {
  class Q {
    class X
  }
  object Q extends Q
}

object TestB {
  def b4: B.X = ???
  def b5: B#X = ???
  def b6: B.Z = ???
  def b7: P.Q.X = ???
}
