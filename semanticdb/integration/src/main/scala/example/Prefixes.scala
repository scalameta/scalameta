package prefixes

class C {
  type T
  def m1: T = ???
}

object M {
  type T
  def n1: T = ???
}

object Test {
  val c: C = ???
  def m2: c.T = ???

  def n2: M.T = ???

  import M._
  def n3: T = ???
}
