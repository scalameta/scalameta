object Assign {
  // first group of assignment-related desugarings: setters
  class A {
    var x = 1
  }
  val a1 = new A
  a1.x = 1
  a1.x_=(2)

  // second group of assignment-related desugarings: updates
  class B1 {
    def update(x: Int, value: Int) = ()
  }
  val b1 = new B1
  b1(1) = 2
  b1.update(3, 4)

  // third group of assignment-related desugarings: opAssigns
  // closely follows convertToAssignment in Typers.scala
  // there are three clauses in that pattern match, and we exhaust them here

  // #1: case Ident(_)
  locally {
    var c1 = 1
    c1 += 2
  }

  // #2: case Select(qualqual, vname)
  class C2(var c21: C2, var x2: Int) {
    c21.x2 += 3
    val c22: C2 = ???
    c22.x2 += 4
  }

  // #3: case Apply(fn, indices)
  class C3 {
    def apply(x: Int) = 5
    def update(x: Int, value: Int) = ()
  }
  var c31 = new C3
  val c32 = new C3
  var arg = 6
  c31(7) += 8
  c31(arg) += 9
  c32(10) += 11
  c32(arg) += 12
}