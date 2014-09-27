object Assign {
  class A { var x = 1 }
  val a1 = new A
  a1.x = 1
  a1 x_= 2
  class B1 { def update(x: Int, value: Int) = () }
  val b1 = new B1
  b1(1) = 2
  b1(3) = 4
  locally {
    var c1 = 1
    c1 += 2
  }
  class C2(var c21: C2, var x2: Int) {
    c21.x2 += 3
    val c22: C2 = ???
    c22.x2 += 4
  }
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