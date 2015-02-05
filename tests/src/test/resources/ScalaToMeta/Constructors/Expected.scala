object Constructors {
  class C1(x: Int)
  case class D1(x: Int)
  class C2(val x: Int)
  case class D2(x: Int)
  class C3(x: Int)
  class C4(x: Int) { def this() = this(0) }
  class C5(x: Int) {
    def this() {
      this(0)
      println("hello world")
      ()
    }
  }
  class C6[T: scala.reflect.ClassTag](x: Int) { def this() = this(0) }
}