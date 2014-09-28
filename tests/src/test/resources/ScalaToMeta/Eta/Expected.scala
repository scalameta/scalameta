object Eta {
  def manualEta(x1: => Int): Unit = {
    x1 _
    def x2 = 2
    x2 _
    def x3(x: Int) = x
    x3 _
    def x4[T](x: T) = x
    {
      x4 _
      ()
    }
  }
  def autoEta(fn: Int => Int): Unit = {
    class C {
      def foo1(x: Int): Int = x
      def foo2(x: Int)(y: Int): Int = x + y
    }
    val c1 = new C
    def c2 = new C
    autoEta(c1.foo1)
    autoEta(c1 foo2 1)
    autoEta(c2.foo1)
    autoEta(c2 foo2 2)
  }
}