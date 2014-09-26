object Eta {
  def foo(x1: => Int) = {
    x1 _
    def x2 = 2
    x2 _
    def x3(x: Int) = x
    x3 _
    def x4[T](x: T) = x
    x4 _
  }
}