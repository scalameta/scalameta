package Simple7

object M {
  def enclosingMethod: Unit = {
    def foo[T](x: T => T) = ???
    def bar(x: Int): Int = ???
    foo(bar)
    foo(foo[Int])
  }
}