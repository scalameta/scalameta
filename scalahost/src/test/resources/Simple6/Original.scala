package Simple6

object M {
  def enclosingMethod: Unit = {
    def foo[T](x: T => T) = ???
    foo((x: Int) => x)

    def bar(x: Int => Int) = ???
    bar(x => x)

    val fn: String => String = x => x
  }
}