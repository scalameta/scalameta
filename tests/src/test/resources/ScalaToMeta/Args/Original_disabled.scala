object Args {
  def foo(x: => Int) = x
  foo(1)
  def bar(xs: Int*) = xs
  bar()
  bar(1)
  bar(1, 2)
  bar(List(1, 2): _*)
}