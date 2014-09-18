object Implicits {
  implicit val x: Int = 2
  def foo1(implicit ev: Int) = ???
  def foo2(x: Int)(implicit ev: Int) = ???
  foo1
  foo1(1)
  foo2(2)
  foo2(2)(3)
  class C
  def bar = new C
  implicit def c2bool(c: C): Boolean = ???
  implicit def c2int(c: C)(implicit ev: Int): Int = ???
  if (bar) 1 else 2
  val baz: Int = bar
}