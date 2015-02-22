object Super {
  class C { def ++=(x: Int) = () }
  class D extends C { override def ++=(x: Int) = super.++=(x) }

  class T1 { def foo: Int = ??? }
  trait T2
  new T1 with T2 {
    override def foo: Int = super.foo
  }
}