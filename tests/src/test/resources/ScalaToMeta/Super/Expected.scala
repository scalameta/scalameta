object Super {
  class C { def ++=(x: Int) = () }
  class D extends C { override def ++=(x: Int) = super.++=(x) }
}