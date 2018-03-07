package example

import com.javacp.InnerClasses

class MetacJava {
  InnerClasses.StaticInner.isStatic()
  new InnerClasses.StaticInner().isNotStatic()
  val inner = new InnerClasses()
  val overload1 = new inner.Overload1()
  val overload2 = new inner.Overload2()
  inner.overload(new overload1.A())
  inner.overload(new overload2.A())
  val staticInner = new InnerClasses.StaticInner()
  val nonStatic = new staticInner.NonStatic()
  nonStatic.method(nonStatic)
}
