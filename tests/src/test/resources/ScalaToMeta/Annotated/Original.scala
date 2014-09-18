object Annotated {
  val x1 = 1: @unchecked
  val x2 = 1: Int @unchecked
  class foo extends scala.annotation.StaticAnnotation
  class bar(x: Int) extends scala.annotation.StaticAnnotation
  val y1 = 1: @foo @bar(2)
  val y2 = 1: @bar(y1) @foo
  val z1 = 1: Int @foo @bar(3)
  val z2 = 1: Int @bar(z1) @foo
}