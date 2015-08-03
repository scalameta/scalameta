package pkg
private[pkg] class C1
protected[pkg] class C2
object M {
  private[pkg] class D1
  private[M] class D2
  protected[pkg] class D3
  protected[M] class D4
  private val x1 = 1
  private[this] val x2 = 2
  private[pkg] var x3 = 3
  private[M] var x4 = 4
  protected val y1 = 1
  protected[this] val y2 = 2
  protected[pkg] var y3 = 3
  protected[M] var y4 = 4
}