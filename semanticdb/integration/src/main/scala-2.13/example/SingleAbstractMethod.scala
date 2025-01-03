package example

class SingleAbstractMethod {

  trait SAM {
    def foo(a: Int): Int
  }
  def withSAM(a: SAM) = a.foo(0)

  withSAM(_ + 1)
  withSAM((x: Int) => x - 1)

  def funcSAM(y: Int) = y * 2
  withSAM(funcSAM)

}
