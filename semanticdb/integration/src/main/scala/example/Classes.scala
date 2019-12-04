package classes

class C1(val x1: Int) extends AnyVal

class C2(val x2: Int) extends AnyVal
object C2

case class C3(x: Int)

case class C4(x: Int)
object C4

object M {
  implicit class C5(x: Int)
}

case class C6(private val x: Int)

class C7(x: Int)

class C8(private[this] val x: Int)

class C9(private[this] var x: Int)

object N {
  val anonClass = new C7(42) {
    val local = ???
  }
  val anonFun = List(1).map { i =>
    val local = 2
    local + 2
  }
}

object CaseClasses1 {
  case class CClass(i: String)

  val cclass1 = CClass.apply(i = "").copy(i = "")
  val cclass2 = CClass.apply(i = "")

}

object Chain {
  class A {
    def tst1(i: Int): A = this
    def tst2(i: Int): A = this
    def tst3(i: Int, j: Int): A = this
  }

  val a = new A
  a.tst1(i = 1).tst2(i = 2)
  a.tst2(i = 1).tst1(i = 2)
  a.tst2(i = 1).tst1(i = 2).tst3(i = 1, 2)
  a.tst2(i = 1).tst1(i = 2).tst3(1, j = 2)
}