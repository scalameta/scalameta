object Test {
  trait ClassParams {
    class C1(x: Int)
    class C2(val x: Int)
    class C3(var x: Int)
    class C4(private[this] val x: Int)
    class C5(private val x: Int)
    class D1(implicit val x: Int)
    class D2(implicit var x: Int)
    class D3(private[this] implicit val x: Int)
    class D4(private implicit val x: Int)
    class E1(protected val x: Int)
    class E2(protected var x: Int)
    class E3(protected[this] val x: Int)
    class F1(final val x: Int)
    class F2(private[this] final val x: Int)
    class F3(private final val x: Int)
  }
  trait EagerVals {
    class C1 { val x: Int = 1 }
    class C2 { var x: Int = 2 }
    class C3 { private[this] val x: Int = 3 }
    class C4 { private val x: Int = 4 }
    class D1 { implicit val x: Int = 1 }
    class D2 { implicit var x: Int = 2 }
    class D3 { private[this] implicit val x: Int = 3 }
    class D4 { private implicit val x: Int = 4 }
    class E1 { protected val x: Int = 1 }
    class E2 { protected var x: Int = 2 }
    class E3 { protected[this] val x: Int = 3 }
    class F1 { final val x: Int = 1 }
    class F2 { private[this] final val x: Int = 2 }
    class F3 { private final val x: Int = 3 }
  }
  trait LazyVals {
    class C1 { lazy val x: Int = 1 }
    class C3 { private[this] lazy val x: Int = 3 }
    class C4 { private lazy val x: Int = 4 }
    class D1 { implicit lazy val x: Int = 1 }
    class D3 { private[this] implicit lazy val x: Int = 3 }
    class D4 { private implicit lazy val x: Int = 4 }
    class E1 { protected lazy val x: Int = 1 }
    class E3 { protected[this] lazy val x: Int = 3 }
    class F1 { final lazy val x: Int = 1 }
    class F2 { private[this] final lazy val x: Int = 2 }
    class F3 { private final lazy val x: Int = 3 }
  }
}