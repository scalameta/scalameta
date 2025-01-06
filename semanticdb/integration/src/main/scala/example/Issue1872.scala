package example

object Issue1872 {

  val List(a1_1) = List(1)

  val a2_1, List(/*a2*/ a2_2) = List(1, 2)
  val b2_1, List(/*b2*/ b2_2: Int) = List(1, 2)
  val c2_1, List(/*c2*/ c2_2 @ _) = List(1, 2)

  val a3_1, List(a3_2, a3_3) = List(1, 2, 3)

}
