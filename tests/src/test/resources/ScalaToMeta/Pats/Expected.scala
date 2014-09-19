object Pats {
  val List() = List()
  val List() = List()
  val List() = List()
  val List(x1) = List(1)
  var List(x2) = List(2)
  implicit val List(x3) = List(3)
  val List(x4, y4) = List(4, 5)
  var List(x5, y5) = List(6, 7)
  implicit val List(x6, y6) = List(8, 9)
  locally {
    val List() = List()
    val List() = List()
    val List() = List()
    val List(x1) = List(1)
    var List(x2) = List(2)
    implicit val List(x3) = List(3)
    val List(x4, y4) = List(4, 5)
    var List(x5, y5) = List(6, 7)
    implicit val List(x6, y6) = List(8, 9)
    ()
  }
}