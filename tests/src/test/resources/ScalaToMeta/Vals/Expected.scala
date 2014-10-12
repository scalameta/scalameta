object Vals {
  val List() = List()
  val List() = List()
  val List() = List()
  lazy val List() = List()
  val x1 = List(1)
  var x2 = List(2)
  implicit val x3 = List(3)
  implicit lazy val x4 = List(4)
  val List(x5) = List(5)
  var List(x6) = List(6)
  implicit val List(x7) = List(7)
  implicit lazy val List(x8) = List(8)
  val List(x9, y9) = List(9, 10)
  var List(x10, y10) = List(11, 12)
  implicit val List(x11, y11) = List(13, 14)
  implicit lazy val List(x12, y12) = List(15, 16)
  locally {
    val List() = List()
    val List() = List()
    val List() = List()
    lazy val List() = List()
    val x1 = List(1)
    var x2 = List(2)
    implicit val x3 = List(3)
    implicit lazy val x4 = List(4)
    val List(x5) = List(5)
    var List(x6) = List(6)
    implicit val List(x7) = List(7)
    implicit lazy val List(x8) = List(8)
    val List(x9, y9) = List(9, 10)
    var List(x10, y10) = List(11, 12)
    implicit val List(x11, y11) = List(13, 14)
    implicit lazy val List(x12, y12) = List(15, 16)
  }
  trait Trait1 {
    val List() = List()
    val List() = List()
    val List() = List()
    lazy val List() = List()
    val x1 = List(1)
    var x2 = List(2)
    implicit val x3 = List(3)
    implicit lazy val x4 = List(4)
    val List(x5) = List(5)
    var List(x6) = List(6)
    implicit val List(x7) = List(7)
    implicit lazy val List(x8) = List(8)
    val List(x9, y9) = List(9, 10)
    var List(x10, y10) = List(11, 12)
    implicit val List(x11, y11) = List(13, 14)
    implicit lazy val List(x12, y12) = List(15, 16)
  }
  trait Trait2 {
    val x1: Int
    var x2: Int
  }
  trait Trait3 {
    protected[this] val x: Int
    private lazy val y = x
  }
}