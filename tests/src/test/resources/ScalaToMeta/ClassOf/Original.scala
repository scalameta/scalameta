class Original {
  val x1 = classOf[Byte]
  val y1: Class[Short] = classOf
  val x2 = Predef.classOf[Int]
  val y2: Class[Long] = Predef.classOf
  val y3 = classOf[Int].getClass
}