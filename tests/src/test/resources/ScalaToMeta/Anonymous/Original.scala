object Anonymous {
  val f1: Function1[Int, Int] = _ + 2
  val f2: Function2[Int, Int, Int] = _ * _
  val f3 = (_: Int) * 2
  val f4: Function1[Boolean, Int] = if (_) 2 else 3
  val f5: Function1[List[Int], List[Int]] = _.map(f1)
  val f6: Function1[List[Int], List[Int]] = _.map(_ + 2)
  val f7: Function1[Int, Int] = _ => 2
}