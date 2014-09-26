object Anonymous {
  val f1: Int => Int = _ + 2
  val f2: (Int, Int) => Int = _ * _
  val f3 = (_: Int) * 2
  val f4: Boolean => Int = if (_) 2 else 3
  val f5: List[Int] => List[Int] = _.map(f1)
  val f6: List[Int] => List[Int] = _.map(_ + 2)
  val f7: Int => Int = _ => 2
}