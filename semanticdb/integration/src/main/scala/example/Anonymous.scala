class C {
  this: C =>

  def m1[T[_], _] = ???
  def m2: Map[_, List[_]] = ???
  locally {
    ??? match { case _: List[_] => }
  }
  locally {
    val x: Int => Int = _ => ???
  }
}
