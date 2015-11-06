package Simple5

object M {
  def foo(xs: List[Int]) = xs match {
    case xs: List[Int] => xs
    case xs @ _ => xs
    case xs => xs
    case List(x, y, z) => xs
    case _ => xs
  }
}