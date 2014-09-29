object PartialFunction {
  def takesLambda(fn: Int => Int) = ???
  def takesPartial(fn: PartialFunction[Int, Int]) = ???
  val fn1: Int => Int = {
    case 1 =>
      2
  }
  val fn2: PartialFunction[Int, Int] = {
    case 2 =>
      3
  }
  takesLambda({
    case 3 =>
      4
  })
  takesPartial({
    case 4 =>
      5
  })
  def lam0(fn: () => Int) = ???
  def lam1(fn: Int => Int) = ???
  def lam2(fn: (Int, Int) => Int) = ???
  lam0({
    case () =>
      0
  })
  lam1({
    case _ =>
      1
  })
  lam2({
    case (_, _) =>
      2
  })
}