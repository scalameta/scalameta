package example

class ValPattern/*<=example.ValPattern#*/ {

  val (left/*<=example.ValPattern#left.*/, right/*<=example.ValPattern#right.*/) = (1, 2)
  val Some/*=>scala.Some.*/(number1/*<=example.ValPattern#number1.*/) =
    Some/*=>scala.Some.*/(1)

  var (leftVar/*<=example.ValPattern#leftVar().*/, rightVar/*<=example.ValPattern#rightVar().*/) = (1, 2)
  var Some/*=>scala.Some.*/(number1Var/*<=example.ValPattern#number1Var().*/) =
    Some/*=>scala.Some.*/(1)

  def app/*<=example.ValPattern#app().*/(): Unit/*=>scala.Unit#*/ = {
    println/*=>scala.Predef.println(+1).*/(
      (
        number1/*=>example.ValPattern#number1.*/,
        left/*=>example.ValPattern#left.*/,
        right/*=>example.ValPattern#right.*/,
        number1Var/*=>example.ValPattern#number1Var().*/,
        leftVar/*=>example.ValPattern#leftVar().*/,
        rightVar/*=>example.ValPattern#rightVar().*/
      )
    )
    locally/*=>scala.Predef.locally().*/ {
      val (left/*<=local6*/, right/*<=local7*/) = (1, 2)
      val Some/*=>scala.Some.*/(number1/*<=local10*/) =
        Some/*=>scala.Some.*/(1)

      var (leftVar/*<=local11*/, rightVar/*<=local12*/) = (1, 2)
      var Some/*=>scala.Some.*/(number1Var/*<=local15*/) =
        Some/*=>scala.Some.*/(1)
      println/*=>scala.Predef.println(+1).*/(
        (
          number1/*=>local9*/,
          left/*=>local6*/,
          right/*=>local7*/,
          number1Var/*=>local14*/,
          leftVar/*=>local11*/,
          rightVar/*=>local12*/
        )
      )
    }
  }

}
