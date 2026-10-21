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
      val (left/*<=local0*/, right/*<=local1*/) = (1, 2)
      val Some/*=>scala.Some.*/(number1/*<=local3*/) =
        Some/*=>scala.Some.*/(1)

      var (leftVar/*<=local4*/, rightVar/*<=local5*/) = (1, 2)
      var Some/*=>scala.Some.*/(number1Var/*<=local7*/) =
        Some/*=>scala.Some.*/(1)
      println/*=>scala.Predef.println(+1).*/(
        (
          number1/*=>local3*/,
          left/*=>local0*/,
          right/*=>local1*/,
          number1Var/*=>local7*/,
          leftVar/*=>local4*/,
          rightVar/*=>local5*/
        )
      )
    }
  }

}
