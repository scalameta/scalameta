package example

class ValPattern/*example.ValPattern#*/ {

  val (left/*local0*/, right/*local1*/) = (1, 2)
  val Some/*scala.Some.*/(number1/*local2*/) =
    Some/*scala.Some.*/(1)

  var (leftVar/*local3*/, rightVar/*local4*/) = (1, 2)
  var Some/*scala.Some.*/(number1Var/*local5*/) =
    Some/*scala.Some.*/(1)

  def app/*example.ValPattern#app().*/(): Unit/*scala.Unit#*/ = {
    println/*scala.Predef.println(+1).*/(
      (
        number1/*example.ValPattern#number1.*/,
        left/*example.ValPattern#left.*/,
        right/*example.ValPattern#right.*/,
        number1Var/*example.ValPattern#number1Var().*/,
        leftVar/*example.ValPattern#leftVar().*/,
        rightVar/*example.ValPattern#rightVar().*/
      )
    )
  }

}
