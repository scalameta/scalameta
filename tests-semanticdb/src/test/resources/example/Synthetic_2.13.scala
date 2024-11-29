package example

class Synthetic/*<=example.Synthetic#*/ {
  List/*=>scala.package.List.*/(1).map/*=>scala.collection.immutable.List#map().*/(_ +/*=>scala.Int#`+`(+4).*/ 2)
  Array/*=>scala.Array.*/.empty/*=>scala.Array.empty().*/[Int/*=>scala.Int#*/].headOption/*=>scala.collection.ArrayOps#headOption().*/
  "fooo".stripPrefix/*=>scala.collection.StringOps#stripPrefix().*/("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name/*<=example.Synthetic#Name.*/ = "name:(.*)".r/*=>scala.collection.StringOps#r().*/
  val x/*<=example.Synthetic#x.*/ #::/*=>scala.package.`#::`.*/ xs/*<=example.Synthetic#xs.*/ = Stream/*=>scala.package.Stream.*/(1, 2)
  val Name/*=>example.Synthetic#Name.*/(name/*<=example.Synthetic#name.*/) = "name:foo"
  1 #::/*=>scala.collection.immutable.Stream.Deferrer#`#::`().*/ 2 #::/*=>scala.collection.immutable.Stream.Deferrer#`#::`().*/ Stream/*=>scala.package.Stream.*/.empty/*=>scala.collection.immutable.Stream.empty().*/

  val lst/*<=example.Synthetic#lst.*/ = 1 #::/*=>scala.collection.immutable.Stream.Deferrer#`#::`().*/ 2 #::/*=>scala.collection.immutable.Stream.Deferrer#`#::`().*/ Stream/*=>scala.package.Stream.*/.empty/*=>scala.collection.immutable.Stream.empty().*/
  lst/*=>example.Synthetic#lst.*/ +/*=>scala.Predef.any2stringadd#`+`().*/ "foo"

  for (x/*<=local3*/ <- 1 to/*=>scala.runtime.RichInt#to().*/ 10; y/*<=local4*/ <- 0 until/*=>scala.runtime.RichInt#until().*/ 10) println/*=>scala.Predef.println(+1).*/(x/*=>local3*/ ->/*=>scala.Predef.ArrowAssoc#`->`().*/ x/*=>local3*/)
  for (i/*<=local5*/ <- 1 to/*=>scala.runtime.RichInt#to().*/ 10; j/*<=local6*/ <- 0 until/*=>scala.runtime.RichInt#until().*/ 10) yield (i/*=>local5*/, j/*=>local6*/)
  for (i/*<=local7*/ <- 1 to/*=>scala.runtime.RichInt#to().*/ 10; j/*<=local8*/ <- 0 until/*=>scala.runtime.RichInt#until().*/ 10 if i/*=>local7*/ %/*=>scala.Int#`%`(+3).*/ 2 ==/*=>scala.Int#`==`(+3).*/ 0) yield (i/*=>local7*/, j/*=>local8*/)

  object s/*<=example.Synthetic#s.*/ {
    def apply/*<=example.Synthetic#s.apply().*/() = 2
    s/*=>example.Synthetic#s.*/()
    s/*=>example.Synthetic#s.*/.apply/*=>example.Synthetic#s.apply().*/()
    case class Bar/*<=example.Synthetic#s.Bar#*/()
    Bar/*=>example.Synthetic#s.Bar.*/()
    null.asInstanceOf/*=>scala.Any#asInstanceOf().*/[Int/*=>scala.Int#*/ => Int/*=>scala.Int#*/](2)
  }

  class J/*<=example.Synthetic#J#*/[T/*<=example.Synthetic#J#[T]*/: Manifest/*=>scala.AnyRef#*/] { val arr/*<=example.Synthetic#J#arr.*/ = Array/*=>scala.Array.*/.empty/*=>scala.Array.empty().*/[T/*=>example.Synthetic#J#[T]*/] }

  class F/*<=example.Synthetic#F#*/
  implicit val ordering/*<=example.Synthetic#ordering.*/: Ordering/*=>scala.package.Ordering#*/[F/*=>example.Synthetic#F#*/] = ???/*=>scala.Predef.`???`().*/
  val f/*<=example.Synthetic#f.*/: Ordered/*=>scala.package.Ordered#*/[F/*=>example.Synthetic#F#*/] = new F/*=>example.Synthetic#F#*/

  import scala.concurrent.ExecutionContext/*=>scala.concurrent.ExecutionContext.*/.Implicits/*=>scala.concurrent.ExecutionContext.Implicits.*/.global/*=>scala.concurrent.ExecutionContext.Implicits.global().*/
  for {
    a/*<=local9*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(1)
    b/*<=local10*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(2)
  } println/*=>scala.Predef.println(+1).*/(a/*=>local9*/)
  for {
    a/*<=local11*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(1)
    b/*<=local12*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(2)
    if a/*=>local11*/ </*=>scala.Int#`<`(+3).*/ b/*=>local12*/
  } yield a/*=>local13*/

}
