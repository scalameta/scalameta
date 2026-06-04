package example

class Synthetic/*<=example.Synthetic#*/ {
  List/*=>scala.collection.immutable.List.*/(1).map/*=>scala.collection.immutable.List#map().*/(_ +/*=>scala.Int#`+`(+4).*/ 2)
  Array/*=>scala.Array.*/.empty/*=>scala.Array.empty().*/[Int/*=>scala.Int#*/].headOption/*=>scala.collection.TraversableLike#headOption().*/
  "fooo".stripPrefix/*=>scala.collection.immutable.StringLike#stripPrefix().*/("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name/*<=example.Synthetic#Name.*/ = "name:(.*)".r/*=>scala.collection.immutable.StringLike#r().*/
  val x/*<=example.Synthetic#x.*/ #::/*=>scala.package.`#::`.*/ xs/*<=example.Synthetic#xs.*/ = Stream/*=>scala.package.Stream.*/(1, 2)
  val Name/*=>example.Synthetic#Name.*/(name/*<=example.Synthetic#name.*/) = "name:foo"
  1 #::/*=>scala.collection.immutable.Stream.ConsWrapper#`#::`().*/ 2 #::/*=>scala.collection.immutable.Stream.ConsWrapper#`#::`().*/ Stream/*=>scala.package.Stream.*/.empty/*=>scala.collection.immutable.Stream.empty().*/

  val lst/*<=example.Synthetic#lst.*/ = 1 #::/*=>scala.collection.immutable.Stream.ConsWrapper#`#::`().*/ 2 #::/*=>scala.collection.immutable.Stream.ConsWrapper#`#::`().*/ Stream/*=>scala.package.Stream.*/.empty/*=>scala.collection.immutable.Stream.empty().*/
  lst/*=>example.Synthetic#lst.*/ +/*=>scala.Predef.any2stringadd#`+`().*/ "foo"

  // See https://github.com/scalameta/scalameta/issues/1045
  val ys/*<=example.Synthetic#ys.*/: List/*=>scala.package.List#*/[Int/*=>scala.Int#*/] = Nil/*=>scala.collection.immutable.Nil.*/
  2 ::/*=>scala.collection.immutable.List#`::`().*/ ys/*=>example.Synthetic#ys.*/

  for (x/*<=local8*/ <- 1 to/*=>scala.runtime.RichInt#to().*/ 10; y/*<=local9*/ <- 0 until/*=>scala.runtime.RichInt#until().*/ 10) println/*=>scala.Predef.println(+1).*/(x/*=>local8*/ ->/*=>scala.Predef.ArrowAssoc#`->`().*/ x/*=>local8*/)
  for (i/*<=local10*/ <- 1 to/*=>scala.runtime.RichInt#to().*/ 10; j/*<=local11*/ <- 0 until/*=>scala.runtime.RichInt#until().*/ 10) yield (i/*=>local10*/, j/*=>local11*/)
  for (i/*<=local12*/ <- 1 to/*=>scala.runtime.RichInt#to().*/ 10; j/*<=local13*/ <- 0 until/*=>scala.runtime.RichInt#until().*/ 10 if i/*=>local12*/ %/*=>scala.Int#`%`(+3).*/ 2 ==/*=>scala.Int#`==`(+3).*/ 0) yield (i/*=>local12*/, j/*=>local13*/)

  object s/*<=example.Synthetic#s.*/ {
    def apply/*<=example.Synthetic#s.apply().*/() = 2
    s/*=>example.Synthetic#s.*/()
    s/*=>example.Synthetic#s.*/.apply/*=>example.Synthetic#s.apply().*/()
    case class Bar/*<=example.Synthetic#s.Bar#*/()
    Bar/*=>example.Synthetic#s.Bar.*/()
    null.asInstanceOf/*=>scala.Any#asInstanceOf().*/[Int/*=>scala.Int#*/ => Int/*=>scala.Int#*/](2)
  }

  class J/*<=example.Synthetic#J#*/[T/*<=example.Synthetic#J#[T]*/: Manifest/*=>scala.Predef.Manifest#*/] { val arr/*<=example.Synthetic#J#arr.*/ = Array/*=>scala.Array.*/.empty/*=>scala.Array.empty().*/[T/*=>example.Synthetic#J#[T]*/] }

  class F/*<=example.Synthetic#F#*/
  implicit val ordering/*<=example.Synthetic#ordering.*/: Ordering/*=>scala.package.Ordering#*/[F/*=>example.Synthetic#F#*/] = ???/*=>scala.Predef.`???`().*/
  val f/*<=example.Synthetic#f.*/: Ordered/*=>scala.package.Ordered#*/[F/*=>example.Synthetic#F#*/] = new F/*=>example.Synthetic#F#*/

  import scala.concurrent.ExecutionContext/*=>scala.concurrent.ExecutionContext.*/.Implicits/*=>scala.concurrent.ExecutionContext.Implicits.*/.global/*=>scala.concurrent.ExecutionContext.Implicits.global.*/
  for {
    a/*<=local14*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(1)
    b/*<=local15*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(2)
  } println/*=>scala.Predef.println(+1).*/(a/*=>local14*/)
  for {
    a/*<=local16*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(1)
    b/*<=local17*/ <- scala.concurrent.Future/*=>scala.concurrent.Future.*/.successful/*=>scala.concurrent.Future.successful().*/(2)
    if a/*=>local16*/ </*=>scala.Int#`<`(+3).*/ b/*=>local17*/
  } yield a/*=>local16*/

}
