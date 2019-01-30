package example
import scala.language/*=>scala.language.*/.higherKinds/*=>scala.language.higherKinds.*/

class Anonymous/*<=example.Anonymous#*/ {
  this: Anonymous/*=>example.Anonymous#*/ =>

  def m1/*<=example.Anonymous#m1().*/[T/*<=example.Anonymous#m1().[T]*/[_], _] = ???/*=>scala.Predef.`???`().*/
  def m2/*<=example.Anonymous#m2().*/: Map/*=>scala.Predef.Map#*/[_, List/*=>scala.package.List#*/[_]] = ???/*=>scala.Predef.`???`().*/
  locally/*=>scala.Predef.locally().*/ {
    ???/*=>scala.Predef.`???`().*/ match { case _: List/*=>scala.package.List#*/[_] => }
  }
  locally/*=>scala.Predef.locally().*/ {
    val x/*<=local3*/: Int/*=>scala.Int#*/ => Int/*=>scala.Int#*/ = _ => ???/*=>scala.Predef.`???`().*/
  }

  trait Foo/*<=example.Anonymous#Foo#*/
  new Foo/*=>example.Anonymous#Foo#*/ /*=>java.lang.Object#`<init>`().*/{}
}
