package example
import scala.language/*=>scala.language.*/.higherKinds/*=>scala.language.higherKinds.*/

class Anonymous/*<=example.Anonymous#*/ {
  this: Anonymous/*=>example.Anonymous#*/ =>

  def m1/*<=example.Anonymous#m1().*/[T/*<=example.Anonymous#m1().[T]*/[_/*<=example.Anonymous#m1().[T][_$$1]*/], _/*<=example.Anonymous#m1().[_$$1]*/] = ???/*=>scala.Predef.`???`().*/
  def m2/*<=example.Anonymous#m2().*/: Map/*=>scala.Predef.Map#*/[_/*<=local0*/, List/*=>scala.package.List#*/[_/*<=local1*/]] = ???/*=>scala.Predef.`???`().*/
  locally/*=>scala.Predef.locally().*/ {
    ???/*=>scala.Predef.`???`().*/ match { case _: List/*=>scala.package.List#*/[_/*<=local2*/] => }
  }
  locally/*=>scala.Predef.locally().*/ {
    val x/*<=local4*/: Int/*=>scala.Int#*/ => Int/*=>scala.Int#*/ = _/*<=local3*/ => ???/*=>scala.Predef.`???`().*/
  }

  trait Foo/*<=example.Anonymous#Foo#*/
  new /*<=local5*/Foo/*=>example.Anonymous#Foo#*//*=>java.lang.Object#`<init>`().*/ {}
}
