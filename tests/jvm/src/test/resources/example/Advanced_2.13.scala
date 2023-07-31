package advanced

import scala.language/*=>scala.language.*/.existentials/*=>scala.language.existentials.*/
import scala.language/*=>scala.language.*/.higherKinds/*=>scala.language.higherKinds.*/
import scala.language/*=>scala.language.*/.reflectiveCalls/*=>scala.language.reflectiveCalls.*/

class C/*<=advanced.C#*/[T/*<=advanced.C#[T]*/] {
  def t/*<=advanced.C#t().*/: T/*=>advanced.C#[T]*/ = ???/*=>scala.Predef.`???`().*/
}

class Structural/*<=advanced.Structural#*/ {
  def s1/*<=advanced.Structural#s1().*/: { val x/*<=local0*/: Int/*=>scala.Int#*/ } = ???/*=>scala.Predef.`???`().*/
  def s2/*<=advanced.Structural#s2().*/ = new /*<=local2*/{ val x/*<=local1*/: Int/*=>scala.Int#*/ = ???/*=>scala.Predef.`???`().*/ }
  def s3/*<=advanced.Structural#s3().*/ = new /*<=local5*/{ def m/*<=local3*/(x/*<=local4*/: Int/*=>scala.Int#*/): Int/*=>scala.Int#*/ = ???/*=>scala.Predef.`???`().*/ }
}

class Existential/*<=advanced.Existential#*/ {
  def e1/*<=advanced.Existential#e1().*/: List/*=>scala.package.List#*/[_] = ???/*=>scala.Predef.`???`().*/
  def e2/*<=advanced.Existential#e2().*/: C/*=>advanced.C#*/[List/*=>scala.package.List#*/[T/*=>local7*/] forSome { type T/*<=local7*/ }] = ???/*=>scala.Predef.`???`().*/
  def e4/*<=advanced.Existential#e4().*/: U/*=>local8*/[Int/*=>scala.Int#*/] forSome { type U/*<=local8*/[T/*<=local9*/ <: Int] } = ???/*=>scala.Predef.`???`().*/
}

class D/*<=advanced.D#*/[CC/*<=advanced.D#[CC]*/[_]] extends C/*=>advanced.C#*/[CC/*=>advanced.D#[CC]*/[_]]

object Test/*<=advanced.Test.*/ {
  val s/*<=advanced.Test.s.*/ = new Structural/*=>advanced.Structural#*/
  val s1/*<=advanced.Test.s1.*/ = s/*=>advanced.Test.s.*/.s1/*=>advanced.Structural#s1().*/
  val s1x/*<=advanced.Test.s1x.*/ = s/*=>advanced.Test.s.*/.s1/*=>advanced.Structural#s1().*/.x/*=>local0*/
  val s2/*<=advanced.Test.s2.*/ = s/*=>advanced.Test.s.*/.s2/*=>advanced.Structural#s2().*/
  val s2x/*<=advanced.Test.s2x.*/ = s/*=>advanced.Test.s.*/.s2/*=>advanced.Structural#s2().*/.x/*=>local1*/
  val s3/*<=advanced.Test.s3.*/ = s/*=>advanced.Test.s.*/.s3/*=>advanced.Structural#s3().*/
  val s3x/*<=advanced.Test.s3x.*/ = s/*=>advanced.Test.s.*/.s3/*=>advanced.Structural#s3().*/.m/*=>local3*/(???/*=>scala.Predef.`???`().*/)

  val e/*<=advanced.Test.e.*/ = new Existential/*=>advanced.Existential#*/
  val e1/*<=advanced.Test.e1.*/ = e/*=>advanced.Test.e.*/.e1/*=>advanced.Existential#e1().*/
  val e1x/*<=advanced.Test.e1x.*/ = e/*=>advanced.Test.e.*/.e1/*=>advanced.Existential#e1().*/.head/*=>scala.collection.IterableOps#head().*/
  val e2/*<=advanced.Test.e2.*/ = e/*=>advanced.Test.e.*/.e2/*=>advanced.Existential#e2().*/
  val e2x/*<=advanced.Test.e2x.*/ = e/*=>advanced.Test.e.*/.e2/*=>advanced.Existential#e2().*/.t/*=>advanced.C#t().*/
  val e2xx/*<=advanced.Test.e2xx.*/ = e/*=>advanced.Test.e.*/.e2/*=>advanced.Existential#e2().*/.t/*=>advanced.C#t().*/.head/*=>scala.collection.IterableOps#head().*/
  locally/*=>scala.Predef.locally().*/ {
    (???/*=>scala.Predef.`???`().*/ : Any/*=>scala.Any#*/) match {
      case e3/*<=local11*/: List/*=>scala.package.List#*/[_] =>
        val e3x/*<=local13*/ = e3/*=>local11*/.head/*=>scala.collection.IterableOps#head().*/
        ()
    }
  }
}
