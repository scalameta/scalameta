// See https://github.com/scalameta/scalameta/issues/1586
package example

class ExistentialsB/*<=example.ExistentialsB#*/[T/*<=example.ExistentialsB#[T]*/]
class ExistentialsC/*<=example.ExistentialsC#*/[CC/*<=example.ExistentialsC#[CC]*/[_/*<=example.ExistentialsC#[CC][_$$1]*/]] extends ExistentialsB/*=>example.ExistentialsB#*/[CC/*=>example.ExistentialsC#[CC]*/[_/*<=local0*/]]

class ExistentialsE/*<=example.ExistentialsE#*/ {
  def m/*<=example.ExistentialsE#m().*/: List/*=>scala.package.List#*/[List/*=>scala.package.List#*/[_/*<=local1*/]] = ???/*=>scala.Predef.`???`().*/
  def l/*<=example.ExistentialsE#l().*/: List/*=>scala.package.List#*/[_/*<=local2*/] = ???/*=>scala.Predef.`???`().*/
  val x/*<=example.ExistentialsE#x.*/: Map/*=>scala.Predef.Map#*/[_/*<=local3*/, _/*<=local4*/] = ???/*=>scala.Predef.`???`().*/
  def f/*<=example.ExistentialsE#f().*/: T/*=>local5*/ forSome { type T/*<=local5*/ } = ???/*=>scala.Predef.`???`().*/
  def h/*<=example.ExistentialsE#h().*/[F/*<=example.ExistentialsE#h().[F]*/[_/*<=example.ExistentialsE#h().[F][_$$1]*/]]: F/*=>example.ExistentialsE#h().[F]*/[_/*<=local6*/] = ???/*=>scala.Predef.`???`().*/
  type A/*<=example.ExistentialsE#A#*/ = List/*=>scala.package.List#*/[_/*<=local7*/]
}

object ExistentialsTest/*<=example.ExistentialsTest.*/ {
  val x/*<=example.ExistentialsTest.x.*/ = new ExistentialsE/*=>example.ExistentialsE#*/().m/*=>example.ExistentialsE#m().*/
  val y/*<=example.ExistentialsTest.y.*/: Map/*=>scala.Predef.Map#*/[_/*<=local8*/, _/*<=local9*/] = new ExistentialsE/*=>example.ExistentialsE#*/().x/*=>example.ExistentialsE#x.*/
}
