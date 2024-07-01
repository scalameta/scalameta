package classes

class C1/*<=classes.C1#*/(val x1/*<=classes.C1#x1.*/: Int/*=>scala.Int#*/) extends AnyVal/*=>scala.AnyVal#*//*=>scala.AnyVal#`<init>`().*/

class C2/*<=classes.C2#*/(val x2/*<=classes.C2#x2.*/: Int/*=>scala.Int#*/) extends AnyVal/*=>scala.AnyVal#*//*=>scala.AnyVal#`<init>`().*/
object C2/*<=classes.C2.*/

case class C3/*<=classes.C3#*/(x/*<=classes.C3#x.*/: Int/*=>scala.Int#*/)

case class C4/*<=classes.C4#*/(x/*<=classes.C4#x.*/: Int/*=>scala.Int#*/)
object C4/*<=classes.C4.*/

object M/*<=classes.M.*/ {
  implicit class C5/*<=classes.M.C5#*/(x/*<=classes.M.C5#x.*/: Int/*=>scala.Int#*/)
}

case class C6/*<=classes.C6#*/(private val x/*<=classes.C6#x.*/: Int/*=>scala.Int#*/)

class C7/*<=classes.C7#*/(x/*<=classes.C7#x.*/: Int/*=>scala.Int#*/)

class C8/*<=classes.C8#*/(private[this] val x/*<=classes.C8#x.*/: Int/*=>scala.Int#*/)

class C9/*<=classes.C9#*/(private[this] var x/*<=classes.C9#x().*/: Int/*=>scala.Int#*/)

object N/*<=classes.N.*/ {
  val anonClass/*<=classes.N.anonClass.*/ = new C7/*=>classes.C7#*/(42) {
    val local/*<=local9*/ = ???/*=>scala.Predef.`???`().*/
  }
  val anonFun/*<=classes.N.anonFun.*/ = List/*=>scala.package.List.*/(1).map/*=>scala.collection.immutable.List#map().*/ { i/*<=local10*/ =>
    val local/*<=local11*/ = 2
    local/*=>local11*/ +/*=>scala.Int#`+`(+4).*/ 2
  }
}

object Chain/*<=classes.Chain.*/ {
  class A/*<=classes.Chain.A#*/ {
    def tst1/*<=classes.Chain.A#tst1().*/(i/*<=classes.Chain.A#tst1().(i)*/: Int/*=>scala.Int#*/): A/*=>classes.Chain.A#*/ = this
    def tst2/*<=classes.Chain.A#tst2().*/(i/*<=classes.Chain.A#tst2().(i)*/: Int/*=>scala.Int#*/): A/*=>classes.Chain.A#*/ = this
    def tst3/*<=classes.Chain.A#tst3().*/(i/*<=classes.Chain.A#tst3().(i)*/: Int/*=>scala.Int#*/, j/*<=classes.Chain.A#tst3().(j)*/: Int/*=>scala.Int#*/): A/*=>classes.Chain.A#*/ = this
    def tst4/*<=classes.Chain.A#tst4().*/(i/*<=classes.Chain.A#tst4().(i)*/: Int/*=>scala.Int#*/, j/*<=classes.Chain.A#tst4().(j)*/: A/*=>classes.Chain.A#*/): A/*=>classes.Chain.A#*/ = j/*=>classes.Chain.A#tst4().(j)*/
  }

  def tst/*<=classes.Chain.tst().*/(i/*<=classes.Chain.tst().(i)*/: Int/*=>scala.Int#*/, j/*<=classes.Chain.tst().(j)*/: A/*=>classes.Chain.A#*/ = new A/*=>classes.Chain.A#*/()) = j/*=>classes.Chain.tst().(j)*/

  val a/*<=classes.Chain.a.*/ = new A/*=>classes.Chain.A#*/
  a/*=>classes.Chain.a.*/.tst1/*=>classes.Chain.A#tst1().*/(i/*=>classes.Chain.A#tst1().(i)*/ = 1)
    .tst2/*=>classes.Chain.A#tst2().*/(i/*=>classes.Chain.A#tst2().(i)*/ = 2)

  a/*=>classes.Chain.a.*/.tst2/*=>classes.Chain.A#tst2().*/(i/*=>classes.Chain.A#tst2().(i)*/ = 1)
    .tst3/*=>classes.Chain.A#tst3().*/(i/*=>classes.Chain.A#tst3().(i)*/ = 1, 2)
    .tst1/*=>classes.Chain.A#tst1().*/(i/*=>classes.Chain.A#tst1().(i)*/ = 2)

  a/*=>classes.Chain.a.*/.tst2/*=>classes.Chain.A#tst2().*/(i/*=>classes.Chain.A#tst2().(i)*/ = 1)
    .tst3/*=>classes.Chain.A#tst3().*/(i/*=>classes.Chain.A#tst3().(i)*/ = 1, j/*=>classes.Chain.A#tst3().(j)*/ = 2)
    .tst1/*=>classes.Chain.A#tst1().*/(i/*=>classes.Chain.A#tst1().(i)*/ = 2)

  a/*=>classes.Chain.a.*/.tst2/*=>classes.Chain.A#tst2().*/(i/*=>classes.Chain.A#tst2().(i)*/ = 1)
    .tst4/*=>classes.Chain.A#tst4().*/(i/*=>classes.Chain.A#tst4().(i)*/ = 1, j/*=>classes.Chain.A#tst4().(j)*/ = tst/*=>classes.Chain.tst().*/(i/*=>classes.Chain.tst().(i)*/ = 1, j/*=>classes.Chain.tst().(j)*/ = new A/*=>classes.Chain.A#*/()))
    .tst1/*=>classes.Chain.A#tst1().*/(i/*=>classes.Chain.A#tst1().(i)*/ = 2)
}
