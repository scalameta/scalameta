package example

class SingleAbstractMethod/*<=example.SingleAbstractMethod#*/ {

  trait SAM/*<=example.SingleAbstractMethod#SAM#*/ {
    def foo/*<=example.SingleAbstractMethod#SAM#foo().*/(a/*<=example.SingleAbstractMethod#SAM#foo().(a)*/: Int/*=>scala.Int#*/): Int/*=>scala.Int#*/
  }
  def withSAM/*<=example.SingleAbstractMethod#withSAM().*/(a/*<=example.SingleAbstractMethod#withSAM().(a)*/: SAM/*=>example.SingleAbstractMethod#SAM#*/) = a/*=>example.SingleAbstractMethod#withSAM().(a)*/.foo/*=>example.SingleAbstractMethod#SAM#foo().*/(0)

  withSAM/*=>example.SingleAbstractMethod#withSAM().*/(_ +/*=>scala.Int#`+`(+4).*/ 1)
  withSAM/*=>example.SingleAbstractMethod#withSAM().*/((x/*<=local0*/: Int/*=>scala.Int#*/) => x/*=>local0*/ -/*=>scala.Int#`-`(+3).*/ 1)

  def funcSAM/*<=example.SingleAbstractMethod#funcSAM().*/(y/*<=example.SingleAbstractMethod#funcSAM().(y)*/: Int/*=>scala.Int#*/) = y/*=>example.SingleAbstractMethod#funcSAM().(y)*/ */*=>scala.Int#`*`(+3).*/ 2
  withSAM/*=>example.SingleAbstractMethod#withSAM().*/(funcSAM/*=>example.SingleAbstractMethod#funcSAM().*/)

}
