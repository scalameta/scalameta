//see https://github.com/scalameta/metals/issues/5939
package example
import scala.language/*=>scala.language.*/.implicitConversions/*=>scala.language.implicitConversions.*/

class AA/*<=example.AA#*/(val f/*<=example.AA#f.*/: Int/*=>scala.Int#*/ => Int/*=>scala.Int#*/)

object AA/*<=example.AA.*/ {
  implicit def toF/*<=example.AA.toF().*/(a/*<=example.AA.toF().(a)*/: AA/*=>example.AA#*/): Int/*=>scala.Int#*/ => Int/*=>scala.Int#*/ = a/*=>example.AA.toF().(a)*/.f/*=>example.AA#f.*/
  val a/*<=example.AA.a.*/ = new AA/*=>example.AA#*/(_ +/*=>scala.Int#`+`(+4).*/ 1)
  val result/*<=example.AA.result.*/ = a/*=>example.AA.a.*/(1)
}
