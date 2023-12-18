//see https://github.com/scalameta/metals/issues/5939
package example
import scala.language.implicitConversions

class AA(val f: Int => Int)

object AA {
  implicit def toF(a: AA): Int => Int = a.f
  val a = new AA(_ + 1)
  val result = a(1)
}
