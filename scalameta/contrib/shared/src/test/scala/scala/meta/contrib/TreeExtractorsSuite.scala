package scala.meta
package contrib

import org.scalatest.FunSuite

class TreeExtractorsSuite extends FunSuite {
  test("Select.unapply") {
    val Some((q"a", q"b")) = Select.unapply(q"a.b")
    val Some((q"a", t"b")) = Select.unapply(t"a.b")
  }
}
