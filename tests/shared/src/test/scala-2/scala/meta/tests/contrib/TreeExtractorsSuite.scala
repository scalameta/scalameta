package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

import munit.FunSuite

class TreeExtractorsSuite extends FunSuite {
  test("Select.unapply") {
    val Some((q"a", q"b")) = Select.unapply(q"a.b")
    val Some((q"a", t"b")) = Select.unapply(t"a.b")
  }
}
