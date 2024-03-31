package scala.meta.tests
package dialects

import munit.FunSuite
import scala.meta.Dialect

class DialectSuite extends FunSuite {
  test("Dialect.current")(assertEquals(Dialect.current, scala.meta.dialects.Scala211))
}
