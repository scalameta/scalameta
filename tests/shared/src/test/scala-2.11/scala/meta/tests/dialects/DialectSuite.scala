package scala.meta.tests
package dialects

import scala.meta.Dialect

import munit.FunSuite

class DialectSuite extends FunSuite {
  test("Dialect.current")(assertEquals(Dialect.current, scala.meta.dialects.Scala211))
}
