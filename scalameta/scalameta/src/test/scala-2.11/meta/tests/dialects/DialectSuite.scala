package scala.meta.tests
package dialects

import org.scalatest.FunSuite
import scala.meta.Dialect

class DialectSuite extends FunSuite {
  test("Dialect.current") {
    assert(Dialect.current == scala.meta.dialects.Scala211)
  }
}