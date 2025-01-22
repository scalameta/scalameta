package scala.meta.tests
package dialects

import scala.meta._

import munit.FunSuite

class DialectSuite extends FunSuite {
  test("Dialect.current")(assertEquals(Dialect.current, dialects.Scala212))
  test("internal mutation doesn't leak") {
    import dialects.Scala212
    val Scala212WithUnderscoreSeparator = Scala212.withAllowNumericLiteralUnderscoreSeparators(true)
    assert(!Scala212.allowNumericLiteralUnderscoreSeparators)
    assert(Scala212WithUnderscoreSeparator.allowNumericLiteralUnderscoreSeparators)
    assert(Scala212WithUnderscoreSeparator != Scala212)
  }
}
