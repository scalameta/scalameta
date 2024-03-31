package scala.meta.tests
package dialects

import munit.FunSuite
import scala.meta.Dialect

class DialectSuite extends FunSuite {
  test("Dialect.current") { assertEquals(Dialect.current, scala.meta.dialects.Scala212) }
  test("internal mutation doesn't leak") {
    import scala.meta.dialects.Scala212
    val Scala212WithUnderscoreSeparator = Scala212.withAllowNumericLiteralUnderscoreSeparators(true)
    assert(!Scala212.allowNumericLiteralUnderscoreSeparators)
    assert(Scala212WithUnderscoreSeparator.allowNumericLiteralUnderscoreSeparators)
    assert(Scala212WithUnderscoreSeparator != Scala212)
  }
}
