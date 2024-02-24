package scala.meta.tests
package prettyprinters

import scala.meta._

class PlatformSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  test("JVM has proper floats") {
    // this gives different results work in a JS runtime
    assertSyntax(lit(1.40f))(flt("1.4f")) // trailing zero is lost
    assertSyntax(lit(1.4f))(flt("1.4f"))
    // cross-platform way to accomplish the same
    assertSyntax(flt("1.40"))(flt("1.40f"))
    assertSyntax(flt("1.4"))(flt("1.4f"))
  }
}
