package scala.meta.tests
package prettyprinters

import scala.meta._

class PlatformSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  test("JVM has proper floats") {
    // this gives different results work in a JS runtime
    assert(Lit.Float(1.40f).syntax == "1.4f") // trailing zero is lost
    assert(Lit.Float(1.4f).syntax == "1.4f")
    // cross-platform way to accomplish the same
    assert(Lit.Float("1.40").syntax == "1.40f")
    assert(Lit.Float("1.4").syntax == "1.4f")
  }
}
