package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._

class PlatformSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  test("JS floats are not JVM floats") {
    assert(Lit.Float(1.40f).syntax != "1.40f")
    assert(Lit.Float(1.4f).syntax != "1.4f")
    // workaround
    assert(Lit.Float("1.40").syntax == "1.40f")
    assert(Lit.Float("1.4").syntax == "1.4f")
  }
}
