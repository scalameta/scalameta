package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._
import scala.meta.internal.ast._

class PlatformSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  test("JVM has proper floats") {
    // this gives different results work in a JS runtime
    assert(Lit.Float(1.40f).show[Syntax] == "1.4f") // trailing zero is lost
    assert(Lit.Float(1.4f).show[Syntax] == "1.4f")
    // cross-platform way to accomplish the same
    assert(Lit.Float("1.40").show[Syntax] == "1.40f")
    assert(Lit.Float("1.4").show[Syntax] == "1.4f")
  }
}
