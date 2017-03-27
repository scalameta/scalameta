package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._
import scala.meta.internal.ast._

class PlatformSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  test("JS floats are not JVM floats") {
    assert(Lit.Float(1.40f).show[Syntax] != "1.40f")
    assert(Lit.Float(1.4f).show[Syntax] != "1.4f")
    // workaround
    assert(Lit.Float("1.40").show[Syntax] == "1.40f")
    assert(Lit.Float("1.4").show[Syntax] == "1.4f")
  }
}
